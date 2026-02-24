{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.Trie.PersistentSpec
-- Description : Integration tests for RocksDB-backed trie
-- License     : Apache-2.0
--
-- Runs the parameterized 'TrieSpec' and
-- 'TrieManagerSpec' suites against the persistent
-- RocksDB backend. Also includes property-based
-- tests comparing persistent and pure backends,
-- and persistence-specific tests verifying data
-- survives DB close/reopen cycles.
module Cardano.MPFS.Trie.PersistentSpec
    ( -- * Test suite
      spec

      -- * Test utilities
    , withTestDB
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Short qualified as SBS
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    )
import Data.List (nubBy)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    , ioProperty
    , listOf1
    , property
    , shuffle
    , vectorOf
    , (===)
    , (==>)
    )

import Cardano.Ledger.Mary.Value (AssetName (..))
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction
    ( RunTransaction
    , newRunTransaction
    )
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , DB (..)
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )

import MPF.Hashes
    ( mkMPFHash
    , renderMPFHash
    )
import MPF.Test.Lib
    ( encodeHex
    , expectedFullTrieRoot
    , fruitsTestData
    , getRootHashM
    , insertByteStringM
    , runMPFPure'
    )

import Cardano.MPFS.Application
    ( cageColumnFamilies
    , dbConfig
    )
import Cardano.MPFS.Indexer.Codecs (allCodecs)
import Cardano.MPFS.Indexer.Columns (AllColumns)
import Cardano.MPFS.Trie
    ( Trie (..)
    , TrieManager (..)
    )
import Cardano.MPFS.Trie.Persistent
    ( mkPersistentTrieManager
    )
import Cardano.MPFS.TrieManagerSpec qualified as TrieManagerSpec
import Cardano.MPFS.TrieSpec qualified as TrieSpec
import Cardano.MPFS.Types
    ( Root (..)
    , TokenId (..)
    )

import Database.RocksDB (withDBCF)

-- ---------------------------------------------------------
-- RocksDB config & helpers
-- ---------------------------------------------------------

-- | Shorthand for the concrete RunTransaction type
-- used in tests.
type RT =
    RunTransaction
        IO
        ColumnFamily
        AllColumns
        BatchOp

-- | Run an action with a temporary RocksDB that has
-- all cage column families and a 'RunTransaction'.
withTestDB
    :: ( DB
         -> ColumnFamily
         -> ColumnFamily
         -> RT
         -> IO a
       )
    -> IO a
withTestDB action =
    withSystemTempDirectory "mpfs-test" $ \dir ->
        withTestDBAt dir action

-- | Open a RocksDB at a specific path with the
-- standard column families and 'RunTransaction'.
-- Used for reopen tests where the same directory
-- is opened multiple times.
withTestDBAt
    :: FilePath
    -> ( DB
         -> ColumnFamily
         -> ColumnFamily
         -> RT
         -> IO a
       )
    -> IO a
withTestDBAt dir action =
    withDBCF dir dbConfig cageColumnFamilies
        $ \db -> do
            let columns =
                    mkColumns
                        (columnFamilies db)
                        allCodecs
                database =
                    mkRocksDBDatabase db columns
            rt <- newRunTransaction database
            -- trie-nodes is 4th, trie-kv is 5th
            -- (0-indexed: 3, 4)
            case drop 3 (columnFamilies db) of
                (nodesCF : kvCF : _) ->
                    action db nodesCF kvCF rt
                _ ->
                    error
                        "Expected at least 6 \
                        \column families"

-- ---------------------------------------------------------
-- Generators
-- ---------------------------------------------------------

-- | Generate a random ByteString key.
genKeyBytes :: Gen ByteString
genKeyBytes =
    B.pack <$> listOf1 (choose (0, 255))

-- | Generate a random ByteString value.
genValue :: Gen ByteString
genValue =
    B.pack <$> listOf1 (choose (0, 255))

-- | Hash key bytes (Aiken convention).
hashKey :: ByteString -> ByteString
hashKey = renderMPFHash . mkMPFHash

-- | Generate unique key-value pairs (unique by
-- hashed key).
genUniqueKVs :: Gen [(ByteString, ByteString)]
genUniqueKVs = do
    kvs <-
        listOf1
            ((,) <$> genKeyBytes <*> genValue)
    pure
        $ nubBy
            ( \(k1, _) (k2, _) ->
                hashKey k1 == hashKey k2
            )
            kvs

-- ---------------------------------------------------------
-- Token ID helpers
-- ---------------------------------------------------------

-- | Generate a unique 'TokenId' from a counter.
nextTokenId :: IORef Int -> IO TokenId
nextTokenId ref =
    atomicModifyIORef' ref $ \n ->
        ( n + 1
        , TokenId
            $ AssetName
            $ SBS.pack
            $ encodeInt n
        )

-- | Encode an 'Int' as bytes.
encodeInt :: Int -> [Word8]
encodeInt n =
    [ fromIntegral (n `div` 256)
    , fromIntegral (n `mod` 256)
    ]

-- | Fixed token IDs for reopen tests (safe because
-- each reopen test uses its own temp directory).
reopenTidA :: TokenId
reopenTidA =
    TokenId (AssetName (SBS.pack [42, 1]))

reopenTidB :: TokenId
reopenTidB =
    TokenId (AssetName (SBS.pack [42, 2]))

-- ---------------------------------------------------------
-- Fresh persistent trie construction
-- ---------------------------------------------------------

-- | Create a fresh persistent 'Trie IO' for a test.
-- Uses the counter to generate a unique 'TokenId',
-- ensuring isolation across test iterations.
newPersistentTrie
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> IO (Trie IO)
newPersistentTrie db nodesCF kvCF rt counterRef =
    do
        let tm =
                mkPersistentTrieManager
                    db
                    nodesCF
                    kvCF
                    rt
        tid <- nextTokenId counterRef
        createTrie tm tid
        withTrie tm tid pure

-- ---------------------------------------------------------
-- Top-level spec
-- ---------------------------------------------------------

-- | Run all persistent trie tests.
spec
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Spec
spec db nodesCF kvCF rt counterRef = do
    describe "Persistent Trie"
        $ TrieSpec.spec
            ( newPersistentTrie
                db
                nodesCF
                kvCF
                rt
                counterRef
            )
    describe "Persistent TrieManager"
        $ TrieManagerSpec.spec
            -- Clean up test tokens before each test
            -- to avoid stale registry entries from
            -- previous tests in the shared DB.
            ( do
                let tm =
                        mkPersistentTrieManager
                            db
                            nodesCF
                            kvCF
                            rt
                deleteTrie tm TrieManagerSpec.tokenA
                deleteTrie tm TrieManagerSpec.tokenB
                pure tm
            )
    describe "Persistent properties"
        $ propertySpec
            db
            nodesCF
            kvCF
            rt
            counterRef
    describe "Persistence-specific"
        $ persistenceSpec
            db
            nodesCF
            kvCF
            rt
            counterRef

-- ---------------------------------------------------------
-- Property-based tests
-- ---------------------------------------------------------

propertySpec
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Spec
propertySpec db nodesCF kvCF rt counterRef = do
    it "same root as pure backend"
        $ property
        $ propPureEquivalentRoot
            db
            nodesCF
            kvCF
            rt
            counterRef

    it "insertion order independence"
        $ property
        $ propInsertOrderPersistent
            db
            nodesCF
            kvCF
            rt
            counterRef

    it "deleted key not verifiable"
        $ property
        $ propDeleteRemovesPersistent
            db
            nodesCF
            kvCF
            rt
            counterRef

    it "deletion preserves siblings"
        $ property
        $ propDeletePreservesSiblingsPersistent
            db
            nodesCF
            kvCF
            rt
            counterRef

    it "per-token isolation"
        $ property
        $ propTokenIsolation
            db
            nodesCF
            kvCF
            rt
            counterRef

    it "createTrie overwrites existing"
        $ property
        $ propCreateOverwrites
            db
            nodesCF
            kvCF
            rt
            counterRef

    it "delete then re-insert restores root"
        $ property
        $ propDeleteInsertRoundtrip
            db
            nodesCF
            kvCF
            rt
            counterRef

-- | Same operations produce same root on pure and
-- persistent backends.
propPureEquivalentRoot
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Property
propPureEquivalentRoot
    db
    nodesCF
    kvCF
    rt
    counterRef =
        forAll genUniqueKVs $ \kvs ->
            not (null kvs) ==>
                ioProperty $ do
                    let (mRoot, _) = runMPFPure' $ do
                            forM_ kvs
                                $ uncurry
                                    insertByteStringM
                            getRootHashM
                        pureRoot =
                            maybe
                                B.empty
                                renderMPFHash
                                mRoot
                    trie <-
                        newPersistentTrie
                            db
                            nodesCF
                            kvCF
                            rt
                            counterRef
                    forM_ kvs
                        $ uncurry (insert trie)
                    Root persistRoot <- getRoot trie
                    pure (pureRoot === persistRoot)

-- | Insertion order doesn't affect root hash on
-- the persistent backend.
propInsertOrderPersistent
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Property
propInsertOrderPersistent
    db
    nodesCF
    kvCF
    rt
    counterRef =
        forAll genUniqueKVs $ \kvs ->
            length kvs >= 2 ==>
                forAll (shuffle kvs) $ \shuffled ->
                    ioProperty $ do
                        trie1 <-
                            newPersistentTrie
                                db
                                nodesCF
                                kvCF
                                rt
                                counterRef
                        forM_ kvs
                            $ uncurry (insert trie1)
                        root1 <- getRoot trie1

                        trie2 <-
                            newPersistentTrie
                                db
                                nodesCF
                                kvCF
                                rt
                                counterRef
                        forM_ shuffled
                            $ uncurry (insert trie2)
                        root2 <- getRoot trie2
                        pure (root1 === root2)

-- | Deleted key cannot be looked up on a
-- non-empty persistent trie.
propDeleteRemovesPersistent
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Property
propDeleteRemovesPersistent
    db
    nodesCF
    kvCF
    rt
    counterRef =
        forAll genUniqueKVs $ \bg ->
            not (null bg) ==>
                forAll genKeyBytes $ \keyBs ->
                    forAll genValue $ \valBs ->
                        let hk = hashKey keyBs
                            noCollision =
                                all
                                    ( (/= hk)
                                        . hashKey
                                        . fst
                                    )
                                    bg
                        in  noCollision ==>
                                ioProperty $ do
                                    trie <-
                                        newPersistentTrie
                                            db
                                            nodesCF
                                            kvCF
                                            rt
                                            counterRef
                                    forM_ bg
                                        $ uncurry
                                            (insert trie)
                                    _ <-
                                        insert
                                            trie
                                            keyBs
                                            valBs
                                    _ <-
                                        Cardano.MPFS.Trie.delete
                                            trie
                                            keyBs
                                    mVal <-
                                        Cardano.MPFS.Trie.lookup
                                            trie
                                            keyBs
                                    pure
                                        (mVal === Nothing)

-- | Deleting one key preserves siblings on a
-- non-empty persistent trie.
propDeletePreservesSiblingsPersistent
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Property
propDeletePreservesSiblingsPersistent
    db
    nodesCF
    kvCF
    rt
    counterRef =
        forAll genUniqueKVs $ \bg ->
            not (null bg) ==>
                forAll
                    ( vectorOf
                        3
                        ( (,)
                            <$> genKeyBytes
                            <*> genValue
                        )
                    )
                    $ \rawKvs ->
                        let kvs =
                                nubBy
                                    ( \(k1, _) (k2, _) ->
                                        hashKey k1
                                            == hashKey k2
                                    )
                                    rawKvs
                            allHashes =
                                map
                                    (hashKey . fst)
                                    bg
                            noCollision =
                                all
                                    ( (`notElem` allHashes)
                                        . hashKey
                                        . fst
                                    )
                                    kvs
                        in  length kvs == 3
                                && noCollision
                                ==> let ( (keepK, _)
                                            , (delK, _)
                                            ) =
                                                case kvs of
                                                    (a : b : _) ->
                                                        (a, b)
                                                    _ ->
                                                        error
                                                            "impossible"
                                    in  ioProperty $ do
                                            trie <-
                                                newPersistentTrie
                                                    db
                                                    nodesCF
                                                    kvCF
                                                    rt
                                                    counterRef
                                            forM_ bg
                                                $ uncurry
                                                    (insert trie)
                                            forM_ kvs
                                                $ uncurry
                                                    (insert trie)
                                            _ <-
                                                Cardano.MPFS.Trie.delete
                                                    trie
                                                    delK
                                            mVal <-
                                                Cardano.MPFS.Trie.lookup
                                                    trie
                                                    keepK
                                            pure
                                                (isJust mVal)

-- | Random operations on token A don't affect
-- token B's root.
propTokenIsolation
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Property
propTokenIsolation
    db
    nodesCF
    kvCF
    rt
    counterRef =
        forAll genUniqueKVs $ \kvs ->
            not (null kvs) ==>
                ioProperty $ do
                    let tm =
                            mkPersistentTrieManager
                                db
                                nodesCF
                                kvCF
                                rt
                    tidA <- nextTokenId counterRef
                    tidB <- nextTokenId counterRef
                    createTrie tm tidA
                    createTrie tm tidB
                    withTrie tm tidA $ \trie ->
                        forM_ kvs
                            $ uncurry (insert trie)
                    withTrie tm tidB $ \trie -> do
                        Root root <- getRoot trie
                        pure (root === B.empty)

-- | Creating a token that already has data resets
-- its root to empty.
propCreateOverwrites
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Property
propCreateOverwrites
    db
    nodesCF
    kvCF
    rt
    counterRef =
        forAll genUniqueKVs $ \kvs ->
            not (null kvs) ==>
                ioProperty $ do
                    let tm =
                            mkPersistentTrieManager
                                db
                                nodesCF
                                kvCF
                                rt
                    tid <- nextTokenId counterRef
                    createTrie tm tid
                    withTrie tm tid $ \trie ->
                        forM_ kvs
                            $ uncurry (insert trie)
                    createTrie tm tid
                    withTrie tm tid $ \trie -> do
                        Root root <- getRoot trie
                        pure (root === B.empty)

-- | Delete then re-insert restores root on a
-- non-empty persistent trie.
propDeleteInsertRoundtrip
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Property
propDeleteInsertRoundtrip
    db
    nodesCF
    kvCF
    rt
    counterRef =
        forAll genUniqueKVs $ \bg ->
            not (null bg) ==>
                forAll genKeyBytes $ \keyBs ->
                    forAll genValue $ \valBs ->
                        let hk = hashKey keyBs
                            noCollision =
                                all
                                    ( (/= hk)
                                        . hashKey
                                        . fst
                                    )
                                    bg
                        in  noCollision ==>
                                ioProperty $ do
                                    trie <-
                                        newPersistentTrie
                                            db
                                            nodesCF
                                            kvCF
                                            rt
                                            counterRef
                                    forM_ bg
                                        $ uncurry
                                            (insert trie)
                                    root1 <-
                                        insert
                                            trie
                                            keyBs
                                            valBs
                                    _ <-
                                        Cardano.MPFS.Trie.delete
                                            trie
                                            keyBs
                                    root2 <-
                                        insert
                                            trie
                                            keyBs
                                            valBs
                                    pure
                                        (root1 === root2)

-- ---------------------------------------------------------
-- Persistence-specific unit tests
-- ---------------------------------------------------------

persistenceSpec
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> RT
    -> IORef Int
    -> Spec
persistenceSpec db nodesCF kvCF rt counterRef = do
    it
        "data persists across DB reopen"
        persistsAcrossReopen

    it
        "deleted trie stays deleted after reopen"
        deletedTrieStaysDeletedAfterReopen

    it
        "multiple tokens survive reopen"
        multipleToksSurviveReopen

    it "fruit test vectors on persistent" $ do
        trie <-
            newPersistentTrie
                db
                nodesCF
                kvCF
                rt
                counterRef
        forM_ fruitsTestData
            $ uncurry (insert trie)
        root <- getRoot trie
        encodeHex (unRoot root)
            `shouldBe` encodeHex
                expectedFullTrieRoot

-- | Insert data, close DB, reopen, verify data is
-- still present. The persistent registry means
-- 'withTrie' works without calling 'createTrie'
-- after reopen.
persistsAcrossReopen :: IO ()
persistsAcrossReopen =
    withSystemTempDirectory "reopen" $ \dir -> do
        -- Phase 1: insert data
        root1 <-
            withTestDBAt dir
                $ \db nodesCF kvCF rt -> do
                    let mgr =
                            mkPersistentTrieManager
                                db
                                nodesCF
                                kvCF
                                rt
                    createTrie mgr reopenTidA
                    withTrie mgr reopenTidA
                        $ \trie -> do
                            _ <-
                                insert
                                    trie
                                    "hello"
                                    "world"
                            getRoot trie
        -- Phase 2: reopen and verify — no
        -- createTrie needed, registry persisted
        withTestDBAt dir $ \db nodesCF kvCF rt ->
            do
                let mgr =
                        mkPersistentTrieManager
                            db
                            nodesCF
                            kvCF
                            rt
                withTrie mgr reopenTidA $ \trie ->
                    do
                        root2 <- getRoot trie
                        root2 `shouldBe` root1

-- | Delete trie, close DB, reopen, verify the
-- token is no longer registered.
deletedTrieStaysDeletedAfterReopen :: IO ()
deletedTrieStaysDeletedAfterReopen =
    withSystemTempDirectory "reopen-del" $ \dir ->
        do
            -- Phase 1: insert then delete
            withTestDBAt dir
                $ \db nodesCF kvCF rt -> do
                    let mgr =
                            mkPersistentTrieManager
                                db
                                nodesCF
                                kvCF
                                rt
                    createTrie mgr reopenTidA
                    withTrie mgr reopenTidA
                        $ \trie ->
                            void
                                $ insert
                                    trie
                                    "hello"
                                    "world"
                    deleteTrie mgr reopenTidA
            -- Phase 2: reopen — token should be
            -- gone from registry
            withTestDBAt dir
                $ \db nodesCF kvCF rt -> do
                    let mgr =
                            mkPersistentTrieManager
                                db
                                nodesCF
                                kvCF
                                rt
                    -- withTrie should throw
                    result <-
                        try
                            $ withTrie
                                mgr
                                reopenTidA
                            $ \_ -> pure ()
                    case ( result
                            :: Either
                                SomeException
                                ()
                         ) of
                        Left _ -> pure ()
                        Right _ ->
                            error
                                "Expected \
                                \exception for \
                                \deleted trie"

-- | Two tokens with data, close + reopen, both
-- intact. No createTrie needed after reopen.
multipleToksSurviveReopen :: IO ()
multipleToksSurviveReopen =
    withSystemTempDirectory "reopen-multi"
        $ \dir -> do
            -- Phase 1: insert into both tokens
            (rootA1, rootB1) <-
                withTestDBAt dir
                    $ \db nodesCF kvCF rt -> do
                        let mgr =
                                mkPersistentTrieManager
                                    db
                                    nodesCF
                                    kvCF
                                    rt
                        createTrie mgr reopenTidA
                        createTrie mgr reopenTidB
                        rA <-
                            withTrie
                                mgr
                                reopenTidA
                                $ \trie -> do
                                    _ <-
                                        insert
                                            trie
                                            "key-a"
                                            "val-a"
                                    getRoot trie
                        rB <-
                            withTrie
                                mgr
                                reopenTidB
                                $ \trie -> do
                                    _ <-
                                        insert
                                            trie
                                            "key-b"
                                            "val-b"
                                    getRoot trie
                        pure (rA, rB)
            -- Phase 2: reopen and verify both
            withTestDBAt dir
                $ \db nodesCF kvCF rt -> do
                    let mgr =
                            mkPersistentTrieManager
                                db
                                nodesCF
                                kvCF
                                rt
                    withTrie mgr reopenTidA
                        $ \trie -> do
                            rootA2 <- getRoot trie
                            rootA2 `shouldBe` rootA1
                    withTrie mgr reopenTidB
                        $ \trie -> do
                            rootB2 <- getRoot trie
                            rootB2 `shouldBe` rootB1
                    -- Verify isolation still holds
                    rootA <-
                        withTrie mgr reopenTidA
                            $ \trie -> getRoot trie
                    rootB <-
                        withTrie mgr reopenTidB
                            $ \trie -> getRoot trie
                    rootA
                        `shouldSatisfy` (/= rootB)
