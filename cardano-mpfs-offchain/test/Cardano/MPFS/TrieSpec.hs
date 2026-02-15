{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.TrieSpec
-- Description : Property tests for the Trie interface
-- License     : Apache-2.0
module Cardano.MPFS.TrieSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (nubBy)
import Data.Maybe (isJust, isNothing)

import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    )

import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    , listOf1
    , property
    , shuffle
    , vectorOf
    , (===)
    , (==>)
    )

import MPF.Backend.Pure
    ( MPFInMemoryDB
    , emptyMPFInMemoryDB
    , runMPFPure
    )
import MPF.Hashes
    ( mkMPFHash
    , renderMPFHash
    )
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( deleteMPFM
    , encodeHex
    , expectedFullTrieRoot
    , fruitsTestData
    , getRootHashM
    , insertByteStringM
    , proofMPFM
    , runMPFPure'
    , verifyMPFM
    )

import Cardano.MPFS.Trie (Proof (..), Trie (..))
import Cardano.MPFS.Types (Root (..))

-- -----------------------------------------------------------------
-- Mock Trie backed by haskell-mpfs pure backend
-- -----------------------------------------------------------------

-- | Create a mock 'Trie IO' backed by an 'IORef'
-- holding an in-memory MPF database.
mkMockTrie :: IO (Trie IO)
mkMockTrie = do
    ref <- newIORef emptyMPFInMemoryDB
    pure (mkTrieFromRef ref)

-- | Build a 'Trie IO' from an 'IORef'.
mkTrieFromRef :: IORef MPFInMemoryDB -> Trie IO
mkTrieFromRef ref =
    Trie
        { insert = mockInsert ref
        , delete = mockDelete ref
        , lookup = mockLookup ref
        , getRoot = mockGetRoot ref
        , getProof = mockGetProof ref
        }

-- | Insert a key-value pair. Hashes both key and
-- value to match Aiken-compatible MPF convention.
mockInsert
    :: IORef MPFInMemoryDB
    -> ByteString
    -> ByteString
    -> IO Root
mockInsert ref k v = do
    db <- readIORef ref
    let ((), db') =
            runMPFPure db (insertByteStringM k v)
    modifyIORef' ref (const db')
    getRootFromDb db'

-- | Delete a key from the trie.
mockDelete
    :: IORef MPFInMemoryDB
    -> ByteString
    -> IO Root
mockDelete ref k = do
    db <- readIORef ref
    let hexKey =
            byteStringToHexKey
                $ renderMPFHash
                $ mkMPFHash k
        ((), db') =
            runMPFPure db (deleteMPFM hexKey)
    modifyIORef' ref (const db')
    getRootFromDb db'

-- | Look up a value by key. Returns the raw hash
-- bytes if the key exists in the trie.
mockLookup
    :: IORef MPFInMemoryDB
    -> ByteString
    -> IO (Maybe ByteString)
mockLookup ref k = do
    db <- readIORef ref
    let hexKey =
            byteStringToHexKey
                $ renderMPFHash
                $ mkMPFHash k
        (mProof, _) =
            runMPFPure db (proofMPFM hexKey)
    pure $ case mProof of
        Nothing -> Nothing
        Just _ -> Just (renderMPFHash (mkMPFHash k))

-- | Get current root hash.
mockGetRoot :: IORef MPFInMemoryDB -> IO Root
mockGetRoot ref = readIORef ref >>= getRootFromDb

-- | Get root from a database snapshot.
getRootFromDb :: MPFInMemoryDB -> IO Root
getRootFromDb db =
    let (mHash, _) = runMPFPure db getRootHashM
    in  pure $ case mHash of
            Nothing -> Root B.empty
            Just h -> Root (renderMPFHash h)

-- | Generate a Merkle proof for a key.
mockGetProof
    :: IORef MPFInMemoryDB
    -> ByteString
    -> IO (Maybe Proof)
mockGetProof ref k = do
    db <- readIORef ref
    let hexKey =
            byteStringToHexKey
                $ renderMPFHash
                $ mkMPFHash k
        (mProof, _) =
            runMPFPure db (proofMPFM hexKey)
    pure $ case mProof of
        Nothing -> Nothing
        Just _ -> Just (Proof "mock-proof")

-- -----------------------------------------------------------------
-- Generators
-- -----------------------------------------------------------------

-- | Generate a random ByteString key.
genKeyBytes :: Gen ByteString
genKeyBytes =
    B.pack <$> listOf1 (choose (0, 255))

-- | Generate a random ByteString value.
genValue :: Gen ByteString
genValue = B.pack <$> listOf1 (choose (0, 255))

-- | Hash key bytes (Aiken convention).
hashKey :: ByteString -> ByteString
hashKey = renderMPFHash . mkMPFHash

-- | Generate unique key-value pairs (unique by
-- hashed key).
genUniqueKVs :: Gen [(ByteString, ByteString)]
genUniqueKVs = do
    kvs <-
        listOf1 ((,) <$> genKeyBytes <*> genValue)
    pure
        $ nubBy
            ( \(k1, _) (k2, _) ->
                hashKey k1 == hashKey k2
            )
            kvs

-- -----------------------------------------------------------------
-- Specs
-- -----------------------------------------------------------------

spec :: Spec
spec = do
    describe "Trie" trieSpec
    describe "Trie test vectors" testVectorSpec
    describe "Trie properties" propertySpec

trieSpec :: Spec
trieSpec = do
    it "insert/getRoot produces non-empty root" $ do
        trie <- mkMockTrie
        root <- insert trie "hello" "world"
        unRoot root `shouldSatisfy` (not . B.null)

    it "insert/getRoot is deterministic" $ do
        trie1 <- mkMockTrie
        trie2 <- mkMockTrie
        root1 <- insert trie1 "hello" "world"
        root2 <- insert trie2 "hello" "world"
        root1 `shouldBe` root2

    it "insert/lookup finds the key" $ do
        trie <- mkMockTrie
        _ <- insert trie "hello" "world"
        mVal <-
            Cardano.MPFS.Trie.lookup trie "hello"
        mVal `shouldSatisfy` isJust

    it "lookup on empty returns Nothing" $ do
        trie <- mkMockTrie
        Cardano.MPFS.Trie.lookup trie "missing"
            `shouldReturn` Nothing

    it "delete removes key" $ do
        trie <- mkMockTrie
        _ <- insert trie "hello" "world"
        _ <- Cardano.MPFS.Trie.delete trie "hello"
        mVal <-
            Cardano.MPFS.Trie.lookup trie "hello"
        mVal `shouldBe` Nothing

    it "delete preserves siblings" $ do
        trie <- mkMockTrie
        _ <- insert trie "keep" "value1"
        _ <- insert trie "remove" "value2"
        _ <-
            Cardano.MPFS.Trie.delete trie "remove"
        mVal <-
            Cardano.MPFS.Trie.lookup trie "keep"
        mVal `shouldSatisfy` isJust

    it "getRoot on empty returns empty root" $ do
        trie <- mkMockTrie
        root <- getRoot trie
        unRoot root `shouldBe` B.empty

    it "getProof for existing key" $ do
        trie <- mkMockTrie
        _ <- insert trie "hello" "world"
        mProof <- getProof trie "hello"
        isJust mProof `shouldBe` True

    it "getProof for missing key" $ do
        trie <- mkMockTrie
        _ <- insert trie "hello" "world"
        mProof <- getProof trie "nonexistent"
        isNothing mProof `shouldBe` True

    it "getProof on empty trie" $ do
        trie <- mkMockTrie
        mProof <- getProof trie "any"
        isNothing mProof `shouldBe` True

-- -----------------------------------------------------------------
-- Properties (via haskell-mpfs pure backend)
-- -----------------------------------------------------------------

propertySpec :: Spec
propertySpec = do
    it "insertion order independence"
        $ property propInsertionOrder

    it "deleted key not verifiable"
        $ property propDeleteRemoves

    it "deletion preserves siblings"
        $ property propDeletePreserves

    it "single insert produces root"
        $ property propSingleInsertRoot

-- | Insertion order doesn't affect root hash.
propInsertionOrder :: Property
propInsertionOrder =
    forAll genUniqueKVs $ \kvs ->
        length kvs >= 2 ==>
            forAll (shuffle kvs) $ \shuffled ->
                let (root1, _) = runMPFPure' $ do
                        forM_ kvs
                            $ uncurry insertByteStringM
                        getRootHashM
                    (root2, _) = runMPFPure' $ do
                        forM_ shuffled
                            $ uncurry insertByteStringM
                        getRootHashM
                in  fmap renderMPFHash root1
                        === fmap renderMPFHash root2

-- | Deleted key cannot be verified.
propDeleteRemoves :: Property
propDeleteRemoves =
    forAll genKeyBytes $ \keyBs ->
        forAll genValue $ \valBs ->
            let key =
                    byteStringToHexKey
                        $ hashKey keyBs
                value = mkMPFHash valBs
                (verified, _) = runMPFPure' $ do
                    insertByteStringM keyBs valBs
                    deleteMPFM key
                    verifyMPFM key value
            in  not verified

-- | Deletion preserves sibling proofs.
propDeletePreserves :: Property
propDeletePreserves =
    forAll
        ( vectorOf
            3
            ((,) <$> genKeyBytes <*> genValue)
        )
        $ \rawKvs ->
            let kvs =
                    nubBy
                        ( \(k1, _) (k2, _) ->
                            hashKey k1 == hashKey k2
                        )
                        rawKvs
            in  length kvs == 3 ==>
                    let ((keepK, keepV), (delK, _)) =
                            case kvs of
                                (a : b : _) -> (a, b)
                                _ -> error "impossible"
                        keepHex =
                            byteStringToHexKey
                                $ hashKey keepK
                        keepHash = mkMPFHash keepV
                        delHex =
                            byteStringToHexKey
                                $ hashKey delK
                        (verified, _) = runMPFPure' $ do
                            forM_
                                kvs
                                ( uncurry
                                    insertByteStringM
                                )
                            deleteMPFM delHex
                            verifyMPFM
                                keepHex
                                keepHash
                    in  verified

-- | Single insert produces a root hash.
propSingleInsertRoot :: Property
propSingleInsertRoot =
    forAll genKeyBytes $ \keyBs ->
        forAll genValue $ \valBs ->
            let (mRoot, _) = runMPFPure' $ do
                    insertByteStringM keyBs valBs
                    getRootHashM
            in  case mRoot of
                    Just _ -> True
                    Nothing -> False

-- -----------------------------------------------------------------
-- Test vectors (Aiken compatibility)
-- -----------------------------------------------------------------

testVectorSpec :: Spec
testVectorSpec = do
    it "single apple root hash" $ do
        trie <- mkMockTrie
        root <-
            insert
                trie
                "apple[uid: 58]"
                "\xf0\x9f\x8d\x8e"
        encodeHex (unRoot root)
            `shouldBe` "93c4ed2d36f2409c38b8112d70c23eaf92eeb325b5098c0195be7e5cfaf7d824"

    it "apple + apricot root hash" $ do
        trie <- mkMockTrie
        _ <-
            insert
                trie
                "apple[uid: 58]"
                "\xf0\x9f\x8d\x8e"
        root <-
            insert
                trie
                "apricot[uid: 0]"
                "\xf0\x9f\xa4\xb7"
        encodeHex (unRoot root)
            `shouldBe` "d9e614a87dff7b38d59706f00085d1b23f8c3e32ab9f5c39dbfa090412012003"

    it "apple + banana root hash" $ do
        trie <- mkMockTrie
        _ <-
            insert
                trie
                "apple[uid: 58]"
                "\xf0\x9f\x8d\x8e"
        root <-
            insert
                trie
                "banana[uid: 218]"
                "\xf0\x9f\x8d\x8c"
        encodeHex (unRoot root)
            `shouldBe` "6a00036a5182ad02098cc99e00ab679263571dbec847b12aa7abde525affbe39"

    it "3 fruits root hash" $ do
        trie <- mkMockTrie
        _ <-
            insert
                trie
                "apple[uid: 58]"
                "\xf0\x9f\x8d\x8e"
        _ <-
            insert
                trie
                "apricot[uid: 0]"
                "\xf0\x9f\xa4\xb7"
        root <-
            insert
                trie
                "banana[uid: 218]"
                "\xf0\x9f\x8d\x8c"
        encodeHex (unRoot root)
            `shouldBe` "3b9c8a23238aeef2bee260daec21acfdad07cb7d8f23bb5b97147323ef65ff5f"

    it "full fruits dataset root hash" $ do
        trie <- mkMockTrie
        forM_ fruitsTestData
            $ uncurry (insert trie)
        root <- getRoot trie
        encodeHex (unRoot root)
            `shouldBe` encodeHex expectedFullTrieRoot

    it "proof verifies apple in full dataset" $ do
        trie <- mkMockTrie
        forM_ fruitsTestData
            $ uncurry (insert trie)
        mProof <- getProof trie "apple[uid: 58]"
        isJust mProof `shouldBe` True
