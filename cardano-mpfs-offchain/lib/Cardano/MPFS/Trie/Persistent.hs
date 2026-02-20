{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Cardano.MPFS.Trie.Persistent
-- Description : RocksDB-backed TrieManager with token-prefixed keys
-- License     : Apache-2.0
--
-- Provides a 'TrieManager IO' backed by shared
-- RocksDB column families. Each token's trie data
-- is isolated by prefixing all keys with the
-- serialized 'TokenId'. Uses MPF operations
-- through a custom 'Database' that handles
-- prefixing transparently.
module Cardano.MPFS.Trie.Persistent
    ( -- * Construction
      mkPersistentTrieManager
    ) where

import Control.Lens (Prism')
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Set (Set)
import Data.Set qualified as Set

import Database.KV.Database
    ( Codecs (..)
    , Column (..)
    , DSum ((:=>))
    , Database (..)
    , Pos (..)
    , QueryIterator (..)
    , fromPairList
    )
import Database.KV.Transaction
    ( Transaction
    , query
    , runTransactionUnguarded
    )
import Database.RocksDB
    ( BatchOp (..)
    , ColumnFamily
    , DB
    , createIterator
    , destroyIterator
    , getCF
    , iterEntry
    , iterLast
    , iterNext
    , iterPrev
    , iterSeek
    , iterValid
    , write
    )

import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    )
import MPF.Deletion (deleting)
import MPF.Hashes
    ( MPFHash
    , MPFHashing (..)
    , mkMPFHash
    , mpfHashing
    , renderMPFHash
    )
import MPF.Insertion (inserting)
import MPF.Interface
    ( HexIndirect (..)
    , HexKey
    , byteStringToHexKey
    , hexKeyPrism
    , mpfCodecs
    )
import MPF.Proof.Insertion
    ( mkMPFInclusionProof
    )
import MPF.Test.Lib
    ( fromHexKVIdentity
    , mpfHashCodecs
    )

import Cardano.MPFS.OnChain (ProofStep)
import Cardano.MPFS.Proof
    ( serializeProof
    , toProofSteps
    )
import Cardano.MPFS.Trie
    ( Proof (..)
    , Trie (..)
    , TrieManager (..)
    )
import Cardano.MPFS.Types
    ( AssetName (..)
    , Root (..)
    , TokenId (..)
    )

-- | Create a persistent 'TrieManager IO' backed by
-- shared RocksDB column families. Each token's trie
-- data is isolated via key prefixing.
mkPersistentTrieManager
    :: DB
    -- ^ Shared RocksDB handle
    -> ColumnFamily
    -- ^ Column family for trie nodes
    -> ColumnFamily
    -- ^ Column family for trie key-value pairs
    -> IO (TrieManager IO)
mkPersistentTrieManager db nodesCF kvCF = do
    knownRef <- newIORef (Set.empty :: Set TokenId)
    pure
        TrieManager
            { withTrie =
                persistentWithTrie
                    db
                    nodesCF
                    kvCF
                    knownRef
            , createTrie =
                persistentCreateTrie
                    db
                    nodesCF
                    kvCF
                    knownRef
            , deleteTrie =
                persistentDeleteTrie
                    db
                    nodesCF
                    kvCF
                    knownRef
            }

-- | Serialize a 'TokenId' to a prefix byte string.
tokenPrefix :: TokenId -> ByteString
tokenPrefix (TokenId (AssetName sbs)) =
    let raw = SBS.fromShort sbs
        len = BS.length raw
    in  BS.singleton (fromIntegral len) <> raw

-- | Run an action with a token's trie. Creates a
-- prefixed 'Database' for the token and wraps MPF
-- operations.
persistentWithTrie
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> IORef (Set TokenId)
    -> TokenId
    -> (Trie IO -> IO a)
    -> IO a
persistentWithTrie db nodesCF kvCF knownRef tid action = do
    known <- readIORef knownRef
    if Set.member tid known
        then do
            let pfx = tokenPrefix tid
                database =
                    mkPrefixedTrieDB
                        db
                        nodesCF
                        kvCF
                        pfx
            action (mkPersistentTrie database)
        else
            error
                $ "Trie not found: " ++ show tid

-- | Create a new empty trie for a token. Previous
-- data is deleted if the token already exists.
persistentCreateTrie
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> IORef (Set TokenId)
    -> TokenId
    -> IO ()
persistentCreateTrie db nodesCF kvCF knownRef tid = do
    known <- readIORef knownRef
    -- Delete existing data if present
    if Set.member tid known
        then deleteAllWithPrefix db nodesCF kvCF pfx
        else pure ()
    modifyIORef' knownRef (Set.insert tid)
  where
    pfx = tokenPrefix tid

-- | Delete a token's trie and all its data.
persistentDeleteTrie
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> IORef (Set TokenId)
    -> TokenId
    -> IO ()
persistentDeleteTrie db nodesCF kvCF knownRef tid = do
    deleteAllWithPrefix db nodesCF kvCF (tokenPrefix tid)
    modifyIORef' knownRef (Set.delete tid)

-- --------------------------------------------------------
-- Prefixed Database construction
-- --------------------------------------------------------

-- | Create a 'Database' that prefixes all keys with
-- the given prefix and routes operations to the
-- shared column families.
mkPrefixedTrieDB
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> ByteString
    -> Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
mkPrefixedTrieDB db nodesCF kvCF pfx =
    Database
        { valueAt = \cf key ->
            getCF db cf (pfx <> key)
        , applyOps = write db
        , mkOperation = \cf key mv ->
            case mv of
                Just v -> PutCF cf (pfx <> key) v
                Nothing -> DelCF cf (pfx <> key)
        , columns =
            fromPairList
                [ MPFStandaloneKVCol
                    :=> Column
                        { family = kvCF
                        , codecs =
                            Codecs
                                { keyCodec =
                                    hexKeyPrism
                                , valueCodec =
                                    isoMPFHash
                                }
                        }
                , MPFStandaloneMPFCol
                    :=> Column
                        { family = nodesCF
                        , codecs =
                            mpfCodecs isoMPFHash
                        }
                ]
        , newIterator = \cf ->
            mkPrefixedIterator db cf pfx
        }
  where
    isoMPFHash :: Prism' ByteString MPFHash
    isoMPFHash = mpfValueCodec mpfHashCodecs

-- --------------------------------------------------------
-- Prefixed iterator
-- --------------------------------------------------------

-- | Create an iterator that only sees entries with
-- the given prefix. Keys are stripped of the prefix
-- when returned.
mkPrefixedIterator
    :: DB
    -> ColumnFamily
    -> ByteString
    -> IO (QueryIterator IO)
mkPrefixedIterator db cf pfx = do
    i <- createIterator db (Just cf)
    pure
        QueryIterator
            { step = \pos -> case pos of
                PosFirst -> iterSeek i pfx
                PosLast -> do
                    -- Seek past prefix range and step back
                    iterSeek i (incrementPrefix pfx)
                    v <- iterValid i
                    if v
                        then iterPrev i
                        else iterLast i
                PosNext -> iterNext i
                PosPrev -> iterPrev i
                PosAny k -> iterSeek i (pfx <> k)
                PosDestroy -> destroyIterator i
            , isValid = do
                v <- iterValid i
                if v
                    then do
                        me <- iterEntry i
                        case me of
                            Just (k, _) ->
                                pure
                                    (pfx `BS.isPrefixOf` k)
                            Nothing -> pure False
                    else pure False
            , entry = do
                me <- iterEntry i
                case me of
                    Just (k, v)
                        | pfx `BS.isPrefixOf` k ->
                            pure
                                $ Just
                                    (BS.drop (BS.length pfx) k, v)
                    _ -> pure Nothing
            }

-- | Increment the last byte of a prefix to get the
-- upper bound for prefix scanning.
incrementPrefix :: ByteString -> ByteString
incrementPrefix bs
    | BS.null bs = BS.singleton 0
    | otherwise =
        let lastByte = BS.last bs
        in  if lastByte == 0xFF
                then
                    incrementPrefix (BS.init bs)
                        <> BS.singleton 0
                else
                    BS.init bs
                        <> BS.singleton (lastByte + 1)

-- --------------------------------------------------------
-- Trie construction from prefixed Database
-- --------------------------------------------------------

-- | Create a 'Trie IO' from a prefixed 'Database'.
mkPersistentTrie
    :: Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
    -> Trie IO
mkPersistentTrie database =
    Trie
        { insert = persistentInsert database
        , delete = persistentDelete database
        , lookup = persistentLookup database
        , getRoot = persistentGetRoot database
        , getProof = persistentGetProof database
        , getProofSteps =
            persistentGetProofSteps database
        }

persistentInsert
    :: Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
    -> ByteString
    -> ByteString
    -> IO Root
persistentInsert database k v =
    runTransactionUnguarded database $ do
        inserting
            fromHexKVIdentity
            mpfHashing
            MPFStandaloneKVCol
            MPFStandaloneMPFCol
            (byteStringToHexKey (hashBS k))
            (mkMPFHash v)
        persistentGetRootT

persistentDelete
    :: Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
    -> ByteString
    -> IO Root
persistentDelete database k =
    runTransactionUnguarded database $ do
        deleting
            fromHexKVIdentity
            mpfHashing
            MPFStandaloneKVCol
            MPFStandaloneMPFCol
            (byteStringToHexKey (hashBS k))
        persistentGetRootT

persistentLookup
    :: Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
    -> ByteString
    -> IO (Maybe ByteString)
persistentLookup database k =
    runTransactionUnguarded database $ do
        let hexKey =
                byteStringToHexKey (hashBS k)
        mProof <-
            mkMPFInclusionProof
                fromHexKVIdentity
                mpfHashing
                MPFStandaloneMPFCol
                hexKey
        pure $ case mProof of
            Nothing -> Nothing
            Just _ -> Just (hashBS k)

persistentGetRoot
    :: Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
    -> IO Root
persistentGetRoot database =
    runTransactionUnguarded database persistentGetRootT

-- | Get root hash within a transaction.
persistentGetRootT
    :: Database.KV.Transaction.Transaction
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
        Root
persistentGetRootT = do
    mi <- query MPFStandaloneMPFCol []
    pure $ case mi of
        Nothing -> Root BS.empty
        Just HexIndirect{hexIsLeaf, hexJump, hexValue} ->
            Root
                $ renderMPFHash
                $ if hexIsLeaf
                    then
                        leafHash
                            mpfHashing
                            hexJump
                            hexValue
                    else hexValue

persistentGetProof
    :: Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
    -> ByteString
    -> IO (Maybe Proof)
persistentGetProof database k =
    runTransactionUnguarded database $ do
        let hexKey =
                byteStringToHexKey (hashBS k)
        mProof <-
            mkMPFInclusionProof
                fromHexKVIdentity
                mpfHashing
                MPFStandaloneMPFCol
                hexKey
        pure $ fmap (Proof . serializeProof) mProof

persistentGetProofSteps
    :: Database
        IO
        ColumnFamily
        (MPFStandalone HexKey MPFHash MPFHash)
        BatchOp
    -> ByteString
    -> IO (Maybe [ProofStep])
persistentGetProofSteps database k =
    runTransactionUnguarded database $ do
        let hexKey =
                byteStringToHexKey (hashBS k)
        mProof <-
            mkMPFInclusionProof
                fromHexKVIdentity
                mpfHashing
                MPFStandaloneMPFCol
                hexKey
        pure $ fmap toProofSteps mProof

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

-- | Hash bytes using MPF convention.
hashBS :: ByteString -> ByteString
hashBS = renderMPFHash . mkMPFHash

-- | Delete all entries with a given prefix from both
-- column families.
deleteAllWithPrefix
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> ByteString
    -> IO ()
deleteAllWithPrefix db nodesCF kvCF pfx = do
    ops1 <- collectDeleteOps db nodesCF pfx
    ops2 <- collectDeleteOps db kvCF pfx
    let allOps = ops1 ++ ops2
    if null allOps
        then pure ()
        else write db allOps

-- | Collect delete operations for all entries with
-- a given prefix in a column family.
collectDeleteOps
    :: DB
    -> ColumnFamily
    -> ByteString
    -> IO [BatchOp]
collectDeleteOps db cf pfx = do
    i <- createIterator db (Just cf)
    iterSeek i pfx
    ops <- go i []
    destroyIterator i
    pure ops
  where
    go i acc = do
        v <- iterValid i
        if v
            then do
                me <- iterEntry i
                case me of
                    Just (k, _)
                        | pfx `BS.isPrefixOf` k -> do
                            iterNext i
                            go i (DelCF cf k : acc)
                    _ -> pure (reverse acc)
            else pure (reverse acc)
