{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.Trie.PersistentSpec
-- Description : Integration tests for RocksDB-backed trie
-- License     : Apache-2.0
--
-- Runs the parameterized 'TrieSpec' and
-- 'TrieManagerSpec' suites against the persistent
-- RocksDB backend. The RocksDB handle is provided
-- by the caller (typically the test main).
module Cardano.MPFS.Trie.PersistentSpec
    ( -- * Test suite
      spec

      -- * Test utilities
    , withTestDB
    ) where

import Data.ByteString.Short qualified as SBS
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    )
import Data.Word (Word8)
import Test.Hspec (Spec, describe)

import Cardano.Ledger.Mary.Value (AssetName (..))
import Database.RocksDB
    ( ColumnFamily
    , Config (..)
    , DB (..)
    , withDBCF
    )
import System.IO.Temp (withSystemTempDirectory)

import Cardano.MPFS.Trie
    ( Trie
    , TrieManager (..)
    )
import Cardano.MPFS.Trie.Persistent
    ( mkPersistentTrieManager
    )
import Cardano.MPFS.TrieManagerSpec qualified as TrieManagerSpec
import Cardano.MPFS.TrieSpec qualified as TrieSpec
import Cardano.MPFS.Types
    ( TokenId (..)
    )

-- | Default config for test RocksDB.
testConfig :: Config
testConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Run an action with a temporary RocksDB that has
-- "nodes" and "kv" column families.
withTestDB
    :: (DB -> ColumnFamily -> ColumnFamily -> IO a)
    -> IO a
withTestDB action =
    withSystemTempDirectory "mpfs-test" $ \dir ->
        withDBCF
            dir
            testConfig
            [ ("nodes", testConfig)
            , ("kv", testConfig)
            ]
            $ \db@DB{columnFamilies = cfs} ->
                case cfs of
                    [nodesCF, kvCF] ->
                        action db nodesCF kvCF
                    _ ->
                        error
                            "Expected 2 column \
                            \families"

-- | Run all persistent trie tests.
spec
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> IORef Int
    -- ^ Counter for unique token IDs
    -> Spec
spec db nodesCF kvCF counterRef = do
    describe "Persistent Trie" $ do
        TrieSpec.spec
            ( newPersistentTrie
                db
                nodesCF
                kvCF
                counterRef
            )
    describe "Persistent TrieManager" $ do
        TrieManagerSpec.spec
            ( mkPersistentTrieManager
                db
                nodesCF
                kvCF
            )

-- | Create a fresh persistent 'Trie IO' for each
-- test. Uses the counter to generate unique token
-- IDs, ensuring test isolation.
newPersistentTrie
    :: DB
    -> ColumnFamily
    -> ColumnFamily
    -> IORef Int
    -> IO (Trie IO)
newPersistentTrie db nodesCF kvCF counterRef = do
    tm <- mkPersistentTrieManager db nodesCF kvCF
    tid <- nextTokenId counterRef
    createTrie tm tid
    withTrie tm tid pure

-- | Generate a unique 'TokenId'.
nextTokenId :: IORef Int -> IO TokenId
nextTokenId ref =
    atomicModifyIORef' ref $ \n ->
        ( n + 1
        , TokenId
            $ AssetName
            $ SBS.pack
            $ encodeInt n
        )

-- | Encode an 'Int' as bytes for unique token names.
encodeInt :: Int -> [Word8]
encodeInt n =
    [ fromIntegral (n `div` 256)
    , fromIntegral (n `mod` 256)
    ]
