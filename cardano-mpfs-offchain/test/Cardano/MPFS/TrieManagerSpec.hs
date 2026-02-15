{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.TrieManagerSpec
-- Description : Tests for the TrieManager interface
-- License     : Apache-2.0
module Cardano.MPFS.TrieManagerSpec (spec) where

import Data.ByteString qualified as B
import Data.ByteString.Short qualified as SBS
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

import Control.Exception (SomeException, try)
import Control.Monad (void)
import MPF.Backend.Pure
    ( MPFInMemoryDB
    , emptyMPFInMemoryDB
    , runMPFPure
    )
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( deleteMPFM
    , getRootHashM
    , insertByteStringM
    , proofMPFM
    )

import Cardano.MPFS.Trie
    ( Trie (..)
    , TrieManager (..)
    )
import Cardano.MPFS.Types
    ( AssetName (..)
    , Root (..)
    , TokenId (..)
    )

-- -----------------------------------------------------------------
-- Deterministic token IDs for tests
-- -----------------------------------------------------------------

-- | A deterministic 'TokenId' for tests.
tokenA :: TokenId
tokenA = TokenId (AssetName (SBS.pack [1, 2, 3]))

-- | A second deterministic 'TokenId'.
tokenB :: TokenId
tokenB = TokenId (AssetName (SBS.pack [4, 5, 6]))

-- -----------------------------------------------------------------
-- Mock TrieManager backed by Map of IORef databases
-- -----------------------------------------------------------------

-- | Create a mock 'TrieManager IO' backed by a
-- 'Map' of per-token in-memory databases.
mkMockTrieManager :: IO (TrieManager IO)
mkMockTrieManager = do
    ref <-
        newIORef
            ( Map.empty
                :: Map TokenId (IORef MPFInMemoryDB)
            )
    pure
        TrieManager
            { withTrie = mockWithTrie ref
            , createTrie = mockCreateTrie ref
            , deleteTrie = mockDeleteTrie ref
            }

-- | Run an action with access to a token's trie.
-- Throws if the trie doesn't exist.
mockWithTrie
    :: IORef (Map TokenId (IORef MPFInMemoryDB))
    -> TokenId
    -> (Trie IO -> IO a)
    -> IO a
mockWithTrie ref tid action = do
    tries <- readIORef ref
    case Map.lookup tid tries of
        Nothing ->
            error
                $ "Trie not found: " ++ show tid
        Just dbRef ->
            action (mkTrieFromDbRef dbRef)

-- | Create a new empty trie for a token.
mockCreateTrie
    :: IORef (Map TokenId (IORef MPFInMemoryDB))
    -> TokenId
    -> IO ()
mockCreateTrie ref tid = do
    dbRef <- newIORef emptyMPFInMemoryDB
    modifyIORef' ref (Map.insert tid dbRef)

-- | Delete a token's trie.
mockDeleteTrie
    :: IORef (Map TokenId (IORef MPFInMemoryDB))
    -> TokenId
    -> IO ()
mockDeleteTrie ref =
    modifyIORef' ref . Map.delete

-- | Build a 'Trie IO' from an 'IORef' to an
-- in-memory database.
mkTrieFromDbRef :: IORef MPFInMemoryDB -> Trie IO
mkTrieFromDbRef ref =
    Trie
        { insert = \k v -> do
            db <- readIORef ref
            let ((), db') =
                    runMPFPure
                        db
                        (insertByteStringM k v)
            modifyIORef' ref (const db')
            getRootFromDb db'
        , delete = \k -> do
            db <- readIORef ref
            let hexKey =
                    byteStringToHexKey
                        $ renderMPFHash
                        $ mkMPFHash k
                ((), db') =
                    runMPFPure
                        db
                        (deleteMPFM hexKey)
            modifyIORef' ref (const db')
            getRootFromDb db'
        , lookup = \k -> do
            db <- readIORef ref
            let hexKey =
                    byteStringToHexKey
                        $ renderMPFHash
                        $ mkMPFHash k
                (mProof, _) =
                    runMPFPure
                        db
                        (proofMPFM hexKey)
            pure $ case mProof of
                Nothing -> Nothing
                Just _ ->
                    Just
                        ( renderMPFHash
                            (mkMPFHash k)
                        )
        , getRoot = do
            db <- readIORef ref
            getRootFromDb db
        , getProof = \_ -> pure Nothing
        }

-- | Get root hash from a database snapshot.
getRootFromDb :: MPFInMemoryDB -> IO Root
getRootFromDb db =
    let (mHash, _) = runMPFPure db getRootHashM
    in  pure $ case mHash of
            Nothing -> Root B.empty
            Just h -> Root (renderMPFHash h)

-- -----------------------------------------------------------------
-- Specs
-- -----------------------------------------------------------------

spec :: Spec
spec = describe "TrieManager" trieManagerSpec

trieManagerSpec :: Spec
trieManagerSpec = do
    it "createTrie/withTrie round-trip" $ do
        tm <- mkMockTrieManager
        createTrie tm tokenA
        withTrie tm tokenA $ \trie -> do
            root <- getRoot trie
            unRoot root `shouldBe` B.empty

    it "withTrie on missing trie throws" $ do
        tm <- mkMockTrieManager
        result <-
            try
                $ withTrie tm tokenA
                $ \_ -> pure ()
        case ( result
                :: Either SomeException ()
             ) of
            Left _ -> pure ()
            Right _ ->
                error
                    "Expected exception for \
                    \missing trie"

    it "deleteTrie removes trie" $ do
        tm <- mkMockTrieManager
        createTrie tm tokenA
        deleteTrie tm tokenA
        result <-
            try
                $ withTrie tm tokenA
                $ \_ -> pure ()
        case ( result
                :: Either SomeException ()
             ) of
            Left _ -> pure ()
            Right _ ->
                error
                    "Expected exception after \
                    \delete"

    it "per-token isolation" $ do
        tm <- mkMockTrieManager
        createTrie tm tokenA
        createTrie tm tokenB
        -- Insert into tokenA only
        withTrie tm tokenA $ \trie ->
            void $ insert trie "key" "value"
        -- tokenB should still be empty
        withTrie tm tokenB $ \trie -> do
            root <- getRoot trie
            unRoot root `shouldBe` B.empty

    it "createTrie overwrites existing" $ do
        tm <- mkMockTrieManager
        createTrie tm tokenA
        withTrie tm tokenA $ \trie ->
            void $ insert trie "key" "value"
        -- Recreate should give empty trie
        createTrie tm tokenA
        withTrie tm tokenA $ \trie -> do
            root <- getRoot trie
            unRoot root `shouldBe` B.empty

    it "withTrie insert reflects in root" $ do
        tm <- mkMockTrieManager
        createTrie tm tokenA
        withTrie tm tokenA $ \trie -> do
            root <- insert trie "hello" "world"
            unRoot root
                `shouldSatisfy` (not . B.null)

    it "deleteTrie on empty doesn't crash" $ do
        tm <- mkMockTrieManager
        deleteTrie tm tokenA
        pure ()
