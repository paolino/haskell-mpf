{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Cardano.MPFS.Trie.PureManager
-- Description : Pure in-memory TrieManager
-- License     : Apache-2.0
--
-- Provides a 'TrieManager IO' backed by a 'Map' of
-- per-token 'IORef' databases. Useful for testing
-- and development.
module Cardano.MPFS.Trie.PureManager
    ( -- * Construction
      mkPureTrieManager
    ) where

import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import MPF.Backend.Pure
    ( MPFInMemoryDB
    , emptyMPFInMemoryDB
    )

import Cardano.MPFS.Trie (Trie, TrieManager (..))
import Cardano.MPFS.Trie.Pure (mkPureTrieFromRef)
import Cardano.MPFS.Types (TokenId)

-- | Create a new 'TrieManager IO' backed by a 'Map'
-- of per-token in-memory MPF databases. Each token
-- gets its own isolated trie.
mkPureTrieManager :: IO (TrieManager IO)
mkPureTrieManager = do
    ref <-
        newIORef
            ( Map.empty
                :: Map TokenId (IORef MPFInMemoryDB)
            )
    pure
        TrieManager
            { withTrie = pureWithTrie ref
            , withSpeculativeTrie =
                pureWithSpeculativeTrie ref
            , createTrie = pureCreateTrie ref
            , deleteTrie = pureDeleteTrie ref
            }

-- | Run an action with access to a token's trie.
-- Throws if the trie doesn't exist.
pureWithTrie
    :: IORef (Map TokenId (IORef MPFInMemoryDB))
    -> TokenId
    -> (Trie IO -> IO a)
    -> IO a
pureWithTrie ref tid action = do
    tries <- readIORef ref
    case Map.lookup tid tries of
        Nothing ->
            error
                $ "Trie not found: " ++ show tid
        Just dbRef ->
            action (mkPureTrieFromRef dbRef)

-- | Run a speculative session on a copy of the
-- trie. The copy is discarded after the action.
pureWithSpeculativeTrie
    :: IORef (Map TokenId (IORef MPFInMemoryDB))
    -> TokenId
    -> (forall n. Monad n => Trie n -> n a)
    -> IO a
pureWithSpeculativeTrie ref tid action = do
    tries <- readIORef ref
    case Map.lookup tid tries of
        Nothing ->
            error
                $ "Trie not found: " ++ show tid
        Just dbRef -> do
            db <- readIORef dbRef
            copyRef <- newIORef db
            action (mkPureTrieFromRef copyRef)

-- | Create a new empty trie for a token.
pureCreateTrie
    :: IORef (Map TokenId (IORef MPFInMemoryDB))
    -> TokenId
    -> IO ()
pureCreateTrie ref tid = do
    dbRef <- newIORef emptyMPFInMemoryDB
    modifyIORef' ref (Map.insert tid dbRef)

-- | Delete a token's trie.
pureDeleteTrie
    :: IORef (Map TokenId (IORef MPFInMemoryDB))
    -> TokenId
    -> IO ()
pureDeleteTrie ref =
    modifyIORef' ref . Map.delete
