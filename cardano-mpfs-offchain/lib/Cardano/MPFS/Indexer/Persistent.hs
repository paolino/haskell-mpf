{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Cardano.MPFS.Indexer.Persistent
-- Description : RocksDB-backed persistent State
-- License     : Apache-2.0
--
-- Implements the 'State' interface on top of
-- RocksDB via @rocksdb-kv-transactions@. Each
-- individual operation (get, put, remove, list) runs
-- in its own serialized transaction. Column families
-- are defined in "Cardano.MPFS.Indexer.Columns" and
-- serialization codecs in "Cardano.MPFS.Indexer.Codecs".
module Cardano.MPFS.Indexer.Persistent
    ( -- * Construction
      mkPersistentState
    ) where

import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Transaction
    ( RunTransaction (..)
    , delete
    , insert
    , iterating
    , query
    )

import Cardano.MPFS.Core.Types (Request (..))
import Cardano.MPFS.Indexer.Columns
    ( AllColumns (..)
    , CageCheckpoint (..)
    )
import Cardano.MPFS.State
    ( Checkpoints (..)
    , Requests (..)
    , State (..)
    , Tokens (..)
    )

-- | Create a persistent 'State' backed by RocksDB.
-- Each operation runs in its own serialized
-- transaction via 'RunTransaction'.
mkPersistentState
    :: RunTransaction IO cf AllColumns op
    -- ^ Transaction runner from @rocksdb-kv-transactions@
    -> State IO
mkPersistentState rt =
    State
        { tokens = mkTokens rt
        , requests = mkRequests rt
        , checkpoints = mkCheckpoints rt
        }

-- --------------------------------------------------------
-- Tokens
-- --------------------------------------------------------

mkTokens
    :: RunTransaction IO cf AllColumns op
    -> Tokens IO
mkTokens RunTransaction{runTransaction = run} =
    Tokens
        { getToken =
            run . query CageTokens
        , putToken = \tid ts ->
            run $ insert CageTokens tid ts
        , removeToken =
            run . delete CageTokens
        , listTokens =
            run $ iterating CageTokens allKeys
        }
  where
    allKeys = do
        me <- firstEntry
        case me of
            Nothing -> pure []
            Just (Entry k _) -> goK [k]
    goK acc = do
        me <- nextEntry
        case me of
            Nothing -> pure (reverse acc)
            Just (Entry k _) -> goK (k : acc)

-- --------------------------------------------------------
-- Requests
-- --------------------------------------------------------

mkRequests
    :: RunTransaction IO cf AllColumns op
    -> Requests IO
mkRequests RunTransaction{runTransaction = run} =
    Requests
        { getRequest =
            run . query CageRequests
        , putRequest = \txIn req ->
            run $ insert CageRequests txIn req
        , removeRequest =
            run . delete CageRequests
        , requestsByToken =
            run
                . iterating CageRequests
                . filterReqs
        }
  where
    filterReqs tid = do
        me <- firstEntry
        case me of
            Nothing -> pure []
            Just (Entry _ v) -> goR tid (add tid v [])
    goR tid acc = do
        me <- nextEntry
        case me of
            Nothing -> pure (reverse acc)
            Just (Entry _ v) ->
                goR tid (add tid v acc)
    add tid v acc
        | requestToken v == tid = v : acc
        | otherwise = acc

-- --------------------------------------------------------
-- Checkpoints
-- --------------------------------------------------------

mkCheckpoints
    :: RunTransaction IO cf AllColumns op
    -> Checkpoints IO
mkCheckpoints RunTransaction{runTransaction = run} =
    Checkpoints
        { getCheckpoint = run $ do
            mc <- query CageCfg ()
            pure $ case mc of
                Nothing -> Nothing
                Just CageCheckpoint{..} ->
                    Just
                        ( checkpointSlot
                        , checkpointBlockId
                        , rollbackSlots
                        )
        , putCheckpoint = \s b slots ->
            run
                $ insert CageCfg ()
                $ CageCheckpoint s b slots
        }
