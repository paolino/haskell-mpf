{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Cardano.MPFS.Indexer.Columns
-- Description : Column family GADT for indexer persistent state
-- License     : Apache-2.0
--
-- Type-safe column family definitions for the
-- indexer's RocksDB-backed persistent state using
-- @rocksdb-kv-transactions@. Each 'AllColumns'
-- constructor selects a RocksDB column family with
-- its key-value types enforced at the type level via
-- the 'KV' kind. Six column families cover:
--
--   * Cage state: 'CageTokens', 'CageRequests', 'CageCfg'
--   * Rollback storage: 'CageRollbacks'
--   * Trie storage: 'TrieNodes', 'TrieKV'
--
-- Serialization codecs for these columns live in
-- "Cardano.MPFS.Indexer.Codecs".
module Cardano.MPFS.Indexer.Columns
    ( -- * Column selector
      AllColumns (..)

      -- * Checkpoint type
    , CageCheckpoint (..)

      -- * Rollback entry type
    , CageRollbackEntry (..)
    ) where

import Data.ByteString (ByteString)

import Control.Lens (type (:~:) (..))
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    )

import Cardano.MPFS.Core.Types
    ( BlockId
    , Request
    , SlotNo
    , TokenId
    , TokenState
    , TxIn
    )
import Cardano.MPFS.Indexer.Event (CageInverseOp)

-- | Chain sync checkpoint stored in the cage-cfg
-- column family.
data CageCheckpoint = CageCheckpoint
    { checkpointSlot :: !SlotNo
    -- ^ Slot of the last processed block
    , checkpointBlockId :: !BlockId
    -- ^ Header hash of the last processed block
    , rollbackSlots :: ![SlotNo]
    -- ^ Slots with stored inverse ops, bounded by
    -- the security parameter (k ≈ 2160).
    }
    deriving stock (Eq, Show)

-- | Inverse ops stored for a single block's slot,
-- used for rollback.
newtype CageRollbackEntry = CageRollbackEntry
    { unRollbackEntry :: [CageInverseOp]
    }
    deriving stock (Eq, Show)

-- | Column family selector for indexer persistent
-- state. Covers cage state, rollback storage, and
-- per-token trie storage.
data AllColumns x where
    -- | Token state: maps token identifiers to
    -- their on-chain state.
    CageTokens
        :: AllColumns (KV TokenId TokenState)
    -- | Pending requests: maps UTxO references to
    -- request details.
    CageRequests
        :: AllColumns (KV TxIn Request)
    -- | Singleton checkpoint: stores the last
    -- processed block position.
    CageCfg
        :: AllColumns (KV () CageCheckpoint)
    -- | Rollback storage: maps slot numbers to
    -- inverse ops for rollback.
    CageRollbacks
        :: AllColumns (KV SlotNo CageRollbackEntry)
    -- | Trie nodes: MPF trie structure. Keys are
    -- token-prefixed serialized 'HexKey', values
    -- are serialized 'HexIndirect'.
    TrieNodes
        :: AllColumns (KV ByteString ByteString)
    -- | Trie key-value pairs: user data stored in
    -- per-token tries. Keys are token-prefixed.
    TrieKV
        :: AllColumns (KV ByteString ByteString)

instance GEq AllColumns where
    geq CageTokens CageTokens = Just Refl
    geq CageRequests CageRequests = Just Refl
    geq CageCfg CageCfg = Just Refl
    geq CageRollbacks CageRollbacks = Just Refl
    geq TrieNodes TrieNodes = Just Refl
    geq TrieKV TrieKV = Just Refl
    geq _ _ = Nothing

instance GCompare AllColumns where
    gcompare CageTokens CageTokens = GEQ
    gcompare CageTokens _ = GLT
    gcompare _ CageTokens = GGT
    gcompare CageRequests CageRequests = GEQ
    gcompare CageRequests _ = GLT
    gcompare _ CageRequests = GGT
    gcompare CageCfg CageCfg = GEQ
    gcompare CageCfg _ = GLT
    gcompare _ CageCfg = GGT
    gcompare CageRollbacks CageRollbacks = GEQ
    gcompare CageRollbacks _ = GLT
    gcompare _ CageRollbacks = GGT
    gcompare TrieNodes TrieNodes = GEQ
    gcompare TrieNodes TrieKV = GLT
    gcompare TrieKV TrieNodes = GGT
    gcompare TrieKV TrieKV = GEQ
