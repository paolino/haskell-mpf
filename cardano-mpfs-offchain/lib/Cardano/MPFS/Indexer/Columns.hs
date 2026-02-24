{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Cardano.MPFS.Indexer.Columns
-- Description : Column family GADT for indexer persistent state
-- License     : Apache-2.0
--
-- Type-safe column family definitions for the
-- indexer's RocksDB-backed persistent state. Each
-- constructor selects a column with its key-value
-- types enforced at the type level. Covers cage
-- state (tokens, requests, checkpoint) and trie
-- storage (nodes, key-value pairs).
module Cardano.MPFS.Indexer.Columns
    ( -- * Column selector
      AllColumns (..)

      -- * Trie registry
    , TrieStatus (..)

      -- * Checkpoint type
    , CageCheckpoint (..)
    ) where

import Data.ByteString (ByteString)

import Control.Lens (type (:~:) (..))
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    )

import Cardano.MPFS.Types
    ( BlockId
    , Request
    , SlotNo
    , TokenId
    , TokenState
    , TxIn
    )

-- | Visibility status of a token's trie in the
-- persistent registry. Starts as 'TrieVisible';
-- will gain a 'TrieHidden' constructor for burn
-- rollback (issue #34).
data TrieStatus = TrieVisible
    deriving stock (Eq, Show)

-- | Chain sync checkpoint stored in the cage-cfg
-- column family.
data CageCheckpoint = CageCheckpoint
    { checkpointSlot :: !SlotNo
    -- ^ Slot of the last processed block
    , checkpointBlockId :: !BlockId
    -- ^ Header hash of the last processed block
    }
    deriving stock (Eq, Show)

-- | Column family selector for indexer persistent
-- state. Covers cage state and per-token trie
-- storage. UTxO columns (from cardano-utxo-csmt)
-- will be added when integrating the Follower.
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
    -- | Trie nodes: MPF trie structure. Keys are
    -- token-prefixed serialized 'HexKey', values
    -- are serialized 'HexIndirect'.
    TrieNodes
        :: AllColumns (KV ByteString ByteString)
    -- | Trie key-value pairs: user data stored in
    -- per-token tries. Keys are token-prefixed.
    TrieKV
        :: AllColumns (KV ByteString ByteString)
    -- | Trie metadata registry: tracks which tokens
    -- have persistent tries.
    TrieMetadata
        :: AllColumns (KV TokenId TrieStatus)

instance GEq AllColumns where
    geq CageTokens CageTokens = Just Refl
    geq CageRequests CageRequests = Just Refl
    geq CageCfg CageCfg = Just Refl
    geq TrieNodes TrieNodes = Just Refl
    geq TrieKV TrieKV = Just Refl
    geq TrieMetadata TrieMetadata = Just Refl
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
    gcompare TrieNodes TrieNodes = GEQ
    gcompare TrieNodes _ = GLT
    gcompare _ TrieNodes = GGT
    gcompare TrieKV TrieKV = GEQ
    gcompare TrieKV TrieMetadata = GLT
    gcompare TrieMetadata TrieKV = GGT
    gcompare TrieMetadata TrieMetadata = GEQ
