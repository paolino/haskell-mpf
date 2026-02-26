{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : Cardano.MPFS.Indexer.CageFollower
-- Description : Block processor coordinating UTxO index and cage state
-- License     : Apache-2.0
--
-- Implements a 'Follower' that processes each block
-- from ChainSync by:
--
--   1. Applying UTxO changes via cardano-utxo-csmt's
--      'Update' state machine ('forwardTipApply')
--   2. Detecting cage-protocol events and applying
--      state\/trie mutations
--   3. Storing inverse operations for rollback
--   4. Updating the cage checkpoint
--
-- Rollback reverses both: cardano-utxo-csmt reverts
-- its UTxO index via 'rollbackTipApply', and the
-- cage follower replays stored inverse operations.
--
-- Two N2C connections share the same RocksDB instance:
-- one for ChainSync (blocks processed here) and one
-- for LocalStateQuery + LocalTxSubmission.
module Cardano.MPFS.Indexer.CageFollower
    ( -- * Construction
      mkCageFollower
    , mkCageIntersector
    ) where

import Ouroboros.Network.Point
    ( Block (..)
    , WithOrigin (..)
    )

import Cardano.UTxOCSMT.Application.BlockFetch
    ( Fetched (..)
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , Update (..)
    )
import Cardano.UTxOCSMT.Application.Database.Interface qualified as CSMT
import Cardano.UTxOCSMT.Application.UTxOs
    ( Change (..)
    , uTxOs
    )
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Follower (..)
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    )

import Ouroboros.Network.Block qualified as Network

import Cardano.MPFS.Core.Types
    ( BlockId (..)
    , ConwayEra
    , ScriptHash
    , SlotNo (..)
    , TxIn
    , TxOut
    )
import Cardano.MPFS.Indexer.Columns (AllColumns)
import Cardano.MPFS.Indexer.Follower
    ( processCageBlock
    )
import Cardano.MPFS.Indexer.Rollback
    ( rollbackToSlot
    , storeRollback
    )
import Cardano.MPFS.State
    ( Checkpoints (..)
    )
import Cardano.MPFS.State qualified as Cage
import Cardano.MPFS.Trie (TrieManager)
import Data.ByteString.Lazy (LazyByteString)
import Database.KV.Transaction
    ( RunTransaction
    )

-- | UTxO state machine type alias
type UTxOState =
    CSMT.State
        IO
        Point
        LazyByteString
        LazyByteString

-- | Convert a 'Change' to a UTxO 'Operation'.
changeToOp
    :: Change
    -> Operation LazyByteString LazyByteString
changeToOp (Spend k) = Delete k
changeToOp (Create k v) = Insert k v

-- | Extract the slot number from a Cardano 'Point'.
-- Returns 0 for 'Origin'.
pointToSlot :: Point -> SlotNo
pointToSlot (Network.Point Origin) = SlotNo 0
pointToSlot
    (Network.Point (At (Block s _))) = s

-- | Extract the block hash from a Cardano 'Point'.
-- Returns empty bytes for 'Origin'.
pointToBlockId :: Point -> BlockId
pointToBlockId (Network.Point Origin) =
    BlockId mempty
pointToBlockId
    (Network.Point (At (Block _ _))) =
        BlockId mempty

-- | Build an 'Intersector' for the cage follower.
-- On intersection found, produces a 'Follower' that
-- processes both UTxO and cage events for each block.
mkCageIntersector
    :: ScriptHash
    -- ^ Cage script hash for event detection
    -> Cage.State IO
    -- ^ Cage state interface
    -> TrieManager IO
    -- ^ Trie manager for per-token tries
    -> RunTransaction IO cf AllColumns op
    -- ^ Transaction runner for rollback storage
    -> (TxIn -> IO (Maybe (TxOut ConwayEra)))
    -- ^ UTxO resolver for spent inputs
    -> UTxOState
    -- ^ cardano-utxo-csmt state machine
    -> Intersector Fetched
mkCageIntersector
    scriptHash
    cageSt
    tm
    rt
    resolveUtxo
    utxoState =
        Intersector
            { intersectFound = \point -> do
                let f =
                        mkCageFollower
                            scriptHash
                            cageSt
                            tm
                            rt
                            resolveUtxo
                            utxoState
                pure $ f point
            , intersectNotFound = do
                pure
                    ( mkCageIntersector
                        scriptHash
                        cageSt
                        tm
                        rt
                        resolveUtxo
                        utxoState
                    , [Network.Point Origin]
                    )
            }

-- | Build a 'Follower' from the current UTxO state.
mkCageFollower
    :: ScriptHash
    -- ^ Cage script hash for event detection
    -> Cage.State IO
    -- ^ Cage state interface
    -> TrieManager IO
    -- ^ Trie manager for per-token tries
    -> RunTransaction IO cf AllColumns op
    -- ^ Transaction runner for rollback storage
    -> (TxIn -> IO (Maybe (TxOut ConwayEra)))
    -- ^ UTxO resolver for spent inputs
    -> UTxOState
    -- ^ cardano-utxo-csmt state machine
    -> Point
    -- ^ Intersection point
    -> Follower Fetched
mkCageFollower
    scriptHash
    cageSt
    tm
    rt
    resolveUtxo =
        go
      where
        go utxoState _intersectPt =
            follower utxoState

        follower currentState =
            Follower
                { rollForward =
                    rollFwd currentState
                , rollBackward =
                    rollBwd currentState
                }

        rollFwd currentState fetched tipSlot = do
            let Fetched
                    { fetchedPoint
                    , fetchedBlock
                    } = fetched
            -- Step 1: Apply UTxO changes
            let ops =
                    changeToOp
                        <$> uTxOs fetchedBlock
            newState <- case currentState of
                Syncing update -> do
                    newUpdate <-
                        forwardTipApply
                            update
                            fetchedPoint
                            tipSlot
                            ops
                    pure $ Syncing newUpdate
                _ ->
                    error
                        "CageFollower.rollForward:\
                        \ not syncing"

            -- Step 2: Process cage events
            let slot =
                    pointToSlot fetchedPoint
                blockId =
                    pointToBlockId fetchedPoint
            invs <-
                processCageBlock
                    scriptHash
                    cageSt
                    tm
                    resolveUtxo
                    fetchedBlock

            -- Step 3: Store inverse ops
            storeRollback rt slot invs

            -- Step 4: Update cage checkpoint
            mCp <-
                getCheckpoint
                    (Cage.checkpoints cageSt)
            let activeSlots = case mCp of
                    Nothing -> [slot]
                    Just (_, _, slots) ->
                        slot : slots
            putCheckpoint
                (Cage.checkpoints cageSt)
                slot
                blockId
                activeSlots

            pure $ follower newState

        rollBwd currentState point = do
            -- Step 1: Rollback UTxO index
            newState <- case currentState of
                Syncing update ->
                    rollbackTipApply
                        update
                        (At point)
                _ ->
                    error
                        "CageFollower.rollBackward:\
                        \ not syncing"

            -- Step 2: Rollback cage state
            let targetSlot = pointToSlot point
            rollbackToSlot cageSt tm rt targetSlot

            -- Step 3: Return appropriate result
            pure $ case newState of
                Syncing newUpdate ->
                    Progress
                        $ follower
                        $ Syncing newUpdate
                Truncating newUpdate ->
                    Reset
                        $ mkCageIntersector
                            scriptHash
                            cageSt
                            tm
                            rt
                            resolveUtxo
                        $ Syncing newUpdate
                Intersecting ps newUpdate ->
                    Rewind
                        ps
                        $ mkCageIntersector
                            scriptHash
                            cageSt
                            tm
                            rt
                            resolveUtxo
                        $ Syncing newUpdate
