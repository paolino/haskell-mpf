-- |
-- Module      : Cardano.MPFS.Indexer.Rollback
-- Description : Rollback to a previous slot
-- License     : Apache-2.0
--
-- Slot-based rollback for the cage indexer. Inverse
-- operations are stored per-slot by 'storeRollback'
-- and replayed in reverse order by 'rollbackToSlot'
-- when ChainSync reports a rollback event. The set
-- of active slots is tracked in the checkpoint (see
-- "Cardano.MPFS.Indexer.Columns"), bounded by the
-- security parameter (k ≈ 2160).
module Cardano.MPFS.Indexer.Rollback
    ( -- * Rollback
      rollbackToSlot

      -- * Rollback storage
    , storeRollback
    , loadRollback
    , deleteRollback
    ) where

import Control.Monad (forM_)

import Cardano.MPFS.Core.Types (BlockId (..), SlotNo)
import Cardano.MPFS.Indexer.Columns
    ( AllColumns (..)
    , CageRollbackEntry (..)
    )
import Cardano.MPFS.Indexer.Event
    ( CageInverseOp
    )
import Cardano.MPFS.Indexer.Follower
    ( applyCageInverses
    )
import Cardano.MPFS.State
    ( Checkpoints (..)
    , State (..)
    )
import Cardano.MPFS.Trie (TrieManager)
import Database.KV.Transaction
    ( RunTransaction (..)
    , delete
    , insert
    , query
    )

-- | Store inverse ops for a block at a given slot.
storeRollback
    :: RunTransaction IO cf AllColumns op
    -- ^ Transaction runner
    -> SlotNo
    -- ^ Slot of the block
    -> [CageInverseOp]
    -- ^ Inverse operations to store
    -> IO ()
storeRollback RunTransaction{runTransaction = run} slot invs =
    run
        $ insert
            CageRollbacks
            slot
            (CageRollbackEntry invs)

-- | Load inverse ops for a given slot.
loadRollback
    :: RunTransaction IO cf AllColumns op
    -- ^ Transaction runner
    -> SlotNo
    -- ^ Slot to load ops for
    -> IO (Maybe [CageInverseOp])
loadRollback RunTransaction{runTransaction = run} slot =
    run $ do
        mEntry <- query CageRollbacks slot
        pure $ fmap unRollbackEntry mEntry

-- | Delete stored inverse ops for a given slot.
deleteRollback
    :: RunTransaction IO cf AllColumns op
    -- ^ Transaction runner
    -> SlotNo
    -- ^ Slot whose ops to delete
    -> IO ()
deleteRollback RunTransaction{runTransaction = run} slot =
    run $ delete CageRollbacks slot

-- | Roll back cage state to a target slot by replaying
-- inverse operations for all slots after the target.
-- Processes slots in reverse order (most recent first).
-- Updates the checkpoint to reflect the new position.
rollbackToSlot
    :: State IO
    -> TrieManager IO
    -> RunTransaction IO cf AllColumns op
    -> SlotNo
    -- ^ Target slot to roll back to
    -> IO ()
rollbackToSlot st tm rt targetSlot = do
    mCp <- getCheckpoint (checkpoints st)
    case mCp of
        Nothing -> pure ()
        Just (_currentSlot, _blockId, activeSlots) ->
            do
                let slotsToRevert =
                        reverse
                            $ filter (> targetSlot) activeSlots
                forM_ slotsToRevert $ \slot -> do
                    mInvs <- loadRollback rt slot
                    forM_ mInvs $ \invs ->
                        applyCageInverses
                            st
                            tm
                            (reverse invs)
                    deleteRollback rt slot
                -- Update checkpoint
                let keptSlots =
                        filter (<= targetSlot) activeSlots
                    emptyBlockId =
                        BlockId mempty
                putCheckpoint
                    (checkpoints st)
                    targetSlot
                    emptyBlockId
                    keptSlots
