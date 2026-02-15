-- |
-- Module      : Cardano.MPFS.Indexer
-- Description : Chain sync follower interface
-- License     : Apache-2.0
module Cardano.MPFS.Indexer
    ( -- * Indexer interface
      Indexer (..)

      -- * Chain tip
    , ChainTip (..)
    ) where

import Cardano.MPFS.Types (BlockId, SlotNo)

-- | Current chain tip information.
data ChainTip = ChainTip
    { tipSlot :: !SlotNo
    -- ^ Slot number at the tip
    , tipBlockId :: !BlockId
    -- ^ Block hash at the tip
    }

-- | Interface for the chain sync indexer that
-- follows the blockchain and updates local state.
data Indexer m = Indexer
    { start :: m ()
    -- ^ Start the indexer
    , stop :: m ()
    -- ^ Stop the indexer
    , pause :: m ()
    -- ^ Pause indexing
    , resume :: m ()
    -- ^ Resume indexing after a pause
    , getTip :: m ChainTip
    -- ^ Get the current chain tip
    }
