-- |
-- Module      : Cardano.MPFS.Mock.Indexer
-- Description : No-op mock Indexer implementation
-- License     : Apache-2.0
--
-- Stub implementation of the 'Indexer' interface
-- where all lifecycle operations (start, stop,
-- pause, resume) are no-ops and 'getTip' returns
-- slot 0 with a null block id. Useful for tests
-- that don't need chain-sync indexing. See
-- "Cardano.MPFS.Mock.Skeleton" for a slightly
-- richer skeleton with lifecycle tracking.
module Cardano.MPFS.Mock.Indexer
    ( -- * Construction
      mkMockIndexer
    ) where

import Data.ByteString qualified as B

import Cardano.MPFS.Core.Types (BlockId (..), SlotNo (..))
import Cardano.MPFS.Indexer (ChainTip (..), Indexer (..))

-- | Create a mock 'Indexer IO'. All lifecycle
-- operations are no-ops. 'getTip' returns slot 0
-- with a null block id.
mkMockIndexer :: Indexer IO
mkMockIndexer =
    Indexer
        { start = pure ()
        , stop = pure ()
        , pause = pure ()
        , resume = pure ()
        , getTip =
            pure
                ChainTip
                    { tipSlot = SlotNo 0
                    , tipBlockId =
                        BlockId (B.replicate 32 0)
                    }
        }
