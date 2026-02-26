-- |
-- Module      : Cardano.MPFS.Mock.Indexer
-- Description : No-op mock Indexer implementation
-- License     : Apache-2.0
--
-- Provides a mock 'Indexer IO' where start\/stop are
-- no-ops and 'getTip' returns a genesis-like tip.
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
