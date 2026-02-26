-- |
-- Module      : Cardano.MPFS.Mock.Skeleton
-- Description : Skeleton Indexer with no-op chain sync
-- License     : Apache-2.0
--
-- Provides a skeleton 'Indexer IO' with lifecycle
-- management (start\/stop\/pause\/resume) backed by
-- 'IORef' and 'MVar'. Returns a genesis tip.
-- Placeholder until csmt chain sync is wired.
module Cardano.MPFS.Mock.Skeleton
    ( -- * Construction
      mkSkeletonIndexer
    ) where

import Control.Concurrent.MVar
    ( newMVar
    , putMVar
    , takeMVar
    )
import Data.ByteString qualified as B
import Data.IORef
    ( newIORef
    , writeIORef
    )

import Cardano.MPFS.Core.Types (BlockId (..), SlotNo (..))
import Cardano.MPFS.Indexer (ChainTip (..), Indexer (..))

-- | Create a skeleton 'Indexer IO'. Lifecycle
-- operations are tracked via 'IORef' and 'MVar'
-- but no chain sync runs. 'getTip' returns genesis.
mkSkeletonIndexer :: IO (Indexer IO)
mkSkeletonIndexer = do
    running <- newIORef False
    pauseVar <- newMVar ()
    pure
        Indexer
            { start = writeIORef running True
            , stop = writeIORef running False
            , pause = takeMVar pauseVar
            , resume = putMVar pauseVar ()
            , getTip =
                pure
                    ChainTip
                        { tipSlot = SlotNo 0
                        , tipBlockId =
                            BlockId (B.replicate 32 0)
                        }
            }
