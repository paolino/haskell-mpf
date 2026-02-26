-- |
-- Module      : Cardano.MPFS.Mock.Skeleton
-- Description : Skeleton Indexer with no-op chain sync
-- License     : Apache-2.0
--
-- Skeleton implementation of the 'Indexer' interface
-- that tracks lifecycle state via 'IORef' (running
-- flag) and 'MVar' (pause/resume gate) but does not
-- perform any actual chain synchronization.
-- 'getTip' always returns genesis (slot 0).
--
-- Used by 'withApplication' as a temporary
-- placeholder until the real chain-sync indexer
-- (see "Cardano.MPFS.Indexer.Follower") is wired.
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
