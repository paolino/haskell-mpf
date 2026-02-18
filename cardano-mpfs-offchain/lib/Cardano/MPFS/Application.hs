-- |
-- Module      : Cardano.MPFS.Application
-- Description : Application wiring and lifecycle
-- License     : Apache-2.0
--
-- Wires all service interfaces into a 'Context IO'
-- backed by a real N2C node connection, with mock
-- state and trie management.
module Cardano.MPFS.Application
    ( -- * Configuration
      AppConfig (..)

      -- * Lifecycle
    , withApplication
    ) where

import Control.Concurrent.Async (async, cancel)

import Ouroboros.Network.Magic (NetworkMagic)

import Cardano.MPFS.Context (Context (..))
import Cardano.MPFS.Indexer.Skeleton
    ( mkSkeletonIndexer
    )
import Cardano.MPFS.Mock.State (mkMockState)
import Cardano.MPFS.NodeClient.Connection
    ( newLSQChannel
    , newLTxSChannel
    , runNodeClient
    )
import Cardano.MPFS.Provider.NodeClient
    ( mkNodeClientProvider
    )
import Cardano.MPFS.Submitter.N2C (mkN2CSubmitter)
import Cardano.MPFS.Trie.PureManager
    ( mkPureTrieManager
    )
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig
    )
import Cardano.MPFS.TxBuilder.Real
    ( mkRealTxBuilder
    )

-- | Application configuration.
data AppConfig = AppConfig
    { networkMagic :: !NetworkMagic
    -- ^ Network magic (e.g. mainnet, preview)
    , socketPath :: !FilePath
    -- ^ Path to the cardano-node Unix socket
    , channelCapacity :: !Int
    -- ^ TBQueue capacity for N2C channels
    , cageConfig :: !CageConfig
    -- ^ Cage script and protocol parameters
    }

-- | Run an action with a fully wired 'Context IO'.
--
-- Bracket pattern: opens an N2C connection in the
-- background, wires real Provider and Submitter,
-- and tears down on exit.
withApplication
    :: AppConfig -> (Context IO -> IO a) -> IO a
withApplication cfg action = do
    lsqCh <- newLSQChannel (channelCapacity cfg)
    ltxsCh <- newLTxSChannel (channelCapacity cfg)
    nodeThread <-
        async
            $ runNodeClient
                (networkMagic cfg)
                (socketPath cfg)
                lsqCh
                ltxsCh
    st <- mkMockState
    tm <- mkPureTrieManager
    idx <- mkSkeletonIndexer
    let prov = mkNodeClientProvider lsqCh
        ctx =
            Context
                { provider = prov
                , submitter =
                    mkN2CSubmitter ltxsCh
                , state = st
                , trieManager = tm
                , txBuilder =
                    mkRealTxBuilder
                        (cageConfig cfg)
                        prov
                        st
                        tm
                , indexer = idx
                }
    result <- action ctx
    cancel nodeThread
    pure result
