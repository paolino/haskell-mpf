{-# LANGUAGE GADTs #-}

-- |
-- Module      : Cardano.MPFS.NodeClient.Types
-- Description : Type aliases for N2C node client
-- License     : Apache-2.0
--
-- Type aliases and channel types for communicating
-- with the node-to-client Ouroboros protocols:
-- LocalStateQuery and LocalTxSubmission.
module Cardano.MPFS.NodeClient.Types
    ( -- * Block types
      Block
    , BlockPoint

      -- * Channel types
    , LSQChannel (..)
    , LTxSChannel (..)

      -- * Request wrappers
    , SomeLSQQuery (..)
    , TxSubmitRequest (..)
    ) where

import Control.Concurrent.STM (TBQueue, TMVar)

import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( ApplyTxErr
    , GenTx
    )
import Ouroboros.Network.Block qualified as Network

-- | Cardano block type.
type Block =
    Consensus.CardanoBlock
        Consensus.StandardCrypto

-- | Point for the Cardano block.
type BlockPoint = Network.Point Block

-- | Existential wrapper for a query with its result
-- slot, so the protocol loop can serve arbitrary
-- queries without knowing the result type.
data SomeLSQQuery where
    SomeLSQQuery
        :: Query Block result
        -> TMVar result
        -> SomeLSQQuery

-- | Channel for communicating with the
-- LocalStateQuery mini-protocol client.
--
-- Callers enqueue a 'SomeLSQQuery' and then block
-- on the embedded 'TMVar' to receive the result.
newtype LSQChannel = LSQChannel
    { lsqRequests :: TBQueue SomeLSQQuery
    }

-- | A transaction submission request bundled with
-- its response slot.
data TxSubmitRequest = TxSubmitRequest
    { tsrTx :: !(GenTx Block)
    -- ^ The transaction to submit
    , tsrResult
        :: !(TMVar (Either (ApplyTxErr Block) ()))
    -- ^ Where to put the submission result
    }

-- | Channel for communicating with the
-- LocalTxSubmission mini-protocol client.
newtype LTxSChannel = LTxSChannel
    { ltxsRequests :: TBQueue TxSubmitRequest
    }
