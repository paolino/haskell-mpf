{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Cardano.MPFS.Submitter.N2C
-- Description : N2C-backed transaction submitter
-- License     : Apache-2.0
--
-- Production implementation of the 'Submitter'
-- interface. Sends signed transactions to a local
-- Cardano node via the N2C LocalTxSubmission
-- mini-protocol using an 'LTxSChannel' (see
-- "Cardano.MPFS.NodeClient.Connection").
--
-- The ledger 'Tx ConwayEra' is wrapped into a
-- consensus 'GenTx Block' before submission.
-- Rejection reasons are serialized to 'ByteString'
-- and returned as 'Rejected'.
module Cardano.MPFS.Submitter.N2C
    ( -- * Construction
      mkN2CSubmitter
    ) where

import Data.ByteString.Char8 qualified as B8

import Cardano.Ledger.Api.Tx (Tx, txIdTx)

import Ouroboros.Consensus.Cardano.Block
    ( pattern GenTxConway
    )
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( mkShelleyTx
    )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

import Cardano.MPFS.Core.Types (ConwayEra)
import Cardano.MPFS.NodeClient.LocalTxSubmission
    ( submitTxN2C
    )
import Cardano.MPFS.NodeClient.Types
    ( Block
    , LTxSChannel
    )
import Cardano.MPFS.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( GenTx
    )

-- | Create a 'Submitter IO' backed by the N2C
-- LocalTxSubmission protocol.
mkN2CSubmitter
    :: LTxSChannel
    -- ^ LocalTxSubmission channel to the Cardano node
    -> Submitter IO
mkN2CSubmitter ch =
    Submitter
        { submitTx = \tx -> do
            let genTx = toGenTx tx
            result <- submitTxN2C ch genTx
            pure $ case result of
                Right () ->
                    Submitted (txIdTx tx)
                Left err ->
                    Rejected
                        ( B8.pack
                            (show err)
                        )
        }

-- | Convert a ledger 'Tx ConwayEra' to a consensus
-- 'GenTx Block'.
toGenTx :: Tx ConwayEra -> GenTx Block
toGenTx tx = GenTxConway (mkShelleyTx tx)
