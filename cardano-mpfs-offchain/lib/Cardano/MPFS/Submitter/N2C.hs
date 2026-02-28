-- |
-- Module      : Cardano.MPFS.Submitter.N2C
-- Description : N2C-backed transaction submitter
-- License     : Apache-2.0
--
-- Production implementation of the 'Submitter'
-- interface. Delegates to the @cardano-node-clients@
-- library for N2C LocalTxSubmission, converting the
-- library's 'SubmitResult' to the MPFS variant.
module Cardano.MPFS.Submitter.N2C
    ( -- * Construction
      mkN2CSubmitter
    ) where

import Cardano.Ledger.Api.Tx (txIdTx)

import Cardano.Node.Client.N2C.Submitter qualified as Lib
import Cardano.Node.Client.N2C.Types (LTxSChannel)
import Cardano.Node.Client.Submitter qualified as Lib

import Cardano.MPFS.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )

-- | Create a 'Submitter IO' backed by the N2C
-- LocalTxSubmission protocol.
mkN2CSubmitter
    :: LTxSChannel
    -- ^ LocalTxSubmission channel to the Cardano node
    -> Submitter IO
mkN2CSubmitter ch =
    let libSub = Lib.mkN2CSubmitter ch
    in  Submitter
            { submitTx = \tx -> do
                libResult <-
                    Lib.submitTx libSub tx
                pure $ case libResult of
                    Lib.Submitted _ ->
                        Submitted (txIdTx tx)
                    Lib.Rejected reason ->
                        Rejected reason
            }
