-- |
-- Module      : Cardano.MPFS.NodeClient.LocalTxSubmission
-- Description : LocalTxSubmission protocol client
-- License     : Apache-2.0
--
-- Channel-driven LocalTxSubmission client. Reads
-- 'TxSubmitRequest' values from the 'LTxSChannel'
-- queue, submits each to the node, and delivers
-- the accept\/reject result via the request's 'TMVar'.
module Cardano.MPFS.NodeClient.LocalTxSubmission
    ( -- * Client construction
      mkLocalTxSubmissionClient

      -- * Submit helper
    , submitTxN2C
    ) where

import Cardano.MPFS.NodeClient.Types
    ( Block
    , LTxSChannel (..)
    , TxSubmitRequest (..)
    )
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVarIO
    , putTMVar
    , readTBQueue
    , takeTMVar
    , writeTBQueue
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( ApplyTxErr
    , GenTx
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..)
    , LocalTxSubmissionClient (..)
    , SubmitResult (..)
    )

-- | Build a 'LocalTxSubmissionClient' driven by the
-- given channel. The client loops: wait for a tx,
-- submit, deliver result, repeat.
mkLocalTxSubmissionClient
    :: LTxSChannel
    -> LocalTxSubmissionClient
        (GenTx Block)
        (ApplyTxErr Block)
        IO
        ()
mkLocalTxSubmissionClient ch =
    LocalTxSubmissionClient $ clientIdle ch

-- | Idle state: wait for a request, submit it.
clientIdle
    :: LTxSChannel
    -> IO
        ( LocalTxClientStIdle
            (GenTx Block)
            (ApplyTxErr Block)
            IO
            ()
        )
clientIdle ch = do
    TxSubmitRequest{tsrTx, tsrResult} <-
        atomically $ readTBQueue (ltxsRequests ch)
    pure
        $ SendMsgSubmitTx tsrTx
        $ \submitResult -> do
            let result = case submitResult of
                    SubmitSuccess -> Right ()
                    SubmitFail reason -> Left reason
            atomically $ putTMVar tsrResult result
            clientIdle ch

-- | Submit a transaction through the channel and
-- block until the result is available.
submitTxN2C
    :: LTxSChannel
    -- ^ Channel to the LocalTxSubmission client
    -> GenTx Block
    -- ^ Generalized transaction to submit
    -> IO (Either (ApplyTxErr Block) ())
submitTxN2C ch tx = do
    resultVar <- newEmptyTMVarIO
    atomically
        $ writeTBQueue (ltxsRequests ch)
        $ TxSubmitRequest
            { tsrTx = tx
            , tsrResult = resultVar
            }
    atomically $ takeTMVar resultVar
