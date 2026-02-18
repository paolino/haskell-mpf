-- |
-- Module      : Cardano.MPFS.NodeClient.LocalStateQuery
-- Description : LocalStateQuery protocol client
-- License     : Apache-2.0
--
-- A channel-driven LocalStateQuery client that
-- acquires the volatile tip, serves queries from
-- a 'TBQueue', then releases and loops.
module Cardano.MPFS.NodeClient.LocalStateQuery
    ( -- * Client construction
      mkLocalStateQueryClient

      -- * Query helpers
    , queryLSQ
    ) where

import Cardano.MPFS.NodeClient.Types
    ( Block
    , BlockPoint
    , LSQChannel (..)
    , SomeLSQQuery (..)
    )
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVar
    , putTMVar
    , readTBQueue
    , takeTMVar
    , tryReadTBQueue
    , writeTBQueue
    )
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( ClientStAcquired (..)
    , ClientStAcquiring (..)
    , ClientStIdle (..)
    , ClientStQuerying (..)
    , LocalStateQueryClient (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( Target (..)
    )

-- | Build a 'LocalStateQueryClient' driven by the
-- given channel. The client loops: acquire tip,
-- drain the queue, release, repeat.
mkLocalStateQueryClient
    :: LSQChannel
    -> LocalStateQueryClient
        Block
        BlockPoint
        (Query Block)
        IO
        ()
mkLocalStateQueryClient ch =
    LocalStateQueryClient $ pure $ clientIdle ch

-- | Idle state: wait for a query, then acquire.
clientIdle
    :: LSQChannel
    -> ClientStIdle
        Block
        BlockPoint
        (Query Block)
        IO
        ()
clientIdle ch =
    SendMsgAcquire
        VolatileTip
        ClientStAcquiring
            { recvMsgAcquired = clientAcquired ch
            , recvMsgFailure = \_failure ->
                pure $ clientIdle ch
            }

-- | Acquired state: drain and serve queries.
clientAcquired
    :: LSQChannel
    -> IO
        ( ClientStAcquired
            Block
            BlockPoint
            (Query Block)
            IO
            ()
        )
clientAcquired ch = do
    -- Block until at least one query arrives
    req <- atomically $ readTBQueue (lsqRequests ch)
    serveQuery ch req

-- | Serve a single query, then check for more.
serveQuery
    :: LSQChannel
    -> SomeLSQQuery
    -> IO
        ( ClientStAcquired
            Block
            BlockPoint
            (Query Block)
            IO
            ()
        )
serveQuery ch (SomeLSQQuery query resultVar) =
    pure
        $ SendMsgQuery
            query
            ClientStQuerying
                { recvMsgResult = \result -> do
                    atomically
                        $ putTMVar resultVar result
                    -- Try to drain more queries
                    -- before releasing
                    mNext <-
                        atomically
                            $ tryReadTBQueue
                                (lsqRequests ch)
                    case mNext of
                        Just next ->
                            serveQuery ch next
                        Nothing ->
                            -- No more; release
                            pure
                                $ SendMsgRelease
                                $ pure
                                $ clientIdle ch
                }

-- | Submit a query through the channel and block
-- until the result is available.
queryLSQ
    :: LSQChannel
    -> Query Block result
    -> IO result
queryLSQ ch query = do
    resultVar <- atomically newEmptyTMVar
    atomically
        $ writeTBQueue (lsqRequests ch)
        $ SomeLSQQuery query resultVar
    atomically $ takeTMVar resultVar
