{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Cardano.MPFS.Provider.NodeClient
-- Description : N2C-backed Provider via LocalStateQuery
-- License     : Apache-2.0
--
-- Production implementation of the 'Provider'
-- interface. Queries a local Cardano node via the
-- N2C LocalStateQuery mini-protocol using an
-- 'LSQChannel' (see "Cardano.MPFS.NodeClient.Connection").
--
-- Protocol parameters and UTxOs are retrieved with
-- Conway-era queries. An era mismatch (node not yet
-- in Conway) results in a runtime error.
module Cardano.MPFS.Provider.NodeClient
    ( -- * Construction
      mkNodeClientProvider
    ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Cardano.Ledger.State (UTxO (..))

import Ouroboros.Consensus.Cardano.Block
    ( pattern QueryIfCurrentConway
    )
import Ouroboros.Consensus.Ledger.Query
    ( Query (BlockQuery)
    )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( pattern GetCurrentPParams
    , pattern GetUTxOByAddress
    )

import Cardano.MPFS.NodeClient.LocalStateQuery
    ( queryLSQ
    )
import Cardano.MPFS.NodeClient.Types (LSQChannel)
import Cardano.MPFS.Provider (Provider (..))

-- | Create a 'Provider IO' backed by the N2C
-- LocalStateQuery protocol.
mkNodeClientProvider
    :: LSQChannel
    -- ^ LocalStateQuery channel to the Cardano node
    -> Provider IO
mkNodeClientProvider ch =
    Provider
        { queryProtocolParams = do
            result <-
                queryLSQ ch
                    $ BlockQuery
                    $ QueryIfCurrentConway
                        GetCurrentPParams
            case result of
                Right pp -> pure pp
                Left _mismatch ->
                    error
                        "queryProtocolParams: era \
                        \mismatch — node not in Conway"
        , queryUTxOs = \addr -> do
            result <-
                queryLSQ ch
                    $ BlockQuery
                    $ QueryIfCurrentConway
                    $ GetUTxOByAddress
                        (Set.singleton addr)
            case result of
                Right utxo ->
                    pure
                        $ Map.toList
                        $ unUTxO utxo
                Left _mismatch ->
                    error
                        "queryUTxOs: era mismatch \
                        \— node not in Conway"
        , evaluateTx = \_ ->
            error
                "evaluateTx: requires local Plutus \
                \evaluator"
        }
