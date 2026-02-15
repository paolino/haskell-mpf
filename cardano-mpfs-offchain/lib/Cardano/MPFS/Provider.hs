-- |
-- Module      : Cardano.MPFS.Provider
-- Description : Blockchain query interface
-- License     : Apache-2.0
module Cardano.MPFS.Provider
    ( -- * Provider interface
      Provider (..)
    ) where

import Data.ByteString (ByteString)

import Cardano.Ledger.Api.Tx.Out (TxOut)

import Cardano.MPFS.Types
    ( Addr
    , ConwayEra
    , ExUnits
    , PParams
    , TxIn
    )

-- | Interface for querying the blockchain.
-- All era-specific types are fixed to 'ConwayEra'.
data Provider m = Provider
    { queryUTxOs
        :: Addr
        -> m [(TxIn, TxOut ConwayEra)]
    -- ^ Look up UTxOs at an address
    , queryProtocolParams
        :: m (PParams ConwayEra)
    -- ^ Fetch current protocol parameters
    , evaluateTx
        :: ByteString -> m ExUnits
    -- ^ Evaluate execution units for a serialised
    -- CBOR transaction
    }
