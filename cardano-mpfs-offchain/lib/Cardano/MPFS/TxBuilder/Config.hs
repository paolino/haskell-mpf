-- |
-- Module      : Cardano.MPFS.TxBuilder.Config
-- Description : Configuration for real transaction builders
-- License     : Apache-2.0
module Cardano.MPFS.TxBuilder.Config
    ( -- * Configuration
      CageConfig (..)
    ) where

import Data.ByteString.Short (ShortByteString)

import Cardano.Ledger.BaseTypes (Network)

import Cardano.MPFS.Types (Coin)

-- | Configuration for the cage script transaction
-- builders.
--
-- The 'cageScriptBytes' field holds the raw
-- double-CBOR-encoded PlutusV3 script extracted
-- from the CIP-57 blueprint (@plutus.json@).
data CageConfig = CageConfig
    { cageScriptBytes :: !ShortByteString
    -- ^ Raw PlutusV3 script bytes (from blueprint)
    , processTime :: !Integer
    -- ^ Phase 1 window (ms) for oracle processing
    , retractTime :: !Integer
    -- ^ Phase 2 window (ms) for requester retract
    , defaultMaxFee :: !Coin
    -- ^ Default max fee for newly booted tokens
    , network :: !Network
    -- ^ Target network (Mainnet or Testnet)
    }
