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
import Cardano.Ledger.Hashes (ScriptHash)

import Cardano.MPFS.Types (Coin)

-- | Configuration for the cage script transaction
-- builders.
--
-- The 'cageScriptBytes' field holds the raw
-- flat-encoded UPLC script (after parameter
-- application). The 'cfgScriptHash' is the
-- hash of the deserialized script.
data CageConfig = CageConfig
    { cageScriptBytes :: !ShortByteString
    -- ^ PlutusV3 script bytes (applied parameters)
    , cfgScriptHash :: !ScriptHash
    -- ^ Hash of the PlutusV3 script
    , defaultProcessTime :: !Integer
    -- ^ Phase 1 window (ms) for oracle processing
    , defaultRetractTime :: !Integer
    -- ^ Phase 2 window (ms) for requester retract
    , defaultMaxFee :: !Coin
    -- ^ Default max fee for newly booted tokens
    , network :: !Network
    -- ^ Target network (Mainnet or Testnet)
    , systemStartPosixMs :: !Integer
    -- ^ Genesis system start as POSIX time (ms)
    , slotLengthMs :: !Integer
    -- ^ Slot length in milliseconds (e.g. 100)
    }
