{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Cardano.MPFS.Core.Bootstrap.Genesis
-- Description : Generate bootstrap from Shelley genesis
-- License     : Apache-2.0
--
-- Parse a @shelley-genesis.json@ file and produce a
-- CBOR bootstrap file suitable for seeding a fresh
-- database with the initial UTxO set.
module Cardano.MPFS.Core.Bootstrap.Genesis
    ( -- * Bootstrap generation
      generateBootstrapFile

      -- * Genesis parsing
    , readShelleyGenesis
    , genesisUtxoPairs
    ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ListMap (unListMap)

import Cardano.Ledger.Api.Tx.Out (mkBasicTxOut)
import Cardano.Ledger.Binary
    ( natVersion
    , serialize
    )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.Genesis
    ( ShelleyGenesis (..)
    , initialFundsPseudoTxIn
    )
import Cardano.Ledger.Val (inject)

import Cardano.MPFS.Core.Bootstrap
    ( BootstrapHeader (..)
    , encodeBootstrapFile
    )

-- | Generate a CBOR bootstrap file from a Shelley
-- genesis JSON file. The bootstrap has slot 0 and
-- no block hash (genesis is before any block).
generateBootstrapFile
    :: FilePath
    -- ^ Path to @shelley-genesis.json@
    -> FilePath
    -- ^ Output CBOR bootstrap file
    -> IO ()
generateBootstrapFile genesisPath outputPath = do
    genesis <- readShelleyGenesis genesisPath
    let pairs = genesisUtxoPairs genesis
        hdr = BootstrapHeader 0 Nothing
    encodeBootstrapFile outputPath hdr pairs

-- | Extract initial funds from a 'ShelleyGenesis'
-- as CBOR-serialized key-value pairs suitable for
-- CSMT insertion.
genesisUtxoPairs
    :: ShelleyGenesis
    -> [(ByteString, ByteString)]
genesisUtxoPairs genesis =
    map mkPair (fundsList genesis)
  where
    ver = natVersion @11
    mkPair (addr, coin) =
        let txIn =
                initialFundsPseudoTxIn addr
            txOut =
                mkBasicTxOut @ConwayEra
                    addr
                    (inject coin :: MaryValue)
        in  ( BSL.toStrict
                (serialize ver txIn)
            , BSL.toStrict
                (serialize ver txOut)
            )
    fundsList =
        unListMap . sgInitialFunds

-- | Read and parse a Shelley genesis JSON file.
-- Patches @systemStart@ if set to
-- @\"PLACEHOLDER\"@ (common in devnet configs).
readShelleyGenesis
    :: FilePath -> IO ShelleyGenesis
readShelleyGenesis fp = do
    raw <- BSL.readFile fp
    val <- case Aeson.eitherDecode' raw of
        Left err ->
            error
                $ "JSON parse error: " <> err
        Right v -> pure (patchSystemStart v)
    case Aeson.fromJSON val of
        Aeson.Error err ->
            error
                $ "Genesis decode: " <> err
        Aeson.Success g -> pure g

-- | Replace @\"PLACEHOLDER\"@ systemStart with a
-- dummy UTC time so that 'FromJSON' succeeds.
patchSystemStart
    :: Aeson.Value -> Aeson.Value
patchSystemStart = \case
    Aeson.Object obj
        | Just (Aeson.String "PLACEHOLDER") <-
            KM.lookup "systemStart" obj ->
            Aeson.Object
                $ KM.insert
                    "systemStart"
                    ( Aeson.String
                        "2020-01-01T00:00:00Z"
                    )
                    obj
    v -> v
