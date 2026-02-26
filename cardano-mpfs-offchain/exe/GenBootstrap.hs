{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Main
-- Description : Generate CBOR bootstrap from genesis
-- License     : Apache-2.0
--
-- Parses a @shelley-genesis.json@ file, extracts
-- @initialFunds@, and writes a CBOR bootstrap file
-- suitable for fresh database seeding.
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.ListMap (unListMap)
import Options.Applicative

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Api.Tx.Out (mkBasicTxOut)
import Cardano.Ledger.Binary
    ( natVersion
    , serialize
    )
import Cardano.Ledger.Coin (Coin)
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

-- | CLI options.
data Opts = Opts
    { genesisFile :: !FilePath
    , outputFile :: !FilePath
    }

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> parser)
        ( fullDesc
            <> progDesc
                "Generate a CBOR bootstrap \
                \file from Shelley genesis JSON"
        )
  where
    parser =
        Opts
            <$> strOption
                ( long "genesis"
                    <> metavar "FILE"
                    <> help
                        "Path to \
                        \shelley-genesis.json"
                )
            <*> strOption
                ( long "output"
                    <> metavar "FILE"
                    <> help
                        "Output CBOR bootstrap \
                        \file"
                )

main :: IO ()
main = do
    Opts{genesisFile, outputFile} <-
        execParser optsParser
    genesis <- readGenesis genesisFile
    let ver = natVersion @11
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
        pairs = map mkPair (fundsList genesis)
        hdr = BootstrapHeader 0 Nothing
    encodeBootstrapFile outputFile hdr pairs
    putStrLn
        $ "Wrote "
            <> show (length pairs)
            <> " UTxO entries to "
            <> outputFile

-- | Extract initial funds as a list of pairs.
fundsList :: ShelleyGenesis -> [(Addr, Coin)]
fundsList = unListMap . sgInitialFunds

-- | Read and parse a Shelley genesis JSON file.
-- Patches @systemStart@ if set to @\"PLACEHOLDER\"@.
readGenesis :: FilePath -> IO ShelleyGenesis
readGenesis fp = do
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
patchSystemStart :: Aeson.Value -> Aeson.Value
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
