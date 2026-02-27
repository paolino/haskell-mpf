{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Main
-- Description : Generate CBOR bootstrap from genesis
-- License     : Apache-2.0
--
-- CLI tool that parses a @shelley-genesis.json@ file,
-- extracts @initialFunds@, and writes a CBOR bootstrap
-- file suitable for fresh database seeding.
module Main (main) where

import Options.Applicative

import Cardano.MPFS.Core.Bootstrap.Genesis
    ( generateBootstrapFile
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
    Opts{..} <- execParser optsParser
    generateBootstrapFile genesisFile outputFile
