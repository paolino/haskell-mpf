{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.E2E.ProviderSpec
-- Description : E2E tests for the N2C Provider
-- License     : Apache-2.0
module Cardano.MPFS.E2E.ProviderSpec (spec) where

import Lens.Micro ((^.))
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldSatisfy
    )

import Cardano.Ledger.Api.PParams (ppMaxTxSizeL)

import Cardano.MPFS.E2E.Setup
    ( genesisAddr
    , withDevnet
    )
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.Provider.NodeClient
    ( mkNodeClientProvider
    )

spec :: Spec
spec =
    around withDevnet'
        $ describe "Provider.NodeClient"
        $ do
            it "queryProtocolParams returns valid PParams"
                $ \(lsq, _) -> do
                    let provider =
                            mkNodeClientProvider lsq
                    pp <-
                        queryProtocolParams provider
                    let maxTxSize =
                            pp ^. ppMaxTxSizeL
                    maxTxSize
                        `shouldSatisfy` (> 0)

            it "queryUTxOs returns genesis funds"
                $ \(lsq, _) -> do
                    let provider =
                            mkNodeClientProvider lsq
                    utxos <-
                        queryUTxOs
                            provider
                            genesisAddr
                    utxos
                        `shouldSatisfy` (not . null)
  where
    withDevnet' action =
        withDevnet $ curry action
