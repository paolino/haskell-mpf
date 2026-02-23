module Main (main) where

import Test.Hspec (hspec)

import Cardano.MPFS.E2E.CageSpec qualified as CageSpec
import Cardano.MPFS.E2E.IndexerSpec qualified as IndexerSpec
import Cardano.MPFS.E2E.ProviderSpec qualified as ProviderSpec
import Cardano.MPFS.E2E.SubmitterSpec qualified as SubmitterSpec

main :: IO ()
main = hspec $ do
    ProviderSpec.spec
    SubmitterSpec.spec
    CageSpec.spec
    IndexerSpec.spec
