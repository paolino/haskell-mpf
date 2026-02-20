module Main (main) where

import Test.Hspec (hspec)

import Cardano.MPFS.Mock.State
    ( mkMockCheckpoints
    , mkMockRequests
    , mkMockTokens
    )
import Cardano.MPFS.Trie.Pure (mkPureTrie)
import Cardano.MPFS.Trie.PureManager
    ( mkPureTrieManager
    )

import Cardano.MPFS.BalanceSpec qualified as BalanceSpec
import Cardano.MPFS.Indexer.CageEventSpec qualified as CageEventSpec
import Cardano.MPFS.Indexer.CodecsSpec qualified as CodecsSpec
import Cardano.MPFS.Indexer.InverseSpec qualified as InverseSpec
import Cardano.MPFS.OnChainSpec qualified as OnChainSpec
import Cardano.MPFS.ProofSpec qualified as ProofSpec
import Cardano.MPFS.StateSpec qualified as StateSpec
import Cardano.MPFS.TrieManagerSpec qualified as TrieManagerSpec
import Cardano.MPFS.TrieSpec qualified as TrieSpec
import Cardano.MPFS.TxBuilderSpec qualified as TxBuilderSpec

main :: IO ()
main = hspec $ do
    BalanceSpec.spec
    CageEventSpec.spec
    CodecsSpec.spec
    InverseSpec.spec
    TrieSpec.spec mkPureTrie
    TrieManagerSpec.spec mkPureTrieManager
    StateSpec.spec
        mkMockTokens
        mkMockRequests
        mkMockCheckpoints
    ProofSpec.spec
    OnChainSpec.spec
    TxBuilderSpec.spec
