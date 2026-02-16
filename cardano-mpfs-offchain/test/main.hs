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

import Cardano.MPFS.StateSpec qualified as StateSpec
import Cardano.MPFS.TrieManagerSpec qualified as TrieManagerSpec
import Cardano.MPFS.TrieSpec qualified as TrieSpec

main :: IO ()
main = hspec $ do
    TrieSpec.spec mkPureTrie
    TrieManagerSpec.spec mkPureTrieManager
    StateSpec.spec
        mkMockTokens
        mkMockRequests
        mkMockCheckpoints
