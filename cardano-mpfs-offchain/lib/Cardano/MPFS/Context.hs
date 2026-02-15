-- |
-- Module      : Cardano.MPFS.Context
-- Description : Facade bundling all singleton interfaces
-- License     : Apache-2.0
module Cardano.MPFS.Context
    ( -- * Context
      Context (..)
    ) where

import Cardano.MPFS.Indexer (Indexer)
import Cardano.MPFS.Provider (Provider)
import Cardano.MPFS.State (State)
import Cardano.MPFS.Submitter (Submitter)
import Cardano.MPFS.Trie (TrieManager)
import Cardano.MPFS.TxBuilder (TxBuilder)

-- | Top-level context bundling all service
-- interfaces. Parametric in the effect @m@.
data Context m = Context
    { provider :: Provider m
    -- ^ Blockchain query operations
    , trieManager :: TrieManager m
    -- ^ Per-token trie management
    , state :: State m
    -- ^ Token and request state tracking
    , indexer :: Indexer m
    -- ^ Chain sync follower
    , submitter :: Submitter m
    -- ^ Transaction submission
    , txBuilder :: TxBuilder m
    -- ^ Transaction construction
    }
