-- |
-- Module      : Cardano.MPFS.TxBuilder.Real
-- Description : Real transaction builders for the MPFS cage
-- License     : Apache-2.0
--
-- Builds unsigned Cardano transactions for the MPFS
-- cage protocol: minting tokens, submitting requests,
-- retracting requests, and processing updates.
--
-- Re-exports from per-operation submodules.
module Cardano.MPFS.TxBuilder.Real
    ( -- * Construction
      mkRealTxBuilder

      -- * Script hash
    , computeScriptHash

      -- * Internals (for testing)
    , mkInlineDatum
    , toPlcData
    , extractCageDatum
    ) where

import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State (State (..))
import Cardano.MPFS.Trie (TrieManager (..))
import Cardano.MPFS.TxBuilder (TxBuilder (..))
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig
    )
import Cardano.MPFS.TxBuilder.Real.Boot
    ( bootTokenImpl
    )
import Cardano.MPFS.TxBuilder.Real.End
    ( endTokenImpl
    )
import Cardano.MPFS.TxBuilder.Real.Internal
    ( computeScriptHash
    , extractCageDatum
    , mkInlineDatum
    , toPlcData
    )
import Cardano.MPFS.TxBuilder.Real.Request
    ( requestDeleteImpl
    , requestInsertImpl
    )
import Cardano.MPFS.TxBuilder.Real.Retract
    ( retractRequestImpl
    )
import Cardano.MPFS.TxBuilder.Real.Update
    ( updateTokenImpl
    )

-- | Create a real 'TxBuilder IO' wired to a
-- 'Provider', 'State', and 'TrieManager'.
mkRealTxBuilder
    :: CageConfig
    -> Provider IO
    -> State IO
    -> TrieManager IO
    -> TxBuilder IO
mkRealTxBuilder cfg prov st tm =
    TxBuilder
        { bootToken = bootTokenImpl cfg prov
        , requestInsert =
            requestInsertImpl cfg prov st
        , requestDelete =
            requestDeleteImpl cfg prov st
        , updateToken =
            updateTokenImpl cfg prov st tm
        , retractRequest =
            retractRequestImpl cfg prov st
        , endToken = endTokenImpl cfg prov
        }
