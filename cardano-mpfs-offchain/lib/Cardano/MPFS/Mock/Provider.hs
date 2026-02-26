-- |
-- Module      : Cardano.MPFS.Mock.Provider
-- Description : No-op mock Provider implementation
-- License     : Apache-2.0
--
-- Provides a mock 'Provider IO' that returns empty
-- results. Useful for testing components that don't
-- require real chain queries.
module Cardano.MPFS.Mock.Provider
    ( -- * Construction
      mkMockProvider
    ) where

import Cardano.MPFS.Core.Types (ExUnits (..))
import Cardano.MPFS.Provider (Provider (..))

-- | Create a mock 'Provider IO'. Returns empty UTxO
-- sets, default protocol params, and zero execution
-- units.
mkMockProvider :: Provider IO
mkMockProvider =
    Provider
        { queryUTxOs = \_ -> pure []
        , queryProtocolParams =
            error
                "mkMockProvider: queryProtocolParams \
                \not implemented"
        , evaluateTx = \_ ->
            pure (ExUnits 0 0)
        }
