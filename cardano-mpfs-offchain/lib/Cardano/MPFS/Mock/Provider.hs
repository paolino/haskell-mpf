-- |
-- Module      : Cardano.MPFS.Mock.Provider
-- Description : No-op mock Provider implementation
-- License     : Apache-2.0
--
-- Stub implementation of the 'Provider' interface
-- for unit tests and wiring checks. Returns empty
-- UTxO sets and zero execution units.
-- 'queryProtocolParams' throws — tests that need
-- real params should use a custom fixture instead.
-- See "Cardano.MPFS.Provider.NodeClient" for the
-- production implementation.
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
