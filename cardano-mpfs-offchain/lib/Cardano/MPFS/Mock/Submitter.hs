{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.Mock.Submitter
-- Description : No-op mock Submitter implementation
-- License     : Apache-2.0
--
-- Stub implementation of the 'Submitter' interface.
-- Every call to 'submitTx' returns 'Rejected' with
-- a descriptive message. Useful for unit tests that
-- exercise transaction construction without
-- submitting to a real node. See
-- "Cardano.MPFS.Submitter.N2C" for the production
-- implementation.
module Cardano.MPFS.Mock.Submitter
    ( -- * Construction
      mkMockSubmitter
    ) where

import Cardano.MPFS.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )

-- | Create a mock 'Submitter IO'. Rejects all
-- transactions with a descriptive message.
mkMockSubmitter :: Submitter IO
mkMockSubmitter =
    Submitter
        { submitTx = \_ ->
            pure
                $ Rejected
                    "mock submitter: not connected"
        }
