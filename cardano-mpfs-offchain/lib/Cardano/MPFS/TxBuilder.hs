-- |
-- Module      : Cardano.MPFS.TxBuilder
-- Description : Transaction construction interface
-- License     : Apache-2.0
module Cardano.MPFS.TxBuilder
    ( -- * Transaction builder interface
      TxBuilder (..)
    ) where

import Data.ByteString (ByteString)

import Cardano.Ledger.Api.Tx (Tx)

import Cardano.MPFS.Types
    ( Addr
    , ConwayEra
    , TokenId
    , TxIn
    )

-- | Interface for constructing transactions for
-- all MPFS protocol operations. Returns full
-- ledger 'Tx' values ready for signing.
data TxBuilder m = TxBuilder
    { bootToken
        :: Addr -> m (Tx ConwayEra)
    -- ^ Create a new MPFS token
    , requestInsert
        :: TokenId
        -> ByteString
        -> ByteString
        -> Addr
        -> m (Tx ConwayEra)
    -- ^ Request inserting a key-value pair
    , requestDelete
        :: TokenId
        -> ByteString
        -> Addr
        -> m (Tx ConwayEra)
    -- ^ Request deleting a key
    , updateToken
        :: TokenId
        -> Addr
        -> m (Tx ConwayEra)
    -- ^ Process pending requests for a token
    , retractRequest
        :: TxIn
        -> Addr
        -> m (Tx ConwayEra)
    -- ^ Cancel a pending request
    , endToken
        :: TokenId
        -> Addr
        -> m (Tx ConwayEra)
    -- ^ Retire an MPFS token
    }
