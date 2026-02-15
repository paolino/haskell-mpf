-- |
-- Module      : Cardano.MPFS.State
-- Description : Token and request state tracking interface
-- License     : Apache-2.0
module Cardano.MPFS.State
    ( -- * Combined state
      State (..)

      -- * Token state
    , Tokens (..)

      -- * Request state
    , Requests (..)

      -- * Checkpoint state
    , Checkpoints (..)
    ) where

import Cardano.MPFS.Types
    ( BlockId
    , Request
    , SlotNo
    , TokenId
    , TokenState
    , TxIn
    )

-- | Combined state interface bundling token,
-- request, and checkpoint tracking.
data State m = State
    { tokens :: Tokens m
    -- ^ Token state operations
    , requests :: Requests m
    -- ^ Request state operations
    , checkpoints :: Checkpoints m
    -- ^ Checkpoint operations
    }

-- | Interface for managing token state.
data Tokens m = Tokens
    { getToken :: TokenId -> m (Maybe TokenState)
    -- ^ Look up a token's current state
    , putToken :: TokenId -> TokenState -> m ()
    -- ^ Store or update a token's state
    , removeToken :: TokenId -> m ()
    -- ^ Remove a token
    , listTokens :: m [TokenId]
    -- ^ List all known tokens
    }

-- | Interface for managing pending requests.
data Requests m = Requests
    { getRequest
        :: TxIn -> m (Maybe Request)
    -- ^ Look up a request by its UTxO reference
    , putRequest
        :: TxIn -> Request -> m ()
    -- ^ Store a new request
    , removeRequest :: TxIn -> m ()
    -- ^ Remove a fulfilled or retracted request
    , requestsByToken
        :: TokenId -> m [Request]
    -- ^ List all pending requests for a token
    }

-- | Interface for chain sync checkpoints.
data Checkpoints m = Checkpoints
    { getCheckpoint
        :: m (Maybe (SlotNo, BlockId))
    -- ^ Get the last processed checkpoint
    , putCheckpoint
        :: SlotNo -> BlockId -> m ()
    -- ^ Store a new checkpoint
    }
