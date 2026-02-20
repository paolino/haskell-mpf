{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Cardano.MPFS.Indexer.CageEvent
-- Description : Cage event detection from blocks
-- License     : Apache-2.0
--
-- Extracts cage-protocol events (boot, request, update,
-- retract, burn) from Cardano blocks by inspecting
-- mints, outputs, and redeemers at the cage script
-- address and policy ID.
module Cardano.MPFS.Indexer.CageEvent
    ( -- * Event type
      CageEvent (..)

      -- * Inverse operations (for rollback)
    , CageInverseOp (..)

      -- * Detection
    , detectCageEvents
    , inversesOf
    ) where

import Cardano.MPFS.Types
    ( Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    , TxIn
    )

-- | A cage-protocol event detected in a block.
data CageEvent
    = -- | Mint with cage policyId: new token created
      CageBoot !TokenId !TokenState
    | -- | Output to cage address with RequestDatum
      CageRequest !TxIn !Request
    | -- | Consume requests, update trie root
      CageUpdate !TokenId !Root ![TxIn]
    | -- | Cancel a pending request
      CageRetract !TxIn
    | -- | Burn cage token: token removed
      CageBurn !TokenId
    deriving stock (Eq, Show)

-- | Inverse of a cage event, used for rollback.
data CageInverseOp
    = -- | Re-insert a deleted token
      InvRestoreToken !TokenId !TokenState
    | -- | Remove an inserted token
      InvRemoveToken !TokenId
    | -- | Re-insert a deleted request
      InvRestoreRequest !TxIn !Request
    | -- | Remove an inserted request
      InvRemoveRequest !TxIn
    | -- | Restore a token's previous root
      InvRestoreRoot !TokenId !Root
    deriving stock (Eq, Show)

-- | Detect cage events from a block.
--
-- TODO: implement real block inspection. This will
-- pattern-match on mints (boot\/burn), outputs to
-- cage address (request), and spending redeemers
-- (update\/retract).
detectCageEvents :: () -> [CageEvent]
detectCageEvents _ = []

-- | Compute inverse operations for a cage event,
-- given the cage state before the event.
--
-- Used to record rollback information alongside
-- cardano-utxo-csmt's rollback points.
inversesOf
    :: (TokenId -> Maybe TokenState)
    -- ^ Token lookup in current state
    -> (TxIn -> Maybe Request)
    -- ^ Request lookup in current state
    -> CageEvent
    -> [CageInverseOp]
inversesOf lookupToken lookupReq = \case
    CageBoot tid _ts ->
        [InvRemoveToken tid]
    CageRequest txIn _req ->
        [InvRemoveRequest txIn]
    CageUpdate tid _newRoot consumed ->
        let restoreRoot = case lookupToken tid of
                Just ts ->
                    [InvRestoreRoot tid (root ts)]
                Nothing -> []
            restoreReqs =
                concatMap
                    ( \txIn' -> case lookupReq txIn' of
                        Just req ->
                            [InvRestoreRequest txIn' req]
                        Nothing -> []
                    )
                    consumed
        in  restoreRoot ++ restoreReqs
    CageRetract txIn ->
        case lookupReq txIn of
            Just req ->
                [InvRestoreRequest txIn req]
            Nothing -> []
    CageBurn tid ->
        case lookupToken tid of
            Just ts -> [InvRestoreToken tid ts]
            Nothing -> []
