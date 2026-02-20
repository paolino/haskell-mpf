{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Cardano.MPFS.Indexer.CageFollower
-- Description : Block processor for cage protocol events
-- License     : Apache-2.0
--
-- Processes Cardano blocks for cage-protocol events:
-- extracts Conway transactions, detects cage events
-- via 'detectCageEvents', applies state changes and
-- trie mutations, and records inverse operations for
-- rollback support.
module Cardano.MPFS.Indexer.CageFollower
    ( -- * Block processing
      processCageBlock
    , applyCageEvent
    , computeInverse
    , applyRequestOp

      -- * Rollback
    , applyCageInverses

      -- * Transaction extraction
    , extractConwayTxs
    ) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Lens.Micro ((^.))

import Cardano.Ledger.Api.Tx (Tx, bodyTxL)
import Cardano.Ledger.Api.Tx.Body (inputsTxBodyL)
import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Core (fromTxSeq)
import Cardano.Ledger.Hashes (ScriptHash)
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock (..)
    )

import Cardano.MPFS.Indexer.CageEvent
    ( CageEvent (..)
    , CageInverseOp (..)
    , detectCageEvents
    )
import Cardano.MPFS.NodeClient.Types qualified as NodeTypes
import Cardano.MPFS.State
    ( Requests (..)
    , State (..)
    , Tokens (..)
    )
import Cardano.MPFS.Trie
    ( Trie (..)
    , TrieManager (..)
    )
import Cardano.MPFS.Types
    ( ConwayEra
    , Operation (..)
    , Request (..)
    , Root (..)
    , TokenId
    , TokenState (..)
    , TxIn
    , TxOut
    )

-- | Extract Conway-era transactions from a multi-era
-- Cardano block. Returns empty for non-Conway blocks.
extractConwayTxs
    :: NodeTypes.Block -> [Tx ConwayEra]
extractConwayTxs = \case
    O.BlockConway (ShelleyBlock raw _) ->
        toList (fromTxSeq (bbody raw))
    _ -> []

-- | Process a block for cage events. For each
-- Conway transaction, resolves spent inputs, detects
-- cage events, computes inverse operations, and
-- applies state changes. Returns the collected
-- inverse operations for rollback support.
processCageBlock
    :: ScriptHash
    -- ^ Cage script hash
    -> State IO
    -- ^ Cage state interface
    -> TrieManager IO
    -- ^ Trie manager for per-token tries
    -> (TxIn -> IO (Maybe (TxOut ConwayEra)))
    -- ^ UTxO resolver for spent inputs
    -> NodeTypes.Block
    -- ^ The block to process
    -> IO [CageInverseOp]
processCageBlock scriptHash st tm resolveUtxo block = do
    let txs = extractConwayTxs block
    allEvents <-
        fmap concat
            $ traverse
                (detectFromTx scriptHash resolveUtxo)
                txs
    -- Process events sequentially:
    -- compute inverse THEN apply
    fmap concat $ traverse (processEvent st tm) allEvents

-- | Detect cage events from a single transaction.
detectFromTx
    :: ScriptHash
    -> (TxIn -> IO (Maybe (TxOut ConwayEra)))
    -> Tx ConwayEra
    -> IO [CageEvent]
detectFromTx scriptHash resolveUtxo tx = do
    let inputSet = tx ^. bodyTxL . inputsTxBodyL
    resolved <-
        resolveInputs
            resolveUtxo
            (Set.toList inputSet)
    pure $ detectCageEvents scriptHash resolved tx

-- | Process a single cage event: compute its inverse
-- and then apply it.
processEvent
    :: State IO
    -> TrieManager IO
    -> CageEvent
    -> IO [CageInverseOp]
processEvent st tm evt = do
    inv <- computeInverse st evt
    applyCageEvent st tm evt
    pure inv

-- | Resolve a list of 'TxIn' references to their
-- 'TxOut' values using the UTxO resolver.
resolveInputs
    :: (TxIn -> IO (Maybe (TxOut ConwayEra)))
    -> [TxIn]
    -> IO [(TxIn, TxOut ConwayEra)]
resolveInputs resolve =
    fmap catMaybes . traverse go
  where
    go txIn = do
        mOut <- resolve txIn
        pure $ fmap (txIn,) mOut

-- | Compute inverse operations for a cage event,
-- reading the current state before the event is
-- applied. These inverses can be replayed to undo
-- the event during rollback.
computeInverse
    :: State IO -> CageEvent -> IO [CageInverseOp]
computeInverse
    State
        { tokens = Tokens{getToken}
        , requests = Requests{getRequest}
        } = \case
        CageBoot tid _ts ->
            pure [InvRemoveToken tid]
        CageRequest txIn _req ->
            pure [InvRemoveRequest txIn]
        CageUpdate tid _newRoot consumed -> do
            mTs <- getToken tid
            let restoreRoot = case mTs of
                    Just ts ->
                        [InvRestoreRoot tid (root ts)]
                    Nothing -> []
            restoreReqs <-
                fmap concat
                    $ traverse
                        ( \txIn -> do
                            mReq <- getRequest txIn
                            pure $ case mReq of
                                Just req ->
                                    [ InvRestoreRequest
                                        txIn
                                        req
                                    ]
                                Nothing -> []
                        )
                        consumed
            pure $ restoreRoot ++ restoreReqs
        CageRetract txIn -> do
            mReq <- getRequest txIn
            pure $ case mReq of
                Just req ->
                    [InvRestoreRequest txIn req]
                Nothing -> []
        CageBurn tid -> do
            mTs <- getToken tid
            pure $ case mTs of
                Just ts ->
                    [InvRestoreToken tid ts]
                Nothing -> []

-- | Apply a cage event to the state and trie manager.
applyCageEvent
    :: State IO
    -> TrieManager IO
    -> CageEvent
    -> IO ()
applyCageEvent st tm = \case
    CageBoot tid ts -> do
        putToken (tokens st) tid ts
        createTrie tm tid
    CageRequest txIn req ->
        putRequest (requests st) txIn req
    CageUpdate tid newRoot consumed -> do
        -- Apply trie mutations for consumed requests
        withTrie tm tid $ \trie ->
            mapM_
                ( \txIn -> do
                    mReq <-
                        getRequest
                            (requests st)
                            txIn
                    case mReq of
                        Just req ->
                            applyRequestOp trie req
                        Nothing -> pure ()
                )
                consumed
        -- Remove consumed requests
        mapM_
            (removeRequest (requests st))
            consumed
        -- Update token root
        mTs <- getToken (tokens st) tid
        case mTs of
            Just ts ->
                putToken
                    (tokens st)
                    tid
                    ts{root = newRoot}
            Nothing -> pure ()
    CageRetract txIn ->
        removeRequest (requests st) txIn
    CageBurn tid -> do
        removeToken (tokens st) tid
        deleteTrie tm tid

-- | Apply a request's operation to a trie.
applyRequestOp :: Trie IO -> Request -> IO ()
applyRequestOp
    Trie{insert = trieInsert, delete = trieDelete}
    Request{requestKey, requestValue} =
        case requestValue of
            Insert v ->
                void $ trieInsert requestKey v
            Delete _ ->
                void $ trieDelete requestKey
            Update _ newV -> do
                void $ trieDelete requestKey
                void $ trieInsert requestKey newV

-- | Apply inverse operations for rollback, restoring
-- the state to what it was before the events.
applyCageInverses
    :: State IO
    -> TrieManager IO
    -> [CageInverseOp]
    -> IO ()
applyCageInverses st tm = mapM_ applyInv
  where
    applyInv = \case
        InvRestoreToken tid ts -> do
            putToken (tokens st) tid ts
            createTrie tm tid
        InvRemoveToken tid -> do
            removeToken (tokens st) tid
            deleteTrie tm tid
        InvRestoreRequest txIn req ->
            putRequest (requests st) txIn req
        InvRemoveRequest txIn ->
            removeRequest (requests st) txIn
        InvRestoreRoot tid r -> do
            mTs <- getToken (tokens st) tid
            case mTs of
                Just ts ->
                    putToken
                        (tokens st)
                        tid
                        ts{root = r}
                Nothing -> pure ()
