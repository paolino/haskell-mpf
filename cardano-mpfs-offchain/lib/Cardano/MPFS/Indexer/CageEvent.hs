{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Cardano.MPFS.Indexer.CageEvent
-- Description : Cage event detection from transactions
-- License     : Apache-2.0
--
-- Extracts cage-protocol events (boot, request, update,
-- retract, burn) from Cardano transactions by inspecting
-- mints, outputs, and spent inputs at the cage script
-- address and policy ID.
module Cardano.MPFS.Indexer.CageEvent
    ( -- * Event type
      CageEvent (..)

      -- * Inverse operations (for rollback)
    , CageInverseOp (..)

      -- * Detection
    , detectCageEvents
    , detectFromTx
    , inversesOf

      -- * Event application
    , applyCageEvent
    ) where

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Api.Tx
    ( Tx
    , bodyTxL
    , txIdTx
    )
import Cardano.Ledger.Api.Tx.Body
    ( inputsTxBodyL
    , mintTxBodyL
    , outputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Out
    ( TxOut
    , valueTxOutL
    )
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    , MultiAsset (..)
    )
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import Data.ByteString.Short qualified as SBS
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Word (Word16)
import Lens.Micro ((^.))
import PlutusTx.Builtins.Internal
    ( BuiltinByteString (..)
    )

import Cardano.MPFS.OnChain
    ( CageDatum (..)
    , OnChainOperation (..)
    , OnChainRequest (..)
    , OnChainRoot (..)
    , OnChainTokenId (..)
    , OnChainTokenState (..)
    )
import Cardano.MPFS.State
    ( Requests (..)
    , State (..)
    , Tokens (..)
    )
import Cardano.MPFS.Trie (TrieManager (..))
import Cardano.MPFS.TxBuilder.Real.Internal
    ( extractCageDatum
    )
import Cardano.MPFS.Types
    ( AssetName (..)
    , Coin (..)
    , ConwayEra
    , Operation (..)
    , PolicyID (..)
    , Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
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

-- | Detect cage events from a block (stub).
detectCageEvents :: () -> [CageEvent]
detectCageEvents _ = []

-- | Detect cage events from a single transaction.
--
-- Inspects mints (boot\/burn), outputs to cage
-- address (request), and spent inputs at the cage
-- address (update\/retract).
detectFromTx
    :: PolicyID
    -- ^ Cage minting policy ID
    -> ScriptHash
    -- ^ Cage script hash (reserved)
    -> (TxIn -> Maybe (TxOut ConwayEra))
    -- ^ Resolve spent inputs to their outputs
    -> Tx ConwayEra
    -> [CageEvent]
detectFromTx pid _sh resolver tx =
    detectMints pid tx
        ++ detectRequests tx
        ++ detectSpends pid resolver tx

-- ---------------------------------------------------------
-- Mint detection (boot / burn)
-- ---------------------------------------------------------

-- | Detect boot and burn events from the mint field.
detectMints
    :: PolicyID -> Tx ConwayEra -> [CageEvent]
detectMints pid tx =
    let MultiAsset ma = tx ^. bodyTxL . mintTxBodyL
    in  case Map.lookup pid ma of
            Nothing -> []
            Just assets ->
                concatMap (detectMint tx)
                    $ Map.toList assets

-- | Classify a single minted asset as boot or burn.
detectMint
    :: Tx ConwayEra
    -> (AssetName, Integer)
    -> [CageEvent]
detectMint tx (an, qty)
    | qty > 0 =
        let tid = TokenId an
            outs =
                toList
                    (tx ^. bodyTxL . outputsTxBodyL)
            mState = findStateDatumInOuts outs
        in  case mState of
                Just ts -> [CageBoot tid ts]
                Nothing -> []
    | qty < 0 =
        [CageBurn (TokenId an)]
    | otherwise = []

-- | Find a 'StateDatum' in outputs and convert.
findStateDatumInOuts
    :: [TxOut ConwayEra] -> Maybe TokenState
findStateDatumInOuts [] = Nothing
findStateDatumInOuts (out : rest) =
    case extractCageDatum out of
        Just (StateDatum s) ->
            Just (onChainToTokenState s)
        _ -> findStateDatumInOuts rest

-- ---------------------------------------------------------
-- Request detection (outputs with RequestDatum)
-- ---------------------------------------------------------

-- | Detect request events from outputs that carry a
-- 'RequestDatum'.
detectRequests :: Tx ConwayEra -> [CageEvent]
detectRequests tx =
    let txid = txIdTx tx
        outs =
            zip [0 :: Word16 ..]
                $ toList
                    (tx ^. bodyTxL . outputsTxBodyL)
    in  mapMaybe (detectRequest txid) outs

-- | Check if an output is a cage request.
detectRequest
    :: TxId
    -> (Word16, TxOut ConwayEra)
    -> Maybe CageEvent
detectRequest txid (ix, out) =
    case extractCageDatum out of
        Just (RequestDatum r) ->
            let txIn =
                    TxIn
                        txid
                        (TxIx (fromIntegral ix))
                req = onChainToRequest r
            in  Just (CageRequest txIn req)
        _ -> Nothing

-- ---------------------------------------------------------
-- Spend detection (update / retract)
-- ---------------------------------------------------------

-- | Detect update and retract events from spent
-- inputs.
detectSpends
    :: PolicyID
    -> (TxIn -> Maybe (TxOut ConwayEra))
    -> Tx ConwayEra
    -> [CageEvent]
detectSpends pid resolver tx =
    let spentIns =
            Set.toList
                (tx ^. bodyTxL . inputsTxBodyL)
        resolved =
            mapMaybe
                ( \tin -> case resolver tin of
                    Just out -> Just (tin, out)
                    Nothing -> Nothing
                )
                spentIns
        stateSpends =
            filter (hasCageToken pid . snd) resolved
        requestSpends =
            filter
                ( \(_, out) ->
                    isRequestDatum out
                        && not (hasCageToken pid out)
                )
                resolved
    in  case stateSpends of
            [(_, stateOut)] ->
                let tid =
                        extractTokenIdFromOut
                            pid
                            stateOut
                    newRoot = extractNewRoot tx
                    consumedReqs =
                        map fst requestSpends
                in  [ CageUpdate
                        tid
                        newRoot
                        consumedReqs
                    ]
            _ ->
                map (CageRetract . fst) requestSpends

-- ---------------------------------------------------------
-- Conversion helpers
-- ---------------------------------------------------------

-- | Check if a 'TxOut' carries the cage token.
hasCageToken :: PolicyID -> TxOut ConwayEra -> Bool
hasCageToken pid out =
    case out ^. valueTxOutL of
        MaryValue _ (MultiAsset ma) ->
            case Map.lookup pid ma of
                Just assets ->
                    not (Map.null assets)
                Nothing -> False

-- | Check if a 'TxOut' has a 'RequestDatum'.
isRequestDatum :: TxOut ConwayEra -> Bool
isRequestDatum out =
    case extractCageDatum out of
        Just (RequestDatum _) -> True
        _ -> False

-- | Extract the 'TokenId' from a state UTxO.
extractTokenIdFromOut
    :: PolicyID -> TxOut ConwayEra -> TokenId
extractTokenIdFromOut pid out =
    case out ^. valueTxOutL of
        MaryValue _ (MultiAsset ma) ->
            case Map.lookup pid ma of
                Just assets ->
                    case Map.keys assets of
                        [an] -> TokenId an
                        _ ->
                            error
                                "extractTokenIdFromOut:\
                                \ expected 1 asset"
                Nothing ->
                    error
                        "extractTokenIdFromOut:\
                        \ no cage policy"

-- | Extract the new root from the first StateDatum
-- output.
extractNewRoot :: Tx ConwayEra -> Root
extractNewRoot tx =
    let outs =
            toList
                (tx ^. bodyTxL . outputsTxBodyL)
    in  go outs
  where
    go [] =
        error
            "extractNewRoot: no StateDatum output"
    go (out : rest) =
        case extractCageDatum out of
            Just (StateDatum s) ->
                Root
                    (unOnChainRoot (stateRoot s))
            _ -> go rest

-- | Convert on-chain token state to domain type.
onChainToTokenState
    :: OnChainTokenState -> TokenState
onChainToTokenState
    OnChainTokenState
        { stateOwner = BuiltinByteString ownerBs
        , stateRoot = r
        , stateMaxFee = mf
        , stateProcessTime = pt
        , stateRetractTime = rt
        } =
        let kh = case hashFromBytes ownerBs of
                Just h -> KeyHash h
                Nothing ->
                    error
                        "onChainToTokenState: \
                        \invalid owner hash"
        in  TokenState
                { owner = kh
                , root =
                    Root (unOnChainRoot r)
                , maxFee = Coin mf
                , processTime = pt
                , retractTime = rt
                }

-- | Convert on-chain request to domain type.
onChainToRequest :: OnChainRequest -> Request
onChainToRequest
    OnChainRequest
        { requestToken =
            OnChainTokenId
                (BuiltinByteString tidBs)
        , requestOwner =
            BuiltinByteString ownerBs
        , requestKey = rKey
        , requestValue = rVal
        , requestFee = rFee
        , requestSubmittedAt = rSub
        } =
        let kh = case hashFromBytes ownerBs of
                Just h -> KeyHash h
                Nothing ->
                    error
                        "onChainToRequest: \
                        \invalid owner hash"
            op = case rVal of
                OpInsert v -> Insert v
                OpDelete v -> Delete v
                OpUpdate o n -> Update o n
        in  Request
                { requestToken =
                    TokenId
                        (AssetName (SBS.toShort tidBs))
                , requestOwner = kh
                , requestKey = rKey
                , requestValue = op
                , requestFee = Coin rFee
                , requestSubmittedAt = rSub
                }

-- ---------------------------------------------------------
-- Event application
-- ---------------------------------------------------------

-- | Apply a cage event to the state and trie
-- manager.
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
        mTs <- getToken (tokens st) tid
        case mTs of
            Just ts ->
                putToken
                    (tokens st)
                    tid
                    ts{root = newRoot}
            Nothing -> pure ()
        mapM_
            (removeRequest (requests st))
            consumed
    CageRetract txIn ->
        removeRequest (requests st) txIn
    CageBurn tid -> do
        removeToken (tokens st) tid
        deleteTrie tm tid

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
