{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.TxBuilder.Real
-- Description : Real transaction builders for the MPFS cage
-- License     : Apache-2.0
--
-- Builds unsigned Cardano transactions for the MPFS
-- cage protocol: minting tokens, submitting requests,
-- and (later) processing updates.
module Cardano.MPFS.TxBuilder.Real
    ( -- * Construction
      mkRealTxBuilder
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Lens.Micro ((&), (.~))

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Scripts
    ( AsIx (..)
    , fromPlutusScript
    , mkPlutusScript
    )
import Cardano.Ledger.Api.Scripts.Data
    ( Data (..)
    , Datum (..)
    , dataToBinaryData
    )
import Cardano.Ledger.Api.Tx
    ( Tx
    , mkBasicTx
    , witsTxL
    )
import Cardano.Ledger.Api.Tx.Body
    ( collateralInputsTxBodyL
    , inputsTxBodyL
    , mintTxBodyL
    , mkBasicTxBody
    , outputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Out
    ( datumTxOutL
    , mkBasicTxOut
    )
import Cardano.Ledger.Api.Tx.Wits
    ( Redeemers (..)
    , rdmrsTxWitsL
    , scriptTxWitsL
    )
import Cardano.Ledger.BaseTypes (Inject (..), TxIx (..))
import Cardano.Ledger.Conway.Scripts
    ( ConwayPlutusPurpose (..)
    )
import Cardano.Ledger.Core
    ( Script
    , extractHash
    , hashScript
    )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    , MultiAsset (..)
    )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language
    ( Language (PlutusV3)
    , Plutus (..)
    , PlutusBinary (..)
    )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import PlutusCore.Data qualified as PLC
import PlutusTx.Builtins.Internal
    ( BuiltinByteString (..)
    , BuiltinData (..)
    )
import PlutusTx.IsData.Class (ToData (..))

import Cardano.MPFS.Balance (balanceTx)
import Cardano.MPFS.OnChain
    ( CageDatum (..)
    , Mint (..)
    , MintRedeemer (..)
    , OnChainOperation (..)
    , OnChainRequest (..)
    , OnChainRoot (..)
    , OnChainTokenId (..)
    , OnChainTokenState (..)
    , OnChainTxOutRef (..)
    , cageAddr
    , cagePolicyId
    , deriveAssetName
    )
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State (State (..), Tokens (..))
import Cardano.MPFS.Trie (TrieManager)
import Cardano.MPFS.TxBuilder (TxBuilder (..))
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )
import Cardano.MPFS.Types
    ( AssetName (..)
    , Coin (..)
    , ConwayEra
    , TokenId (..)
    , TokenState (..)
    )

-- ---------------------------------------------------------
-- Construction
-- ---------------------------------------------------------

-- | Create a real 'TxBuilder IO' wired to a
-- 'Provider', 'State', and 'TrieManager'.
--
-- Implements 'bootToken', 'requestInsert', and
-- 'requestDelete'. The remaining operations
-- ('updateToken', 'retractRequest', 'endToken')
-- are left as errors for Phase 3b.
mkRealTxBuilder
    :: CageConfig
    -> Provider IO
    -> State IO
    -> TrieManager IO
    -> TxBuilder IO
mkRealTxBuilder cfg prov st _tm =
    TxBuilder
        { bootToken = bootTokenImpl cfg prov
        , requestInsert =
            requestInsertImpl cfg prov st
        , requestDelete =
            requestDeleteImpl cfg prov st
        , updateToken = \_ _ ->
            error
                "mkRealTxBuilder: updateToken \
                \not yet implemented"
        , retractRequest = \_ _ ->
            error
                "mkRealTxBuilder: retractRequest \
                \not yet implemented"
        , endToken = \_ _ ->
            error
                "mkRealTxBuilder: endToken \
                \not yet implemented"
        }

-- ---------------------------------------------------------
-- requestInsert
-- ---------------------------------------------------------

-- | Build a request-insert transaction.
--
-- No script execution — just pays to the cage
-- address with an inline 'RequestDatum'.
requestInsertImpl
    :: CageConfig
    -> Provider IO
    -> State IO
    -> TokenId
    -> ByteString
    -> ByteString
    -> Addr
    -> IO (Tx ConwayEra)
requestInsertImpl cfg prov st tid key value addr = do
    mTs <- getToken (tokens st) tid
    TokenState{maxFee = Coin mf} <- case mTs of
        Nothing ->
            error "requestInsert: unknown token"
        Just x -> pure x
    pp <- queryProtocolParams prov
    utxos <- queryUTxOs prov addr
    feeUtxo <- case utxos of
        [] -> error "requestInsert: no UTxOs"
        (u : _) -> pure u
    let datum =
            mkRequestDatum
                tid
                addr
                key
                (OpInsert value)
                mf
        scriptAddr = cageAddr (network cfg)
        minAda = Coin (2_000_000 + mf)
        txOut =
            mkBasicTxOut
                scriptAddr
                (inject minAda)
                & datumTxOutL
                    .~ mkInlineDatum datum
        body =
            mkBasicTxBody
                & outputsTxBodyL
                    .~ StrictSeq.singleton txOut
        tx = mkBasicTx body
    case balanceTx pp feeUtxo addr tx of
        Left err ->
            error
                $ "requestInsert: "
                    <> show err
        Right balanced -> pure balanced

-- ---------------------------------------------------------
-- requestDelete
-- ---------------------------------------------------------

-- | Build a request-delete transaction.
-- Same structure as requestInsert with 'OpDelete'.
requestDeleteImpl
    :: CageConfig
    -> Provider IO
    -> State IO
    -> TokenId
    -> ByteString
    -> Addr
    -> IO (Tx ConwayEra)
requestDeleteImpl cfg prov st tid key addr = do
    mTs <- getToken (tokens st) tid
    TokenState{maxFee = Coin mf} <- case mTs of
        Nothing ->
            error "requestDelete: unknown token"
        Just x -> pure x
    pp <- queryProtocolParams prov
    utxos <- queryUTxOs prov addr
    feeUtxo <- case utxos of
        [] -> error "requestDelete: no UTxOs"
        (u : _) -> pure u
    let datum =
            mkRequestDatum
                tid
                addr
                key
                (OpDelete key)
                mf
        scriptAddr = cageAddr (network cfg)
        minAda = Coin (2_000_000 + mf)
        txOut =
            mkBasicTxOut
                scriptAddr
                (inject minAda)
                & datumTxOutL
                    .~ mkInlineDatum datum
        body =
            mkBasicTxBody
                & outputsTxBodyL
                    .~ StrictSeq.singleton txOut
        tx = mkBasicTx body
    case balanceTx pp feeUtxo addr tx of
        Left err ->
            error
                $ "requestDelete: "
                    <> show err
        Right balanced -> pure balanced

-- ---------------------------------------------------------
-- bootToken
-- ---------------------------------------------------------

-- | Build a boot-token minting transaction.
--
-- Picks a wallet UTxO as seed for the asset name,
-- mints +1 token at the cage policy, and creates
-- a State UTxO with empty root and maxFee.
bootTokenImpl
    :: CageConfig
    -> Provider IO
    -> Addr
    -> IO (Tx ConwayEra)
bootTokenImpl cfg prov addr = do
    pp <- queryProtocolParams prov
    utxos <- queryUTxOs prov addr
    case utxos of
        [] -> error "bootToken: no UTxOs"
        (seedUtxo : rest) -> do
            let (seedRef, _seedOut) = seedUtxo
                feeUtxo = case rest of
                    [] -> seedUtxo
                    (u : _) -> u
            -- 1. Derive asset name from seed
            let onChainRef = txInToRef seedRef
                assetNameBs =
                    deriveAssetName onChainRef
                assetName =
                    AssetName
                        (SBS.toShort assetNameBs)
            -- 2. Build mint value (+1 token)
            let mintMA =
                    MultiAsset
                        $ Map.singleton
                            cagePolicyId
                        $ Map.singleton
                            assetName
                            1
            -- 3. Build output datum
            let stateDatum =
                    StateDatum
                        OnChainTokenState
                            { stateOwner =
                                BuiltinByteString
                                    (addrKeyHashBytes addr)
                            , stateRoot =
                                OnChainRoot emptyRoot
                            , stateMaxFee =
                                let Coin c =
                                        defaultMaxFee cfg
                                in  c
                            , stateProcessTime =
                                defaultProcessTime cfg
                            , stateRetractTime =
                                defaultRetractTime cfg
                            }
                datumData = toPlcData stateDatum
            -- 4. Build output with ada + token
            let scriptAddr = cageAddr (network cfg)
                outValue =
                    MaryValue (Coin 2_000_000) mintMA
                txOut =
                    mkBasicTxOut
                        scriptAddr
                        outValue
                        & datumTxOutL
                            .~ mkInlineDatum
                                datumData
            -- 5. Build script + redeemer
            let script = mkCageScript cfg
                scriptHash = hashScript script
                redeemer =
                    Minting (Mint onChainRef)
                mintPurpose =
                    ConwayMinting (AsIx 0)
                redeemers =
                    Redeemers
                        $ Map.singleton
                            mintPurpose
                            ( toLedgerData redeemer
                            , defaultMintExUnits
                            )
            -- 6. Build tx body
            let body =
                    mkBasicTxBody
                        & inputsTxBodyL
                            .~ Set.singleton
                                seedRef
                        & outputsTxBodyL
                            .~ StrictSeq.singleton
                                txOut
                        & mintTxBodyL .~ mintMA
                        & collateralInputsTxBodyL
                            .~ Set.singleton
                                (fst feeUtxo)
                tx =
                    mkBasicTx body
                        & witsTxL . scriptTxWitsL
                            .~ Map.singleton
                                scriptHash
                                script
                        & witsTxL . rdmrsTxWitsL
                            .~ redeemers
            -- 7. Balance
            case balanceTx pp feeUtxo addr tx of
                Left err ->
                    error
                        $ "bootToken: "
                            <> show err
                Right balanced -> pure balanced

-- ---------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------

-- | Empty MPF root (32 zero bytes).
emptyRoot :: ByteString
emptyRoot = BS.replicate 32 0

-- | Hardcoded execution units for minting.
-- A later phase will use evaluateTx for precise
-- values.
defaultMintExUnits :: ExUnits
defaultMintExUnits =
    ExUnits 500_000_000 500_000

-- | Build the cage 'Script' from config bytes.
mkCageScript :: CageConfig -> Script ConwayEra
mkCageScript cfg =
    let plutus =
            Plutus @PlutusV3
                $ PlutusBinary
                $ cageScriptBytes cfg
    in  case mkPlutusScript plutus of
            Just ps -> fromPlutusScript ps
            Nothing ->
                error
                    "mkCageScript: invalid \
                    \PlutusV3 script"

-- | Build a 'CageDatum' for a request.
mkRequestDatum
    :: TokenId
    -> Addr
    -> ByteString
    -> OnChainOperation
    -> Integer
    -> PLC.Data
mkRequestDatum tid addr key op fee =
    let onChainTid =
            OnChainTokenId
                $ BuiltinByteString
                $ SBS.fromShort
                $ let AssetName sbs = unTokenId tid
                  in  sbs
        datum =
            OnChainRequest
                { requestToken = onChainTid
                , requestOwner =
                    BuiltinByteString
                        (addrKeyHashBytes addr)
                , requestKey = key
                , requestValue = op
                , requestFee = fee
                , requestSubmittedAt = 0
                }
    in  toPlcData (RequestDatum datum)

-- | Convert a 'ToData' value to
-- 'PlutusCore.Data.Data'.
toPlcData :: (ToData a) => a -> PLC.Data
toPlcData x =
    let BuiltinData d = toBuiltinData x in d

-- | Convert a 'ToData' value to a ledger 'Data'.
toLedgerData
    :: (ToData a) => a -> Data ConwayEra
toLedgerData = Data . toPlcData

-- | Wrap 'PlutusCore.Data.Data' as an inline
-- 'Datum'.
mkInlineDatum :: PLC.Data -> Datum ConwayEra
mkInlineDatum d =
    Datum
        $ dataToBinaryData
            (Data d :: Data ConwayEra)

-- | Convert a ledger 'TxIn' to an on-chain
-- 'OnChainTxOutRef'.
txInToRef :: TxIn -> OnChainTxOutRef
txInToRef (TxIn (TxId h) (TxIx ix)) =
    OnChainTxOutRef
        { txOutRefId =
            BuiltinByteString
                (hashToBytes (extractHash h))
        , txOutRefIdx = fromIntegral ix
        }

-- | Extract the payment key hash raw bytes from
-- an 'Addr'. Returns empty bytes for script
-- addresses (requests from scripts are unusual
-- but not forbidden).
addrKeyHashBytes :: Addr -> ByteString
addrKeyHashBytes (Addr _ (KeyHashObj (KeyHash h)) _) =
    hashToBytes h
addrKeyHashBytes _ = BS.empty
