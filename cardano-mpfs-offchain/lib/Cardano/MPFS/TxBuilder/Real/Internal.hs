{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.TxBuilder.Real.Internal
-- Description : Shared helpers for real transaction builders
-- License     : Apache-2.0
--
-- Utility functions shared across the per-operation
-- transaction builders (@Boot@, @Request@, @Update@,
-- @Retract@, @End@). Covers script construction,
-- datum\/redeemer encoding, address manipulation,
-- UTxO lookup, spending-index computation,
-- execution-unit defaults, and POSIX-to-slot conversion.
module Cardano.MPFS.TxBuilder.Real.Internal
    ( -- * Script construction
      mkCageScript
    , computeScriptHash

      -- * Derived identity
    , cagePolicyIdFromCfg
    , cageAddrFromCfg

      -- * Datum helpers
    , mkRequestDatum
    , toPlcData
    , toLedgerData
    , mkInlineDatum
    , extractCageDatum

      -- * Reference conversion
    , txInToRef
    , addrKeyHashBytes
    , addrFromKeyHashBytes
    , addrWitnessKeyHash

      -- * UTxO lookup
    , findUtxoByTxIn
    , findStateUtxo
    , findRequestUtxos

      -- * Indexing
    , spendingIndex

      -- * Execution units
    , defaultMintExUnits
    , defaultSpendExUnits
    , modifyExUnits
    , contributeExUnits

      -- * Script integrity
    , computeScriptIntegrity

      -- * Slot conversion
    , posixMsToSlot
    , posixMsCeilSlot

      -- * Constants
    , emptyRoot
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word32)
import Lens.Micro ((^.))

import Cardano.Crypto.Hash (hashFromBytes, hashToBytes)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.PParams
    ( LangDepView
    , getLanguageView
    )
import Cardano.Ledger.Alonzo.Scripts
    ( fromPlutusScript
    , mkPlutusScript
    )
import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash
    , hashScriptIntegrity
    )
import Cardano.Ledger.Alonzo.TxWits
    ( Redeemers
    , TxDats (..)
    )
import Cardano.Ledger.Api.Scripts.Data
    ( Data (..)
    , Datum (..)
    , binaryDataToData
    , dataToBinaryData
    )
import Cardano.Ledger.Api.Tx.Out
    ( TxOut
    , datumTxOutL
    , valueTxOutL
    )
import Cardano.Ledger.BaseTypes
    ( Network
    , SlotNo (..)
    , StrictMaybe
    , TxIx (..)
    )
import Cardano.Ledger.Core
    ( Script
    , extractHash
    , hashScript
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    , StakeReference (..)
    )
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys
    ( KeyHash (..)
    , KeyRole (..)
    )
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    , MultiAsset (..)
    , PolicyID (..)
    )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language
    ( Language (PlutusV3)
    , Plutus (..)
    , PlutusBinary (..)
    )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Data.Coerce (coerce)
import PlutusCore.Data qualified as PLC
import PlutusTx.Builtins.Internal
    ( BuiltinByteString (..)
    , BuiltinData (..)
    )
import PlutusTx.IsData.Class
    ( FromData (..)
    , ToData (..)
    )

import Cardano.MPFS.Core.OnChain
    ( CageDatum (..)
    , OnChainOperation (..)
    , OnChainRequest (..)
    , OnChainTokenId (..)
    , OnChainTxOutRef (..)
    )
import Cardano.MPFS.Core.Types
    ( AssetName (..)
    , ConwayEra
    , PParams
    , TokenId (..)
    )
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )

-- | Convert POSIX time (ms) to a 'SlotNo',
-- rounding __down__ (floor). Use this for upper
-- bounds where the validator checks
-- @entirely_before(deadline)@: the last valid
-- slot is at or before the deadline.
posixMsToSlot
    :: CageConfig
    -- ^ Config with system start and slot length
    -> Integer
    -- ^ POSIX time in milliseconds
    -> SlotNo
posixMsToSlot cfg ms =
    let elapsed = max 0 (ms - systemStartPosixMs cfg)
        slot = elapsed `div` slotLengthMs cfg
    in  SlotNo (fromIntegral slot)

-- | Convert POSIX time (ms) to a 'SlotNo',
-- rounding __up__ (ceiling). Use this for lower
-- bounds where the validator checks
-- @entirely_after(deadline)@: the first valid
-- slot is at or after the deadline.
posixMsCeilSlot
    :: CageConfig
    -- ^ Config with system start and slot length
    -> Integer
    -- ^ POSIX time in milliseconds
    -> SlotNo
posixMsCeilSlot cfg ms =
    let elapsed = max 0 (ms - systemStartPosixMs cfg)
        sl = slotLengthMs cfg
        slot = (elapsed + sl - 1) `div` sl
    in  SlotNo (fromIntegral slot)

-- | Empty MPF root (32 zero bytes).
emptyRoot :: ByteString
emptyRoot = BS.replicate 32 0

-- | Hardcoded execution units for minting.
-- A later phase will use evaluateTx for precise
-- values.
defaultMintExUnits :: ExUnits
defaultMintExUnits =
    ExUnits 14_000_000 500_000_000

-- | Hardcoded execution units for spending.
defaultSpendExUnits :: ExUnits
defaultSpendExUnits =
    ExUnits 14_000_000 500_000_000

-- | Execution units for the @Modify@ redeemer,
-- scaled by the number of proofs. Each proof
-- adds roughly 500M CPU steps for the on-chain
-- MPF fold.
modifyExUnits
    :: Int
    -- ^ Number of proofs (one per request)
    -> ExUnits
modifyExUnits nProofs =
    ExUnits
        14_000_000
        (fromIntegral nProofs * 500_000_000)

-- | Execution units for the @Contribute@ redeemer.
-- Contribute just checks the request token matches
-- the state token — much cheaper than @Modify@.
contributeExUnits :: ExUnits
contributeExUnits =
    ExUnits 200_000 100_000_000

-- | Build the cage 'Script' from config bytes.
mkCageScript
    :: CageConfig
    -- ^ Config with applied script bytes
    -> Script ConwayEra
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

-- | Compute the 'ScriptHash' from raw script bytes.
-- Use this when building a 'CageConfig' to fill
-- the 'cfgScriptHash' field.
computeScriptHash
    :: SBS.ShortByteString
    -- ^ Flat-encoded PlutusV3 script bytes
    -> ScriptHash
computeScriptHash sbs =
    let plutus =
            Plutus @PlutusV3
                $ PlutusBinary sbs
    in  case mkPlutusScript @ConwayEra plutus of
            Just ps ->
                hashScript @ConwayEra
                    $ fromPlutusScript ps
            Nothing ->
                error
                    "computeScriptHash: invalid \
                    \PlutusV3 script"

-- | Compute the cage minting policy ID from config.
cagePolicyIdFromCfg :: CageConfig -> PolicyID
cagePolicyIdFromCfg =
    PolicyID . cfgScriptHash

-- | Compute the cage script address from config.
cageAddrFromCfg
    :: CageConfig
    -- ^ Config with script hash
    -> Network
    -- ^ Target network
    -> Addr
cageAddrFromCfg cfg net =
    Addr
        net
        (ScriptHashObj $ cfgScriptHash cfg)
        StakeRefNull

-- | Build a 'CageDatum' for a request.
mkRequestDatum
    :: TokenId
    -- ^ Token to modify
    -> Addr
    -- ^ Requester's address (owner key hash extracted)
    -> ByteString
    -- ^ Trie key to operate on
    -> OnChainOperation
    -- ^ Insert, delete, or update
    -> Integer
    -- ^ Fee (lovelace)
    -> Integer
    -- ^ Submitted at (POSIXTime in ms)
    -> PLC.Data
mkRequestDatum tid addr key op fee submittedAt =
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
                , requestSubmittedAt = submittedAt
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
addrKeyHashBytes
    (Addr _ (KeyHashObj (KeyHash h)) _) =
        hashToBytes h
addrKeyHashBytes _ = BS.empty

-- | Reconstruct an 'Addr' from raw payment key hash
-- bytes.
addrFromKeyHashBytes
    :: Network
    -- ^ Target network
    -> ByteString
    -- ^ Raw 28-byte payment key hash
    -> Addr
addrFromKeyHashBytes net bs =
    case hashFromBytes bs of
        Just h ->
            Addr
                net
                (KeyHashObj (KeyHash h))
                StakeRefNull
        Nothing ->
            error
                "addrFromKeyHashBytes: \
                \invalid hash"

-- | Extract a 'KeyHash' ''Witness' from raw
-- payment key hash bytes (for required signers).
addrWitnessKeyHash
    :: ByteString -> KeyHash 'Witness
addrWitnessKeyHash bs =
    case hashFromBytes bs of
        Just h ->
            coerce (KeyHash h :: KeyHash 'Payment)
        Nothing ->
            error
                "addrWitnessKeyHash: \
                \invalid hash"

-- | Find a UTxO by its 'TxIn'.
findUtxoByTxIn
    :: TxIn
    -> [(TxIn, TxOut ConwayEra)]
    -> Maybe (TxIn, TxOut ConwayEra)
findUtxoByTxIn needle =
    find' (\(tin, _) -> tin == needle)
  where
    find' _ [] = Nothing
    find' p (x : xs)
        | p x = Just x
        | otherwise = find' p xs

-- | Find the state UTxO for a token by checking
-- the 'MultiAsset' for the cage policy + token's
-- asset name.
findStateUtxo
    :: PolicyID
    -- ^ Cage minting policy
    -> TokenId
    -- ^ Token to look for
    -> [(TxIn, TxOut ConwayEra)]
    -- ^ UTxO set to search
    -> Maybe (TxIn, TxOut ConwayEra)
findStateUtxo policyId tid = find' isState
  where
    assetName = unTokenId tid
    isState (_, txOut) =
        case txOut ^. valueTxOutL of
            MaryValue _ (MultiAsset ma) ->
                case Map.lookup policyId ma of
                    Just assets ->
                        Map.member assetName assets
                    Nothing -> False
    find' _ [] = Nothing
    find' p (x : xs)
        | p x = Just x
        | otherwise = find' p xs

-- | Find all request UTxOs for a token by decoding
-- their inline datums.
findRequestUtxos
    :: TokenId
    -- ^ Token to filter by
    -> [(TxIn, TxOut ConwayEra)]
    -- ^ UTxO set to search
    -> [(TxIn, TxOut ConwayEra)]
findRequestUtxos tid = filter isRequest
  where
    targetName = unTokenId tid
    isRequest (_, txOut) =
        case extractCageDatum txOut of
            Just (RequestDatum req) ->
                let OnChainRequest
                        { requestToken =
                            OnChainTokenId
                                (BuiltinByteString bs)
                        } = req
                in  AssetName (SBS.toShort bs)
                        == targetName
            _ -> False

-- | Extract a 'CageDatum' from an inline datum
-- in a 'TxOut'.
extractCageDatum
    :: TxOut ConwayEra -> Maybe CageDatum
extractCageDatum txOut =
    case txOut ^. datumTxOutL of
        Datum bd ->
            let Data plcData =
                    binaryDataToData bd
            in  fromBuiltinData (BuiltinData plcData)
        _ -> Nothing

-- | Compute the spending index of a 'TxIn' in
-- the sorted set of all inputs.
spendingIndex :: TxIn -> Set.Set TxIn -> Word32
spendingIndex needle inputs =
    let sorted = Set.toAscList inputs
    in  go 0 sorted
  where
    go _ [] =
        error "spendingIndex: TxIn not in set"
    go n (x : xs)
        | x == needle = n
        | otherwise = go (n + 1) xs

-- | Compute the 'ScriptIntegrityHash' from the
-- protocol params, redeemers, and languages used.
computeScriptIntegrity
    :: PParams ConwayEra
    -- ^ Current protocol parameters
    -> Redeemers ConwayEra
    -- ^ All redeemers in the transaction
    -> StrictMaybe ScriptIntegrityHash
computeScriptIntegrity pp rdmrs =
    let langViews :: Set.Set LangDepView
        langViews =
            Set.singleton
                (getLanguageView pp PlutusV3)
        emptyDats = TxDats mempty
    in  hashScriptIntegrity langViews rdmrs emptyDats
