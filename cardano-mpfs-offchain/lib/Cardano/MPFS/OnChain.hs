{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.OnChain
-- Description : On-chain type encodings for the MPFS cage validator
-- License     : Apache-2.0
--
-- Haskell types matching the Aiken on-chain datum\/redeemer
-- structures, their PlutusData encoding, script identity,
-- and asset-name derivation.
--
-- These types use Plutus primitives directly (not
-- cardano-ledger types) because they model the exact
-- on-chain data layout expected by the Aiken validator.
module Cardano.MPFS.OnChain
    ( -- * On-chain datum\/redeemer types
      CageDatum (..)
    , MintRedeemer (..)
    , Mint (..)
    , UpdateRedeemer (..)

      -- * On-chain domain types
    , OnChainTokenId (..)
    , OnChainOperation (..)
    , OnChainRoot (..)
    , OnChainRequest (..)
    , OnChainTokenState (..)
    , OnChainTxOutRef (..)

      -- * Proof steps (Aiken MPF proof encoding)
    , ProofStep (..)
    , Neighbor (..)

      -- * Script identity
    , cageScriptHash

      -- * Asset-name derivation
    , deriveAssetName

      -- * Blueprint loading
    , Blueprint (..)
    , loadCageScript
    ) where

import Cardano.MPFS.Blueprint
    ( Blueprint (..)
    , loadBlueprint
    )
import Crypto.Hash (Digest, SHA256, hash)
import Data.Bits (shiftR)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16)
import PlutusCore.Data (Data (..))
import PlutusTx.Builtins.Internal
    ( BuiltinByteString (..)
    , BuiltinData (..)
    )
import PlutusTx.IsData.Class
    ( FromData (..)
    , ToData (..)
    , UnsafeFromData (..)
    )

-- ---------------------------------------------------------
-- On-chain domain types (Plutus primitives)
-- ---------------------------------------------------------

-- | On-chain token identifier (asset name as raw
-- bytes). Matches Aiken @lib/TokenId@.
newtype OnChainTokenId = OnChainTokenId
    { unOnChainTokenId :: BuiltinByteString
    }
    deriving stock (Show, Eq)

-- | On-chain output reference. Matches Aiken
-- @cardano/transaction/OutputReference@.
data OnChainTxOutRef = OnChainTxOutRef
    { txOutRefId :: !BuiltinByteString
    , txOutRefIdx :: !Integer
    }
    deriving stock (Show, Eq)

-- | On-chain MPF root hash (raw bytes).
newtype OnChainRoot = OnChainRoot
    { unOnChainRoot :: ByteString
    }
    deriving stock (Show, Eq)

-- | On-chain operation on a key in the trie.
-- Matches Aiken @types/Operation@.
data OnChainOperation
    = -- | Insert a new key-value pair (Constr 0)
      OpInsert !ByteString
    | -- | Delete a key (Constr 1)
      OpDelete !ByteString
    | -- | Update an existing key (Constr 2)
      OpUpdate !ByteString !ByteString
    deriving stock (Show, Eq)

-- | On-chain request to modify a token's trie.
-- Matches Aiken @types/Request@.
data OnChainRequest = OnChainRequest
    { requestToken :: !OnChainTokenId
    , requestOwner :: !BuiltinByteString
    , requestKey :: !ByteString
    , requestValue :: !OnChainOperation
    }
    deriving stock (Show, Eq)

-- | On-chain token state. Matches Aiken
-- @types/State@.
data OnChainTokenState = OnChainTokenState
    { stateOwner :: !BuiltinByteString
    , stateRoot :: !OnChainRoot
    }
    deriving stock (Show, Eq)

-- ---------------------------------------------------------
-- On-chain-only types (datum / redeemer wrappers)
-- ---------------------------------------------------------

-- | Cage datum: either a pending request or a token
-- state. Matches Aiken @types/CageDatum@.
data CageDatum
    = -- | A pending operation request (Constr 0)
      RequestDatum !OnChainRequest
    | -- | Current token state (Constr 1)
      StateDatum !OnChainTokenState
    deriving stock (Show, Eq)

-- | Minting redeemer. Matches Aiken
-- @types/MintRedeemer@.
data MintRedeemer
    = -- | Mint a new cage token (Constr 0)
      Minting !Mint
    | -- | Burn a cage token (Constr 1)
      Burning
    deriving stock (Show, Eq)

-- | Minting parameters. Matches Aiken
-- @types/Mint@.
newtype Mint = Mint
    { mintAsset :: OnChainTxOutRef
    }
    deriving stock (Show, Eq)

-- | Spending redeemer. Matches Aiken
-- @types/UpdateRedeemer@.
data UpdateRedeemer
    = -- | End the token (Constr 0)
      End
    | -- | Link a request to a state UTxO (Constr 1)
      Contribute !OnChainTxOutRef
    | -- | Fold requests with proofs (Constr 2)
      Modify ![[ProofStep]]
    | -- | Reclaim a pending request (Constr 3)
      Retract
    deriving stock (Show, Eq)

-- | A single step in an MPF Merkle proof, matching
-- the Aiken @ProofStep@ type from
-- @aiken-lang\/merkle-patricia-forestry@.
data ProofStep
    = -- | Branch step (Constr 0)
      Branch
        { branchSkip :: !Integer
        , branchNeighbors :: !ByteString
        }
    | -- | Fork step (Constr 1)
      Fork
        { forkSkip :: !Integer
        , forkNeighbor :: !Neighbor
        }
    | -- | Leaf step (Constr 2)
      Leaf
        { leafSkip :: !Integer
        , leafKey :: !ByteString
        , leafValue :: !ByteString
        }
    deriving stock (Show, Eq)

-- | Neighbor node in a fork proof step.
data Neighbor = Neighbor
    { neighborNibble :: !Integer
    , neighborPrefix :: !ByteString
    , neighborRoot :: !ByteString
    }
    deriving stock (Show, Eq)

-- ---------------------------------------------------------
-- Helpers for manual Data construction
-- ---------------------------------------------------------

mkD :: Data -> BuiltinData
mkD = BuiltinData

unD :: BuiltinData -> Data
unD (BuiltinData d) = d

bsToD :: ByteString -> Data
bsToD = B

bsFromD :: Data -> Maybe ByteString
bsFromD (B bs) = Just bs
bsFromD _ = Nothing

bbsToD :: BuiltinByteString -> Data
bbsToD (BuiltinByteString bs) = B bs

bbsFromD :: Data -> Maybe BuiltinByteString
bbsFromD (B bs) = Just (BuiltinByteString bs)
bbsFromD _ = Nothing

-- ---------------------------------------------------------
-- ToData / FromData instances
-- ---------------------------------------------------------

instance ToData OnChainTokenId where
    toBuiltinData (OnChainTokenId bbs) =
        mkD $ Constr 0 [bbsToD bbs]

instance FromData OnChainTokenId where
    fromBuiltinData bd = case unD bd of
        Constr 0 [x] ->
            OnChainTokenId <$> bbsFromD x
        _ -> Nothing

instance UnsafeFromData OnChainTokenId where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [x] -> case bbsFromD x of
            Just bbs -> OnChainTokenId bbs
            _ ->
                error
                    "unsafeFromBuiltinData: OnChainTokenId"
        _ ->
            error
                "unsafeFromBuiltinData: OnChainTokenId"

instance ToData OnChainTxOutRef where
    toBuiltinData OnChainTxOutRef{..} =
        mkD
            $ Constr
                0
                [bbsToD txOutRefId, I txOutRefIdx]

instance FromData OnChainTxOutRef where
    fromBuiltinData bd = case unD bd of
        Constr 0 [tid, I idx] ->
            OnChainTxOutRef
                <$> bbsFromD tid
                <*> pure idx
        _ -> Nothing

instance UnsafeFromData OnChainTxOutRef where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [B tid, I idx] ->
            OnChainTxOutRef
                (BuiltinByteString tid)
                idx
        _ ->
            error
                "unsafeFromBuiltinData: OnChainTxOutRef"

instance ToData OnChainOperation where
    toBuiltinData (OpInsert v) =
        mkD $ Constr 0 [bsToD v]
    toBuiltinData (OpDelete v) =
        mkD $ Constr 1 [bsToD v]
    toBuiltinData (OpUpdate old new) =
        mkD $ Constr 2 [bsToD old, bsToD new]

instance FromData OnChainOperation where
    fromBuiltinData bd = case unD bd of
        Constr 0 [v] -> OpInsert <$> bsFromD v
        Constr 1 [v] -> OpDelete <$> bsFromD v
        Constr 2 [o, n] ->
            OpUpdate <$> bsFromD o <*> bsFromD n
        _ -> Nothing

instance UnsafeFromData OnChainOperation where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [B v] -> OpInsert v
        Constr 1 [B v] -> OpDelete v
        Constr 2 [B o, B n] -> OpUpdate o n
        _ ->
            error
                "unsafeFromBuiltinData: OnChainOperation"

instance ToData OnChainRoot where
    toBuiltinData (OnChainRoot bs) = mkD $ bsToD bs

instance FromData OnChainRoot where
    fromBuiltinData bd =
        OnChainRoot <$> bsFromD (unD bd)

instance UnsafeFromData OnChainRoot where
    unsafeFromBuiltinData bd = case unD bd of
        B bs -> OnChainRoot bs
        _ ->
            error
                "unsafeFromBuiltinData: OnChainRoot"

instance ToData OnChainRequest where
    toBuiltinData OnChainRequest{..} =
        mkD
            $ Constr
                0
                [ unD (toBuiltinData requestToken)
                , bbsToD requestOwner
                , bsToD requestKey
                , unD (toBuiltinData requestValue)
                ]

instance FromData OnChainRequest where
    fromBuiltinData bd = case unD bd of
        Constr 0 [tok, own, k, val] -> do
            requestToken <-
                fromBuiltinData (mkD tok)
            requestOwner <- bbsFromD own
            requestKey <- bsFromD k
            requestValue <-
                fromBuiltinData (mkD val)
            Just OnChainRequest{..}
        _ -> Nothing

instance UnsafeFromData OnChainRequest where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [tok, B own, B k, val] ->
            OnChainRequest
                { requestToken =
                    unsafeFromBuiltinData (mkD tok)
                , requestOwner =
                    BuiltinByteString own
                , requestKey = k
                , requestValue =
                    unsafeFromBuiltinData (mkD val)
                }
        _ ->
            error
                "unsafeFromBuiltinData: OnChainRequest"

instance ToData OnChainTokenState where
    toBuiltinData OnChainTokenState{..} =
        mkD
            $ Constr
                0
                [ bbsToD stateOwner
                , unD (toBuiltinData stateRoot)
                ]

instance FromData OnChainTokenState where
    fromBuiltinData bd = case unD bd of
        Constr 0 [own, r] -> do
            stateOwner <- bbsFromD own
            stateRoot <- fromBuiltinData (mkD r)
            Just OnChainTokenState{..}
        _ -> Nothing

instance UnsafeFromData OnChainTokenState where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [B own, r] ->
            OnChainTokenState
                { stateOwner =
                    BuiltinByteString own
                , stateRoot =
                    unsafeFromBuiltinData (mkD r)
                }
        _ ->
            error
                "unsafeFromBuiltinData:\
                \ OnChainTokenState"

instance ToData CageDatum where
    toBuiltinData (RequestDatum r) =
        mkD
            $ Constr 0 [unD (toBuiltinData r)]
    toBuiltinData (StateDatum s) =
        mkD
            $ Constr 1 [unD (toBuiltinData s)]

instance FromData CageDatum where
    fromBuiltinData bd = case unD bd of
        Constr 0 [d] ->
            RequestDatum
                <$> fromBuiltinData (mkD d)
        Constr 1 [d] ->
            StateDatum
                <$> fromBuiltinData (mkD d)
        _ -> Nothing

instance UnsafeFromData CageDatum where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [d] ->
            RequestDatum
                $ unsafeFromBuiltinData (mkD d)
        Constr 1 [d] ->
            StateDatum
                $ unsafeFromBuiltinData (mkD d)
        _ -> error "unsafeFromBuiltinData: CageDatum"

instance ToData Mint where
    toBuiltinData (Mint ref) =
        mkD $ Constr 0 [unD (toBuiltinData ref)]

instance FromData Mint where
    fromBuiltinData bd = case unD bd of
        Constr 0 [d] ->
            Mint <$> fromBuiltinData (mkD d)
        _ -> Nothing

instance UnsafeFromData Mint where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [d] ->
            Mint $ unsafeFromBuiltinData (mkD d)
        _ -> error "unsafeFromBuiltinData: Mint"

instance ToData MintRedeemer where
    toBuiltinData (Minting m) =
        mkD $ Constr 0 [unD (toBuiltinData m)]
    toBuiltinData Burning =
        mkD $ Constr 1 []

instance FromData MintRedeemer where
    fromBuiltinData bd = case unD bd of
        Constr 0 [d] ->
            Minting <$> fromBuiltinData (mkD d)
        Constr 1 [] -> Just Burning
        _ -> Nothing

instance UnsafeFromData MintRedeemer where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [d] ->
            Minting $ unsafeFromBuiltinData (mkD d)
        Constr 1 [] -> Burning
        _ ->
            error "unsafeFromBuiltinData: MintRedeemer"

instance ToData Neighbor where
    toBuiltinData Neighbor{..} =
        mkD
            $ Constr
                0
                [ I neighborNibble
                , bsToD neighborPrefix
                , bsToD neighborRoot
                ]

instance FromData Neighbor where
    fromBuiltinData bd = case unD bd of
        Constr 0 [I nib, pfx, rt] ->
            Neighbor nib
                <$> bsFromD pfx
                <*> bsFromD rt
        _ -> Nothing

instance UnsafeFromData Neighbor where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [I nib, B pfx, B rt] ->
            Neighbor nib pfx rt
        _ -> error "unsafeFromBuiltinData: Neighbor"

instance ToData ProofStep where
    toBuiltinData Branch{..} =
        mkD
            $ Constr
                0
                [ I branchSkip
                , bsToD branchNeighbors
                ]
    toBuiltinData Fork{..} =
        mkD
            $ Constr
                1
                [ I forkSkip
                , unD (toBuiltinData forkNeighbor)
                ]
    toBuiltinData Leaf{..} =
        mkD
            $ Constr
                2
                [ I leafSkip
                , bsToD leafKey
                , bsToD leafValue
                ]

instance FromData ProofStep where
    fromBuiltinData bd = case unD bd of
        Constr 0 [I sk, nb] ->
            Branch sk <$> bsFromD nb
        Constr 1 [I sk, nd] ->
            Fork sk
                <$> fromBuiltinData (mkD nd)
        Constr 2 [I sk, k, v] ->
            Leaf sk <$> bsFromD k <*> bsFromD v
        _ -> Nothing

instance UnsafeFromData ProofStep where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [I sk, B nb] -> Branch sk nb
        Constr 1 [I sk, nd] ->
            Fork sk
                $ unsafeFromBuiltinData (mkD nd)
        Constr 2 [I sk, B k, B v] -> Leaf sk k v
        _ ->
            error "unsafeFromBuiltinData: ProofStep"

instance ToData UpdateRedeemer where
    toBuiltinData End = mkD $ Constr 0 []
    toBuiltinData (Contribute ref) =
        mkD $ Constr 1 [unD (toBuiltinData ref)]
    toBuiltinData (Modify proofs) =
        mkD
            $ Constr
                2
                [ List
                    ( map
                        ( List
                            . map
                                (unD . toBuiltinData)
                        )
                        proofs
                    )
                ]
    toBuiltinData Retract = mkD $ Constr 3 []

instance FromData UpdateRedeemer where
    fromBuiltinData bd = case unD bd of
        Constr 0 [] -> Just End
        Constr 1 [d] ->
            Contribute <$> fromBuiltinData (mkD d)
        Constr 2 [List ps] ->
            Modify <$> traverse parseProof ps
          where
            parseProof (List steps) =
                traverse
                    (fromBuiltinData . mkD)
                    steps
            parseProof _ = Nothing
        Constr 3 [] -> Just Retract
        _ -> Nothing

instance UnsafeFromData UpdateRedeemer where
    unsafeFromBuiltinData bd = case unD bd of
        Constr 0 [] -> End
        Constr 1 [d] ->
            Contribute
                $ unsafeFromBuiltinData (mkD d)
        Constr 2 [List ps] ->
            Modify $ map parseProof ps
          where
            parseProof (List steps) =
                map (unsafeFromBuiltinData . mkD) steps
            parseProof _ =
                error
                    "unsafeFromBuiltinData:\
                    \ UpdateRedeemer"
        Constr 3 [] -> Retract
        _ ->
            error
                "unsafeFromBuiltinData: UpdateRedeemer"

-- ---------------------------------------------------------
-- Script identity
-- ---------------------------------------------------------

-- | The cage validator script hash (raw 28 bytes).
-- Versioned with @cardano-mpfs-onchain v0.1.0@.
cageScriptHash :: ByteString
cageScriptHash =
    BS.pack
        [ 0xc1
        , 0xe3
        , 0x92
        , 0xee
        , 0x7d
        , 0xa9
        , 0x41
        , 0x5f
        , 0x94
        , 0x6d
        , 0xe9
        , 0xd2
        , 0xae
        , 0xf9
        , 0x60
        , 0x73
        , 0x22
        , 0xb4
        , 0x7d
        , 0x6e
        , 0x84
        , 0xe8
        , 0x14
        , 0x2e
        , 0xf0
        , 0xc3
        , 0x40
        , 0xbf
        ]

-- ---------------------------------------------------------
-- Asset-name derivation
-- ---------------------------------------------------------

-- | Derive the asset name from an output reference,
-- matching Aiken's @lib.assetName@:
--
-- @SHA2-256(txId ++ bigEndian16(outputIndex))@
deriveAssetName :: OnChainTxOutRef -> ByteString
deriveAssetName OnChainTxOutRef{txOutRefId, txOutRefIdx} =
    convert digest
  where
    digest :: Digest SHA256
    digest = hash (txIdBytes <> indexBytes)

    txIdBytes :: ByteString
    txIdBytes =
        let BuiltinByteString bs = txOutRefId
        in  bs

    indexBytes :: ByteString
    indexBytes =
        let w16 =
                fromIntegral txOutRefIdx :: Word16
        in  BS.pack
                [ fromIntegral (w16 `shiftR` 8)
                , fromIntegral w16
                ]

-- ---------------------------------------------------------
-- Blueprint loading
-- ---------------------------------------------------------

-- | Load and parse a CIP-57 blueprint from a file
-- path. Returns the parsed 'Blueprint' on success.
loadCageScript
    :: FilePath -> IO (Either String Blueprint)
loadCageScript = loadBlueprint
