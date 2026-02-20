{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Cardano.MPFS.Indexer.Codecs
-- Description : CBOR serialization codecs for indexer columns
-- License     : Apache-2.0
--
-- Provides 'Prism''-based codecs for encoding and
-- decoding indexer column keys and values to/from
-- 'ByteString'. Uses CBOR via @cborg@ for
-- structured types. Ledger types ('TxIn', 'Coin',
-- 'KeyHash') are serialized to bytes via
-- @cardano-ledger-binary@ and embedded as CBOR
-- byte strings. Trie columns use identity codecs
-- (raw 'ByteString' passthrough).
module Cardano.MPFS.Indexer.Codecs
    ( -- * Column codecs
      allCodecs

      -- * Individual prisms
    , tokenIdPrism
    , tokenStatePrism
    , txInPrism
    , requestPrism
    , unitPrism
    , checkpointPrism
    , rawBytesPrism
    ) where

import Cardano.Ledger.Binary
    ( DecCBOR
    , EncCBOR
    , Version
    , decodeFull
    , natVersion
    , serialize
    )
import Cardano.Ledger.Mary.Value (AssetName (..))
import Codec.CBOR.Decoding
    ( Decoder
    , decodeBytes
    , decodeInteger
    , decodeListLen
    , decodeListLenOf
    , decodeWord8
    )
import Codec.CBOR.Encoding
    ( Encoding
    , encodeBytes
    , encodeInteger
    , encodeListLen
    , encodeWord8
    )
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Control.Lens (Prism', prism')
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Database.KV.Transaction
    ( Codecs (..)
    , DMap
    , DSum ((:=>))
    , fromPairList
    )

import Cardano.MPFS.Indexer.Columns
    ( AllColumns (..)
    , CageCheckpoint (..)
    )
import Cardano.MPFS.Types
    ( BlockId (..)
    , Operation (..)
    , Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    , TxIn
    )

-- | Codecs for all indexer column families.
allCodecs :: DMap AllColumns Codecs
allCodecs =
    fromPairList
        [ CageTokens
            :=> Codecs
                { keyCodec = tokenIdPrism
                , valueCodec = tokenStatePrism
                }
        , CageRequests
            :=> Codecs
                { keyCodec = txInPrism
                , valueCodec = requestPrism
                }
        , CageCfg
            :=> Codecs
                { keyCodec = unitPrism
                , valueCodec = checkpointPrism
                }
        , TrieNodes
            :=> Codecs
                { keyCodec = rawBytesPrism
                , valueCodec = rawBytesPrism
                }
        , TrieKV
            :=> Codecs
                { keyCodec = rawBytesPrism
                , valueCodec = rawBytesPrism
                }
        ]

-- | Encode/decode 'TokenId' as raw asset name
-- bytes. No CBOR wrapping — the 'AssetName'
-- 'ShortByteString' is used directly.
tokenIdPrism :: Prism' ByteString TokenId
tokenIdPrism =
    prism'
        ( \(TokenId (AssetName sbs)) ->
            SBS.fromShort sbs
        )
        ( Just
            . TokenId
            . AssetName
            . SBS.toShort
        )

-- | Encode/decode 'TokenState' as a 5-element CBOR
-- list: @[owner, root, maxFee, processTime,
-- retractTime]@. Ledger types are embedded as
-- sub-encoded byte strings.
tokenStatePrism :: Prism' ByteString TokenState
tokenStatePrism = prism' enc dec
  where
    enc TokenState{..} =
        toStrictByteString
            $ encodeListLen 5
                <> encodeBytes
                    (ledgerEnc owner)
                <> encodeBytes (unRoot root)
                <> encodeBytes
                    (ledgerEnc maxFee)
                <> encodeInteger processTime
                <> encodeInteger retractTime
    dec = decodeCBOR $ do
        decodeListLenOf 5
        owner' <- ledgerDec =<< decodeBytes
        root' <- Root <$> decodeBytes
        maxFee' <- ledgerDec =<< decodeBytes
        processTime' <- decodeInteger
        retractTime' <- decodeInteger
        pure
            TokenState
                { owner = owner'
                , root = root'
                , maxFee = maxFee'
                , processTime = processTime'
                , retractTime = retractTime'
                }

-- | Encode/decode 'TxIn' via its ledger CBOR
-- instances.
txInPrism :: Prism' ByteString TxIn
txInPrism = prism' ledgerEnc ledgerDecMaybe

-- | Encode/decode 'Request' as a 6-element CBOR
-- list: @[token, owner, key, op, fee,
-- submittedAt]@.
requestPrism :: Prism' ByteString Request
requestPrism = prism' enc dec
  where
    enc Request{..} =
        toStrictByteString
            $ encodeListLen 6
                <> encodeBytes
                    (encTokenId requestToken)
                <> encodeBytes
                    (ledgerEnc requestOwner)
                <> encodeBytes requestKey
                <> encodeOperation
                    requestValue
                <> encodeBytes
                    (ledgerEnc requestFee)
                <> encodeInteger
                    requestSubmittedAt
    dec = decodeCBOR $ do
        decodeListLenOf 6
        requestToken <-
            decTokenId =<< decodeBytes
        requestOwner <-
            ledgerDec =<< decodeBytes
        requestKey <- decodeBytes
        requestValue <- decodeOperation
        requestFee <-
            ledgerDec =<< decodeBytes
        requestSubmittedAt <-
            decodeInteger
        pure Request{..}

-- | Encode/decode unit key as empty bytes.
unitPrism :: Prism' ByteString ()
unitPrism =
    prism' (const mempty) (const (Just ()))

-- | Encode/decode 'CageCheckpoint' as a 2-element
-- CBOR list: @[slot, blockId]@.
checkpointPrism :: Prism' ByteString CageCheckpoint
checkpointPrism = prism' enc dec
  where
    enc CageCheckpoint{..} =
        toStrictByteString
            $ encodeListLen 2
                <> encodeBytes
                    (ledgerEnc checkpointSlot)
                <> encodeBytes
                    (unBlockId checkpointBlockId)
    dec = decodeCBOR $ do
        decodeListLenOf 2
        s <- ledgerDec =<< decodeBytes
        b <- BlockId <$> decodeBytes
        pure
            CageCheckpoint
                { checkpointSlot = s
                , checkpointBlockId = b
                }

-- | Identity prism for raw 'ByteString' columns
-- (trie nodes and key-value pairs). No encoding or
-- decoding — bytes pass through unchanged.
rawBytesPrism :: Prism' ByteString ByteString
rawBytesPrism = prism' id Just

-- --------------------------------------------------------
-- Ledger serialization bridge
-- --------------------------------------------------------

-- | Protocol version for ledger CBOR encoding.
ledgerVer :: Version
ledgerVer = natVersion @2

-- | Serialize a ledger type to strict bytes.
ledgerEnc :: (EncCBOR a) => a -> ByteString
ledgerEnc =
    BSL.toStrict . serialize ledgerVer

-- | Deserialize a ledger type from strict bytes,
-- failing in the 'Decoder' monad on error.
ledgerDec
    :: (DecCBOR a) => ByteString -> Decoder s a
ledgerDec bs =
    case decodeFull ledgerVer (BSL.fromStrict bs) of
        Left err -> fail (show err)
        Right a -> pure a

-- | Deserialize a ledger type, returning 'Maybe'.
ledgerDecMaybe
    :: (DecCBOR a) => ByteString -> Maybe a
ledgerDecMaybe bs =
    case decodeFull ledgerVer (BSL.fromStrict bs) of
        Left _ -> Nothing
        Right a -> Just a

-- --------------------------------------------------------
-- TokenId helpers
-- --------------------------------------------------------

encTokenId :: TokenId -> ByteString
encTokenId (TokenId (AssetName sbs)) =
    SBS.fromShort sbs

decTokenId :: ByteString -> Decoder s TokenId
decTokenId bs =
    pure $ TokenId $ AssetName $ SBS.toShort bs

-- --------------------------------------------------------
-- Operation CBOR helpers
-- --------------------------------------------------------

-- | Tag: 0 = Insert, 1 = Delete, 2 = Update.
encodeOperation :: Operation -> Encoding
encodeOperation = \case
    Insert v ->
        encodeListLen 2
            <> encodeWord8 0
            <> encodeBytes v
    Delete v ->
        encodeListLen 2
            <> encodeWord8 1
            <> encodeBytes v
    Update v1 v2 ->
        encodeListLen 3
            <> encodeWord8 2
            <> encodeBytes v1
            <> encodeBytes v2

decodeOperation :: Decoder s Operation
decodeOperation = do
    len <- decodeListLen
    tag <- decodeWord8
    case (tag, len) of
        (0, 2) -> Insert <$> decodeBytes
        (1, 2) -> Delete <$> decodeBytes
        (2, 3) ->
            Update
                <$> decodeBytes
                <*> decodeBytes
        _ -> fail "Unknown Operation tag"

-- --------------------------------------------------------
-- CBOR decode helper
-- --------------------------------------------------------

-- | Run a CBOR decoder on a strict 'ByteString'.
decodeCBOR
    :: (forall s. Decoder s a)
    -> ByteString
    -> Maybe a
decodeCBOR decoder bs =
    case deserialiseFromBytes
        decoder
        (BSL.fromStrict bs) of
        Right (rest, a)
            | BSL.null rest -> Just a
        _ -> Nothing
