-- |
-- Module      : Cardano.MPFS.Core.Bootstrap
-- Description : CBOR bootstrap file for UTxO seeding
-- License     : Apache-2.0
--
-- Encode and decode CBOR bootstrap files for seeding a
-- fresh database with UTxO entries. A bootstrap file
-- captures a UTxO snapshot at a known chain point so
-- the indexer can start from that slot instead of
-- replaying the entire chain.
--
-- The file format is a 3-element CBOR array:
--
-- @
-- [slot :: Word64, blockHash :: bytes|null, {_ k: v, … }]
-- @
--
-- Decoding is streaming ('foldBootstrapEntries') so
-- arbitrarily large snapshots can be loaded without
-- holding the full map in memory.
module Cardano.MPFS.Core.Bootstrap
    ( -- * Header
      BootstrapHeader (..)

      -- * Encoding
    , encodeBootstrapFile

      -- * Decoding
    , foldBootstrapEntries
    ) where

import Codec.CBOR.Decoding
    ( Decoder
    , TokenType (..)
    , decodeBreakOr
    , decodeBytes
    , decodeListLenOf
    , decodeMapLenIndef
    , decodeNull
    , decodeWord64
    , peekTokenType
    )
import Codec.CBOR.Encoding
    ( Encoding
    , encodeBreak
    , encodeBytes
    , encodeListLen
    , encodeMapLenIndef
    , encodeNull
    , encodeWord64
    )
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Word (Word64)

-- | Bootstrap file header: slot number and optional
-- block hash. When the block hash is 'Nothing', a
-- lightweight discovery ChainSync is needed to learn
-- the actual block hash at the given slot.
data BootstrapHeader = BootstrapHeader
    { bootstrapSlot :: !Word64
    -- ^ Slot number at which the snapshot was taken
    , bootstrapBlockHash :: !(Maybe ByteString)
    -- ^ Block header hash, or 'Nothing' if discovery
    -- ChainSync is needed to resolve it
    }
    deriving stock (Eq, Show)

-- | Write a bootstrap CBOR file. The format is:
--
-- @
-- CBOR array(3):
--   [0] Word64         — slot number
--   [1] null | bytes   — optional block hash
--   [2] indef map      — {_ key: value, ... break }
-- @
--
-- Key-value pairs are raw bytes (pre-serialized at
-- the appropriate ledger CBOR version).
encodeBootstrapFile
    :: FilePath
    -- ^ Output file path
    -> BootstrapHeader
    -- ^ Slot and optional block hash
    -> [(ByteString, ByteString)]
    -- ^ Key-value pairs (pre-serialized CBOR)
    -> IO ()
encodeBootstrapFile fp hdr pairs =
    BSL.writeFile fp
        $ toLazyByteString encoding
  where
    encoding =
        encodeListLen 3
            <> encodeWord64 (bootstrapSlot hdr)
            <> encodeMaybeBytes
                (bootstrapBlockHash hdr)
            <> encodeMapLenIndef
            <> foldMap
                ( \(k, v) ->
                    encodeBytes k
                        <> encodeBytes v
                )
                pairs
            <> encodeBreak

-- | Stream-decode a bootstrap CBOR file. Invokes
-- @onHeader@ once with the header, then @onEntry@
-- for each key-value pair in the indefinite map.
-- Never loads the full map into memory.
foldBootstrapEntries
    :: FilePath
    -- ^ Path to the bootstrap CBOR file
    -> (BootstrapHeader -> IO ())
    -- ^ Callback invoked once with the header
    -> (ByteString -> ByteString -> IO ())
    -- ^ Callback invoked for each key-value entry
    -> IO ()
foldBootstrapEntries fp onHeader onEntry = do
    lbs <- BSL.readFile fp
    case deserialiseFromBytes decodeHdr lbs of
        Left err ->
            error
                $ "Bootstrap CBOR header: "
                    <> show err
        Right (rest, hdr) -> do
            onHeader hdr
            case deserialiseFromBytes
                decodeMapLenIndef
                rest of
                Left err ->
                    error
                        $ "Bootstrap CBOR map: "
                            <> show err
                Right (rest', ()) ->
                    go rest'
  where
    decodeHdr = do
        decodeListLenOf 3
        slot <- decodeWord64
        BootstrapHeader slot <$> decodeMaybeBytes
    go remaining =
        case deserialiseFromBytes
            decodeEntryOrBreak
            remaining of
            Left err ->
                error
                    $ "Bootstrap CBOR entry: "
                        <> show err
            Right (_, Nothing) -> pure ()
            Right (rest', Just (k, v)) -> do
                onEntry k v
                go rest'

-- -------------------------------------------------
-- Helpers
-- -------------------------------------------------

-- | Encode 'Maybe ByteString' as CBOR @null@ or
-- @bytes@.
encodeMaybeBytes :: Maybe ByteString -> Encoding
encodeMaybeBytes Nothing = encodeNull
encodeMaybeBytes (Just bs) = encodeBytes bs

-- | Decode CBOR @null@ or @bytes@ into
-- 'Maybe ByteString'.
decodeMaybeBytes
    :: Decoder s (Maybe ByteString)
decodeMaybeBytes = do
    tt <- peekTokenType
    case tt of
        TypeNull -> Nothing <$ decodeNull
        _ -> Just <$> decodeBytes

-- | Decode one map entry or the break token.
-- Returns 'Nothing' on break (end of map).
decodeEntryOrBreak
    :: Decoder s (Maybe (ByteString, ByteString))
decodeEntryOrBreak = do
    done <- decodeBreakOr
    if done
        then pure Nothing
        else
            Just
                <$> ( (,)
                        <$> decodeBytes
                        <*> decodeBytes
                    )
