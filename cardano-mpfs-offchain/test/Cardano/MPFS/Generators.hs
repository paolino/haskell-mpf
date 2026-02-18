{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Cardano.MPFS.Generators
-- Description : QuickCheck generators for MPFS domain types
-- License     : Apache-2.0
module Cardano.MPFS.Generators
    ( -- * Generators
      genTokenId
    , genRoot
    , genKeyHash
    , genTxIn
    , genTokenState
    , genRequest
    , genOperation
    , genSlotNo
    , genBlockId
    ) where

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Maybe (fromJust)
import Data.Word (Word16, Word64)
import Numeric (showHex)

import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , vectorOf
    )

import Cardano.Crypto.Hash
    ( Blake2b_224
    , Blake2b_256
    , hashFromStringAsHex
    )
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))

import Cardano.MPFS.Types
    ( AssetName (..)
    , BlockId (..)
    , Coin (..)
    , Operation (..)
    , Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    )

-- | Generate a hex string of given byte-length
-- (each byte = 2 hex chars).
genHex :: Int -> Gen String
genHex n =
    concatMap (\b -> padHex (showHex b ""))
        <$> vectorOf n (choose (0 :: Int, 255))
  where
    padHex [c] = ['0', c]
    padHex s = s

-- | Generate a 'TokenId' wrapping a random
-- 'AssetName' (1-32 bytes).
genTokenId :: Gen TokenId
genTokenId = do
    len <- choose (1, 32)
    bs <- SBS.pack <$> vectorOf len arbitrary
    pure $ TokenId (AssetName bs)

-- | Generate a 32-byte 'Root' hash.
genRoot :: Gen Root
genRoot = Root . BS.pack <$> vectorOf 32 arbitrary

-- | Generate a 'KeyHash' ''Payment' from a random
-- 28-byte Blake2b-224 hash.
genKeyHash :: Gen (KeyHash 'Payment)
genKeyHash = do
    hex <- genHex 28
    pure
        $ KeyHash
        $ fromJust
        $ hashFromStringAsHex @Blake2b_224 hex

-- | Generate a 'TxIn' from a random 'TxId' and
-- small index.
genTxIn :: Gen TxIn
genTxIn = do
    hex <- genHex 32
    let txId =
            TxId
                $ unsafeMakeSafeHash
                $ fromJust
                $ hashFromStringAsHex @Blake2b_256 hex
    ix <- TxIx <$> choose (0 :: Word16, 10)
    pure $ TxIn txId ix

-- | Generate a 'Coin' value.
genCoin :: Gen Coin
genCoin = Coin <$> choose (0, 10_000_000)

-- | Generate a 'TokenState' with random owner,
-- root, and maxFee.
genTokenState :: Gen TokenState
genTokenState =
    TokenState
        <$> genKeyHash
        <*> genRoot
        <*> genCoin

-- | Generate a random 'Operation'.
genOperation :: Gen Operation
genOperation = do
    bs1 <- BS.pack <$> vectorOf 8 arbitrary
    bs2 <- BS.pack <$> vectorOf 8 arbitrary
    elements
        [ Insert bs1
        , Delete bs1
        , Update bs1 bs2
        ]

-- | Generate a 'Request' for a given 'TokenId'.
genRequest :: TokenId -> Gen Request
genRequest tid = do
    owner <- genKeyHash
    k <- BS.pack <$> vectorOf 8 arbitrary
    op <- genOperation
    fee <- genCoin
    submittedAt <-
        fromIntegral
            <$> choose (0 :: Int, 2_000_000_000)
    pure
        Request
            { requestToken = tid
            , requestOwner = owner
            , requestKey = k
            , requestValue = op
            , requestFee = fee
            , requestSubmittedAt = submittedAt
            }

-- | Generate a 'SlotNo'.
genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> choose (0 :: Word64, 1000000)

-- | Generate a 'BlockId' (32-byte hash).
genBlockId :: Gen BlockId
genBlockId =
    BlockId . BS.pack <$> vectorOf 32 arbitrary
