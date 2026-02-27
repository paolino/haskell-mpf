{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Serialize.ExtraSpec
-- Description : Tests for cereal serialization helpers
--
-- Tests for the cereal convenience functions:
--
-- * evalPutM discards result and returns bytes
-- * unsafeEvalGet roundtrips with PutM
-- * evalGetM returns Nothing on bad input
-- * Serialization roundtrip properties
module Data.Serialize.ExtraSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize
    ( getByteString
    , getWord16be
    , getWord8
    , putByteString
    , putWord16be
    , putWord8
    )
import Data.Serialize.Extra
    ( evalGetM
    , evalPutM
    , unsafeEvalGet
    )
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , choose
    , forAll
    , property
    , vectorOf
    , (===)
    )

spec :: Spec
spec = describe "Data.Serialize.Extra" $ do
    describe "evalPutM" $ do
        it "serializes a byte" $ do
            let bs = evalPutM (putWord8 42)
            B.length bs `shouldBe` 1
            B.head bs `shouldBe` 42

        it "serializes multiple bytes" $ do
            let bs = evalPutM $ do
                    putWord8 1
                    putWord8 2
                    putWord8 3
            B.length bs `shouldBe` 3

    describe "unsafeEvalGet" $ do
        it "parses a single byte" $ do
            let bs = B.singleton 99
                result = unsafeEvalGet getWord8 bs
            result `shouldBe` 99

        it "roundtrips with evalPutM" $ do
            let original = 123 :: Int
                bs = evalPutM (putWord8 $ fromIntegral original)
                result = unsafeEvalGet getWord8 bs
            result `shouldBe` fromIntegral original

    describe "evalGetM" $ do
        it "returns Just on valid input" $ do
            let bs = B.singleton 77
                result = evalGetM getWord8 bs
            result `shouldBe` Just 77

        it "returns Nothing on empty input" $ do
            let result = evalGetM getWord8 B.empty
            result `shouldBe` Nothing

    describe "properties" $ do
        it "Word8 roundtrips through put/get" $ property $ \(w :: Word8) ->
            evalGetM getWord8 (evalPutM (putWord8 w)) === Just w

        it "Word16be roundtrips through put/get" $ property $ \w ->
            evalGetM getWord16be (evalPutM (putWord16be w)) === Just w

        it "ByteString roundtrips through put/get"
            $ forAll genByteString
            $ \bs ->
                let encoded = evalPutM $ do
                        putWord16be (fromIntegral $ B.length bs)
                        putByteString bs
                    decoded = evalGetM (getWord16be >>= getByteString . fromIntegral) encoded
                in  decoded === Just bs

        it "unsafeEvalGet agrees with evalGetM on valid input" $ property $ \(w :: Word8) ->
            let bs = evalPutM (putWord8 w)
            in  unsafeEvalGet getWord8 bs === w

        it "evalGetM returns Nothing on truncated Word16be" $ property $ \(w :: Word8) ->
            evalGetM getWord16be (B.singleton w) === Nothing

genByteString :: Gen ByteString
genByteString = B.pack <$> vectorOf 16 (choose (0, 255))
