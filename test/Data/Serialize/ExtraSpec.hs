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
module Data.Serialize.ExtraSpec (spec) where

import Data.ByteString qualified as B
import Data.Serialize (getWord8, putWord8)
import Data.Serialize.Extra
    ( evalGetM
    , evalPutM
    , unsafeEvalGet
    )
import Test.Hspec

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
