{-# LANGUAGE OverloadedStrings #-}

module MPF.InterfaceSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.Word (Word8)
import MPF.Interface
    ( HexDigit (..)
    , HexIndirect (..)
    , byteStringToHexKey
    , compareHexKeys
    , getHexIndirect
    , getHexKey
    , hexKeyToByteString
    , mkHexDigit
    , putHexIndirect
    , putHexKey
    )
import MPF.Test.Lib (genHexKey)
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , choose
    , elements
    , forAll
    , property
    , vectorOf
    , (===)
    )

spec :: Spec
spec = describe "MPF.Interface" $ do
    describe "HexDigit" $ do
        it "mkHexDigit succeeds for valid values" $ do
            mkHexDigit 0 `shouldBe` Just (HexDigit 0)
            mkHexDigit 15 `shouldBe` Just (HexDigit 15)
            mkHexDigit 8 `shouldBe` Just (HexDigit 8)

        it "mkHexDigit fails for invalid values" $ do
            mkHexDigit 16 `shouldBe` Nothing
            mkHexDigit 255 `shouldBe` Nothing

    describe "HexKey conversion" $ do
        it "byteStringToHexKey converts bytes to nibbles" $ do
            byteStringToHexKey "\x12" `shouldBe` [HexDigit 1, HexDigit 2]
            byteStringToHexKey "\xab" `shouldBe` [HexDigit 10, HexDigit 11]
            byteStringToHexKey "\x00" `shouldBe` [HexDigit 0, HexDigit 0]
            byteStringToHexKey "\xff" `shouldBe` [HexDigit 15, HexDigit 15]

        it "hexKeyToByteString converts nibbles back to bytes" $ do
            hexKeyToByteString [HexDigit 1, HexDigit 2] `shouldBe` "\x12"
            hexKeyToByteString [HexDigit 10, HexDigit 11] `shouldBe` "\xab"

        it "roundtrips correctly for even-length keys" $ do
            let bs = "\x12\x34\x56" :: ByteString
            hexKeyToByteString (byteStringToHexKey bs) `shouldBe` bs

    describe "compareHexKeys" $ do
        it "finds common prefix" $ do
            let k1 = [HexDigit 1, HexDigit 2, HexDigit 3]
                k2 = [HexDigit 1, HexDigit 2, HexDigit 4]
            compareHexKeys k1 k2
                `shouldBe` ([HexDigit 1, HexDigit 2], [HexDigit 3], [HexDigit 4])

        it "handles empty common prefix" $ do
            let k1 = [HexDigit 1, HexDigit 2]
                k2 = [HexDigit 3, HexDigit 4]
            compareHexKeys k1 k2
                `shouldBe` ([], [HexDigit 1, HexDigit 2], [HexDigit 3, HexDigit 4])

        it "handles identical keys" $ do
            let k = [HexDigit 1, HexDigit 2]
            compareHexKeys k k `shouldBe` (k, [], [])

        it "handles one key being prefix of another" $ do
            let k1 = [HexDigit 1, HexDigit 2]
                k2 = [HexDigit 1, HexDigit 2, HexDigit 3]
            compareHexKeys k1 k2
                `shouldBe` ([HexDigit 1, HexDigit 2], [], [HexDigit 3])

    describe "properties" $ do
        it "hexKeyToByteString . byteStringToHexKey = id (even-length)"
            $ forAll genEvenByteString
            $ \bs ->
                hexKeyToByteString (byteStringToHexKey bs) === bs

        it "compareHexKeys decomposition: common ++ rest1 = k1"
            $ forAll ((,) <$> genHexKey <*> genHexKey)
            $ \(k1, k2) ->
                let (common, rest1, _rest2) = compareHexKeys k1 k2
                in  common ++ rest1 === k1

        it "compareHexKeys decomposition: common ++ rest2 = k2"
            $ forAll ((,) <$> genHexKey <*> genHexKey)
            $ \(k1, k2) ->
                let (common, _rest1, rest2) = compareHexKeys k1 k2
                in  common ++ rest2 === k2

        it "compareHexKeys symmetry: same common prefix" $ property $ \k1 k2 ->
            let (common1, _, _) = compareHexKeys k1 k2
                (common2, _, _) = compareHexKeys k2 k1
            in  common1 === common2

        it "putHexKey/getHexKey serialization roundtrip"
            $ forAll genHexKey
            $ \k ->
                evalGetM getHexKey (evalPutM (putHexKey k)) === Just k

        it "putHexIndirect/getHexIndirect serialization roundtrip"
            $ forAll genHexIndirectBS
            $ \hi ->
                evalGetM getHexIndirect (evalPutM (putHexIndirect hi))
                    === Just hi

        it "hexKeyPrism law: decode . encode = Just"
            $ forAll genHexKey
            $ \k ->
                evalGetM getHexKey (evalPutM (putHexKey k)) === Just k

        it "mkHexDigit succeeds exactly for [0..15]" $ property $ \(w :: Word8) ->
            case mkHexDigit w of
                Just (HexDigit n) -> w < 16 && n == w
                Nothing -> w >= 16

genEvenByteString :: Gen ByteString
genEvenByteString = do
    n <- choose (0, 32)
    B.pack <$> vectorOf n (choose (0, 255))

genHexIndirectBS :: Gen (HexIndirect ByteString)
genHexIndirectBS = do
    key <- genHexKey
    val <- B.pack . (: []) <$> choose (0 :: Word8, 255)
    isLeaf <- elements [True, False]
    pure HexIndirect{hexJump = key, hexValue = val, hexIsLeaf = isLeaf}
