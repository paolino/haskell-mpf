{-# LANGUAGE OverloadedStrings #-}

module MPF.HashesSpec (spec) where

import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import MPF.Hashes
    ( MPFHash
    , computeLeafHash
    , computeMerkleRoot
    , merkleProof
    , mkMPFHash
    , nullHash
    , parseMPFHash
    , renderMPFHash
    )
import MPF.Interface (HexDigit (..))
import MPF.Test.Lib (genMPFHash)
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , choose
    , forAll
    , property
    , vectorOf
    , (===)
    , (==>)
    )

spec :: Spec
spec = describe "MPF.Hashes" $ do
    describe "nullHash" $ do
        it "is 32 bytes of zeros" $ do
            renderMPFHash nullHash `shouldSatisfy` \bs ->
                all (== 0) (B.unpack bs)

    describe "mkMPFHash" $ do
        it "produces 32-byte hashes" $ do
            let h = mkMPFHash "hello"
            B.length (renderMPFHash h) `shouldBe` 32

        it "is deterministic" $ do
            mkMPFHash "test" `shouldBe` mkMPFHash "test"

        it "produces different hashes for different inputs" $ do
            mkMPFHash "a" `shouldNotBe` mkMPFHash "b"

    describe "computeLeafHash" $ do
        it "produces different hashes for different suffixes" $ do
            let v = mkMPFHash "value"
                h1 = computeLeafHash [] v
                h2 = computeLeafHash [HexDigit 1] v
            h1 `shouldNotBe` h2

        it "handles empty suffix" $ do
            let v = mkMPFHash "value"
                h = computeLeafHash [] v
            B.length (renderMPFHash h) `shouldBe` 32

    describe "computeMerkleRoot" $ do
        it "handles all Nothing (sparse array)" $ do
            let result = computeMerkleRoot (replicate 16 Nothing)
            B.length (renderMPFHash result) `shouldBe` 32

        it "handles single element" $ do
            let h = mkMPFHash "child"
                children = Just h : replicate 15 Nothing
                result = computeMerkleRoot children
            B.length (renderMPFHash result) `shouldBe` 32

    describe "properties" $ do
        it "mkMPFHash always produces 32 bytes"
            $ forAll genArbitraryBS
            $ \bs ->
                B.length (renderMPFHash (mkMPFHash bs)) === 32

        it "parseMPFHash . renderMPFHash = Just"
            $ forAll genMPFHash
            $ \h ->
                parseMPFHash (renderMPFHash h) === Just h

        it "parseMPFHash rejects non-32-byte inputs"
            $ forAll genNon32ByteBS
            $ \bs ->
                parseMPFHash bs === Nothing

        it "computeLeafHash output is always 32 bytes" $ property $ \suffix ->
            forAll genMPFHash $ \val ->
                B.length (renderMPFHash (computeLeafHash suffix val)) === 32

        it "computeMerkleRoot position sensitivity"
            $ forAll ((,) <$> genMPFHash <*> genMPFHash)
            $ \(h1, h2) ->
                h1
                    /= h2
                    ==> let children1 = Just h1 : Just h2 : replicate 14 Nothing
                            children2 = Just h2 : Just h1 : replicate 14 Nothing
                        in  computeMerkleRoot children1 /= computeMerkleRoot children2

        it "merkleProof reconstruction matches computeMerkleRoot"
            $ forAll genSparseChildren
            $ \(children, idx) ->
                let root = computeMerkleRoot children
                    proof = merkleProof children idx
                    padded = take 16 $ children ++ repeat Nothing
                    element = fromMaybe nullHash (padded !! idx)
                    reconstructed = reconstructFromProof element proof idx
                in  reconstructed === root

genArbitraryBS :: Gen B.ByteString
genArbitraryBS = B.pack <$> vectorOf 10 (choose (0, 255))

genNon32ByteBS :: Gen B.ByteString
genNon32ByteBS = do
    len <- choose (0, 64)
    if len == 32
        then B.pack <$> vectorOf 31 (choose (0, 255))
        else B.pack <$> vectorOf len (choose (0, 255))

-- | Generate a sparse 16-element child array with some slots filled
genSparseChildren :: Gen ([Maybe MPFHash], Int)
genSparseChildren = do
    idx <- choose (0, 15)
    nFilled <- choose (1, 16)
    filledPositions <- vectorOf nFilled (choose (0, 15))
    hashes <- vectorOf 16 genMPFHash
    let children =
            [ if i `elem` filledPositions
                then Just (hashes !! i)
                else Nothing
            | i <- [0 .. 15]
            ]
    pure (children, idx)

-- | Reconstruct merkle root from element, proof hashes, and position
reconstructFromProof :: MPFHash -> [MPFHash] -> Int -> MPFHash
reconstructFromProof element proofList me =
    foldl step element (zip (reverse proofList) [1, 2, 4, 8 :: Int])
  where
    step acc (sibling, pivot) =
        if even (me `div` pivot)
            then mkMPFHash (renderMPFHash acc <> renderMPFHash sibling)
            else mkMPFHash (renderMPFHash sibling <> renderMPFHash acc)
