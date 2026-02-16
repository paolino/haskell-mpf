{-# LANGUAGE OverloadedStrings #-}

module Cardano.MPFS.ProofSpec (spec) where

import Cardano.MPFS.Proof (serializeProof)
import Control.Monad (forM_)
import Data.ByteString qualified as B
import MPF.Hashes
    ( MPFHash
    , merkleProof
    , mkMPFHash
    , mpfHashing
    , nullHash
    , renderMPFHash
    )
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( foldMPFProof
    , fruitsTestData
    , getRootHashM
    , insertByteStringM
    , proofMPFM
    , runMPFPure'
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

spec :: Spec
spec = do
    describe "Cardano.MPFS.Proof" $ do
        describe "merkleProof" $ do
            it "produces 4 intermediate hashes" $ do
                let children =
                        map
                            Just
                            [ mkMPFHash (B.pack [i])
                            | i <- [0 .. 15]
                            ]
                    result = merkleProof children 0
                length result `shouldBe` 4

            it "reconstructs root from proof and element" $ do
                let hashes =
                        [ mkMPFHash (B.pack [i])
                        | i <- [0 .. 15]
                        ]
                    children = map Just hashes
                    pos = 5
                    proof = merkleProof children pos
                    -- Reconstruct root from proof
                    root = reconstructRoot (hashes !! pos) proof pos
                    -- Compare with direct merkle root
                    expected = computeFullMerkleRoot hashes
                root `shouldBe` expected

            it "works with sparse children" $ do
                let children =
                        [ if i `elem` [0 :: Int, 3, 7, 12]
                            then Just $ mkMPFHash (B.pack [fromIntegral i])
                            else Nothing
                        | i <- [0 .. 15]
                        ]
                    pos = 3
                    proof = merkleProof children pos
                length proof `shouldBe` 4

            it "produces correct proof for position 0" $ do
                let hashes =
                        [ mkMPFHash (B.pack [i])
                        | i <- [0 .. 15]
                        ]
                    children = map Just hashes
                    proof = merkleProof children 0
                    root = reconstructRoot (hashes !! 0) proof 0
                    expected = computeFullMerkleRoot hashes
                root `shouldBe` expected

            it "produces correct proof for position 15" $ do
                let hashes =
                        [ mkMPFHash (B.pack [i])
                        | i <- [0 .. 15]
                        ]
                    children = map Just hashes
                    proof = merkleProof children 15
                    root = reconstructRoot (hashes !! 15) proof 15
                    expected = computeFullMerkleRoot hashes
                root `shouldBe` expected

            it "produces correct proof for all positions" $ do
                let hashes =
                        [ mkMPFHash (B.pack [i])
                        | i <- [0 .. 15]
                        ]
                    children = map Just hashes
                    expected = computeFullMerkleRoot hashes
                forM_ [0 .. 15] $ \pos -> do
                    let proof = merkleProof children pos
                        root = reconstructRoot (hashes !! pos) proof pos
                    root `shouldBe` expected

        describe "serializeProof" $ do
            it "produces non-empty bytes for single element" $ do
                let key =
                        byteStringToHexKey
                            $ renderMPFHash
                            $ mkMPFHash "hello"
                    (mProof, _) = runMPFPure' $ do
                        insertByteStringM "hello" "world"
                        proofMPFM key
                case mProof of
                    Nothing -> fail "Expected proof"
                    Just proof -> do
                        let bs = serializeProof proof
                        B.length bs `shouldSatisfy` (> 0)

            it "produces non-empty bytes for fruits dataset" $ do
                let (mProof, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        let appleKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "apple[uid: 58]"
                        proofMPFM appleKey
                case mProof of
                    Nothing -> fail "Expected proof"
                    Just proof -> do
                        let bs = serializeProof proof
                        B.length bs `shouldSatisfy` (> 0)

            it "proof folds to correct root after serialization" $ do
                -- Verify that the proof structure used for
                -- serialization is the same one that folds
                -- to the correct root
                let (result, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        let appleKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "apple[uid: 58]"
                            appleValue =
                                mkMPFHash "\xf0\x9f\x8d\x8e"
                        mProof <- proofMPFM appleKey
                        mRoot <- getRootHashM
                        pure (mProof, mRoot, appleValue)
                case result of
                    (Just proof, Just root, val) -> do
                        let computed =
                                foldMPFProof mpfHashing val proof
                        computed `shouldBe` root
                        -- Also check that serialization
                        -- doesn't crash
                        let _bs = serializeProof proof
                        pure ()
                    _ -> fail "Expected proof and root"

            it "serializes proofs for all fruits" $ do
                let (results, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        mRoot <- getRootHashM
                        proofs <-
                            mapM
                                ( \(k, v) -> do
                                    let hk =
                                            byteStringToHexKey
                                                $ renderMPFHash
                                                $ mkMPFHash k
                                        hv = mkMPFHash v
                                    mp <- proofMPFM hk
                                    pure (k, hv, mp)
                                )
                                fruitsTestData
                        pure (mRoot, proofs)
                case results of
                    (Just root, proofs) ->
                        forM_ proofs
                            $ \(name, val, mProof) ->
                                case mProof of
                                    Nothing ->
                                        fail
                                            $ "No proof for: "
                                                ++ show name
                                    Just proof -> do
                                        let computed =
                                                foldMPFProof
                                                    mpfHashing
                                                    val
                                                    proof
                                        computed `shouldBe` root
                                        let bs = serializeProof proof
                                        B.length bs
                                            `shouldSatisfy` (> 0)
                    _ -> fail "Expected root"

-- ----------------------------------------------------------
-- Helpers for merkle proof reconstruction
-- ----------------------------------------------------------

-- | Reconstruct merkle root from a leaf hash, its 4-level
-- proof (top-to-bottom order), and the leaf position (0-15).
-- Works bottom-up: reverses proof, starts at pivot=1.
reconstructRoot :: MPFHash -> [MPFHash] -> Int -> MPFHash
reconstructRoot leaf proofHashes pos =
    go leaf (reverse proofHashes) pos 1
  where
    go acc [] _ _ = acc
    go acc (sibling : rest) p pivot =
        let (combined, newPos) =
                if p `mod` (pivot * 2) < pivot
                    then (hashPair acc sibling, p)
                    else (hashPair sibling acc, p)
        in  go combined rest newPos (pivot * 2)

-- | Hash two MPFHash values together (pairwise reduction)
hashPair :: MPFHash -> MPFHash -> MPFHash
hashPair a b =
    mkMPFHash (renderMPFHash a <> renderMPFHash b)

-- | Compute full merkle root from 16 hashes via pairwise
-- reduction.
computeFullMerkleRoot :: [MPFHash] -> MPFHash
computeFullMerkleRoot [] = nullHash
computeFullMerkleRoot [h] = h
computeFullMerkleRoot hs = computeFullMerkleRoot $ pairUp hs
  where
    pairUp [] = []
    pairUp [h] = [h]
    pairUp (a : b : rest) = hashPair a b : pairUp rest
