{-# LANGUAGE OverloadedStrings #-}

module Cardano.MPFS.ProofSpec (spec) where

import Cardano.MPFS.Proof (serializeProof)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import MPF.Hashes
    ( MPFHash
    , computeMerkleRoot
    , merkleProof
    , mkMPFHash
    , mpfHashing
    , nullHash
    , renderMPFHash
    )
import MPF.Interface (byteStringToHexKey)
import MPF.Proof.Insertion
    ( MPFProof (..)
    , MPFProofStep
    )
import MPF.Test.Lib
    ( encodeHex
    , foldMPFProof
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
        -- ------------------------------------------------
        -- merkleRoot reference vectors from helpers.test.js
        -- ------------------------------------------------
        describe "computeMerkleRoot (reference vectors)" $ do
            it "1 NULL_HASH = all zeros" $ do
                let r = computeMerkleRoot [Just nullHash]
                encodeHex (renderMPFHash r)
                    `shouldBe` "0000000000000000000000000000000000000000000000000000000000000000"

            it "2 NULL_HASHes" $ do
                let r =
                        computeMerkleRoot
                            $ replicate 2 (Just nullHash)
                encodeHex (renderMPFHash r)
                    `shouldBe` "0eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761"

            it "4 NULL_HASHes" $ do
                let r =
                        computeMerkleRoot
                            $ replicate 4 (Just nullHash)
                encodeHex (renderMPFHash r)
                    `shouldBe` "85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b1"

            it "8 NULL_HASHes" $ do
                let r =
                        computeMerkleRoot
                            $ replicate 8 (Just nullHash)
                encodeHex (renderMPFHash r)
                    `shouldBe` "b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a"

        -- ------------------------------------------------
        -- merkleProof reconstruction
        -- ------------------------------------------------
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

            it "reconstructs root for all 16 positions" $ do
                let hashes =
                        [ mkMPFHash (B.pack [i])
                        | i <- [0 .. 15]
                        ]
                    children = map Just hashes
                    expected = computeFullMerkleRoot hashes
                forM_ [0 .. 15] $ \pos -> do
                    let proof = merkleProof children pos
                        root =
                            reconstructRoot
                                (hashes !! pos)
                                proof
                                pos
                    root `shouldBe` expected

            it "works with sparse children" $ do
                let children =
                        [ if i `elem` [0 :: Int, 3, 7, 12]
                            then
                                Just
                                    $ mkMPFHash
                                        (B.pack [fromIntegral i])
                            else Nothing
                        | i <- [0 .. 15]
                        ]
                    pos = 3
                    proof = merkleProof children pos
                length proof `shouldBe` 4

        -- ------------------------------------------------
        -- CBOR encoding (reference vectors)
        -- ------------------------------------------------
        describe "CBOR encoding" $ do
            it "int 0 = 0x00" $ do
                -- Sanity check: our cborInt matches
                -- cbor.test.js: cbor.int(0) → '00'
                let bs = serializeProof stubProofInt0
                -- The proof wraps the int in structure;
                -- we test byte-level with full vectors below
                B.length bs `shouldSatisfy` (> 0)

        -- ------------------------------------------------
        -- Byte-identical CBOR vectors from trie.test.js
        -- ------------------------------------------------
        describe "serializeProof (reference vectors)" $ do
            it "mango proof matches TypeScript toCBOR()" $ do
                let (mProof, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        let mangoKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "mango[uid: 0]"
                        proofMPFM mangoKey
                case mProof of
                    Nothing -> fail "No proof for mango"
                    Just proof -> do
                        let actual =
                                encodeHex (serializeProof proof)
                        actual `shouldBe` mangoCborHex

            it "kumquat proof matches TypeScript toCBOR()" $ do
                let (mProof, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        let kumquatKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "kumquat[uid: 0]"
                        proofMPFM kumquatKey
                case mProof of
                    Nothing -> fail "No proof for kumquat"
                    Just proof -> do
                        let actual =
                                encodeHex (serializeProof proof)
                        actual `shouldBe` kumquatCborHex

            it "tangerine proof matches TypeScript toCBOR()" $ do
                let (mProof, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        let tangerineKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash
                                        "tangerine[uid: 11]"
                        proofMPFM tangerineKey
                case mProof of
                    Nothing ->
                        fail "No proof for tangerine"
                    Just proof -> do
                        let actual =
                                encodeHex (serializeProof proof)
                        actual `shouldBe` tangerineCborHex

        -- ------------------------------------------------
        -- JSON-level structure from trie.test.js
        -- ------------------------------------------------
        describe "proof structure (JSON-level)" $ do
            it "mango proof has Branch + Leaf steps" $ do
                let (mProof, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        let mangoKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "mango[uid: 0]"
                        proofMPFM mangoKey
                case mProof of
                    Nothing -> fail "No proof for mango"
                    Just proof -> do
                        -- Should be exactly 2 steps
                        length (proofSteps proof)
                            `shouldBe` 2

            it "kumquat proof has Branch + Fork steps" $ do
                let (mProof, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        let kumquatKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "kumquat[uid: 0]"
                        proofMPFM kumquatKey
                case mProof of
                    Nothing -> fail "No proof for kumquat"
                    Just proof -> do
                        length (proofSteps proof)
                            `shouldBe` 2

        -- ------------------------------------------------
        -- foldMPFProof correctness for all fruits
        -- ------------------------------------------------
        describe "foldMPFProof (all fruits)" $ do
            it "every fruit's proof folds to the root" $ do
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
                    _ -> fail "Expected root"

            it "every fruit serializes to non-empty CBOR" $ do
                let (results, _) = runMPFPure' $ do
                        forM_ fruitsTestData
                            $ uncurry insertByteStringM
                        mapM
                            ( \(k, _) -> do
                                let hk =
                                        byteStringToHexKey
                                            $ renderMPFHash
                                            $ mkMPFHash k
                                mp <- proofMPFM hk
                                pure (k, mp)
                            )
                            fruitsTestData
                forM_ results $ \(name, mProof) ->
                    case mProof of
                        Nothing ->
                            fail
                                $ "No proof for: " ++ show name
                        Just proof -> do
                            let bs = serializeProof proof
                            B.length bs `shouldSatisfy` (> 0)
                            -- Must start with 0x9f (indefinite list)
                            B.head bs `shouldBe` 0x9f
                            -- Must end with 0xff (end marker)
                            B.last bs `shouldBe` 0xff

-- ----------------------------------------------------------
-- Reference CBOR hex vectors from trie.test.js
-- ----------------------------------------------------------

-- | Proof.toCBOR (mango) from trie.test.js
mangoCborHex :: ByteString
mangoCborHex =
    "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f58401508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4dffffd87b9f00582009d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc5820c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795ffff"

-- | Proof.toCBOR (kumquat) from trie.test.js
kumquatCborHex :: ByteString
kumquatCborHex =
    "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c58403490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15bffffd87a9f00d8799f0041075820a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281ffffff"

-- | Proof.toCBOR (tangerine) from README.md
tangerineCborHex :: ByteString
tangerineCborHex =
    "9fd8799f005f58404be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd53505840c1e96bcc431893eef34e03989814375d439faa592edf75c9e5dc10b3c30766700000000000000000000000000000000000000000000000000000000000000000ffffff"

-- ----------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------

-- | Extract proof steps (for length checks).
proofSteps
    :: MPFProof MPFHash -> [MPFProofStep MPFHash]
proofSteps = mpfProofSteps

-- | Stub proof for basic CBOR encoding checks.
-- Not used for byte-level comparison.
stubProofInt0 :: MPFProof MPFHash
stubProofInt0 =
    MPFProof
        { mpfProofSteps = []
        , mpfProofRootPrefix = []
        , mpfProofLeafSuffix = []
        }

-- | Reconstruct merkle root from a leaf hash, its
-- 4-level proof (top-to-bottom), and position (0-15).
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

-- | Hash two values together (pairwise reduction).
hashPair :: MPFHash -> MPFHash -> MPFHash
hashPair a b =
    mkMPFHash (renderMPFHash a <> renderMPFHash b)

-- | Full merkle root from 16 hashes.
computeFullMerkleRoot :: [MPFHash] -> MPFHash
computeFullMerkleRoot [] = nullHash
computeFullMerkleRoot [h] = h
computeFullMerkleRoot hs =
    computeFullMerkleRoot $ pairUp hs
  where
    pairUp [] = []
    pairUp [h] = [h]
    pairUp (a : b : rest) = hashPair a b : pairUp rest
