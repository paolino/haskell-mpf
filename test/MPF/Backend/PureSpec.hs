{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MPF.Backend.PureSpec
-- Description : Tests for the in-memory pure backend
--
-- Unit tests and property tests for the pure in-memory MPF backend.
module MPF.Backend.PureSpec (spec) where

import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import MPF.Backend.Pure
    ( MPFInMemoryDB (..)
    , emptyMPFInMemoryDB
    , runMPFPure
    )
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( deleteMPFM
    , genKeyBytes
    , genValue
    , getRootHashM
    , insertMPFM
    , runMPFPure'
    , toHexKey
    , verifyMPFM
    )
import Test.Hspec
import Test.QuickCheck
    ( forAll
    , vectorOf
    , (===)
    , (==>)
    )

spec :: Spec
spec = describe "MPF.Backend.Pure" $ do
    describe "emptyMPFInMemoryDB" $ do
        it "has empty MPF map" $ do
            mpfInMemoryMPF emptyMPFInMemoryDB
                `shouldBe` Map.empty

        it "has empty KV map" $ do
            mpfInMemoryKV emptyMPFInMemoryDB
                `shouldBe` Map.empty

        it "has empty iterators map" $ do
            mpfInMemoryIterators emptyMPFInMemoryDB
                `shouldBe` Map.empty

    describe "runMPFPure" $ do
        it "returns result and updated database" $ do
            let key = byteStringToHexKey "test"
                value = mkMPFHash "value"
                ((), db) =
                    runMPFPure emptyMPFInMemoryDB
                        $ insertMPFM key value
            mpfInMemoryMPF db
                `shouldSatisfy` (not . Map.null)

        it "preserves state across operations" $ do
            let k1 = byteStringToHexKey "k1"
                v1 = mkMPFHash "v1"
                k2 = byteStringToHexKey "k2"
                v2 = mkMPFHash "v2"
                (mRoot, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    insertMPFM k2 v2
                    getRootHashM
            mRoot `shouldSatisfy` \case
                Just _ -> True
                Nothing -> False

        it "chains insert then delete correctly" $ do
            let key = byteStringToHexKey "ephemeral"
                value = mkMPFHash "temp"
                (mRoot, db) = runMPFPure' $ do
                    insertMPFM key value
                    deleteMPFM key
                    getRootHashM
            mRoot `shouldBe` Nothing
            -- KV column should also be empty after delete
            mpfInMemoryKV db `shouldBe` Map.empty

    describe "database isolation" $ do
        it "separate runs don't share state" $ do
            let key = byteStringToHexKey "isolated"
                value = mkMPFHash "data"
                (_, db1) = runMPFPure' $ insertMPFM key value
                (mRoot, _) =
                    runMPFPure
                        emptyMPFInMemoryDB
                        getRootHashM
            -- Fresh empty database should have no root
            mRoot `shouldBe` Nothing
            -- But the first db should have data
            mpfInMemoryMPF db1
                `shouldSatisfy` (not . Map.null)

        it "continuing from previous state works" $ do
            let k1 = byteStringToHexKey "first"
                v1 = mkMPFHash "1"
                (_, db1) = runMPFPure' $ insertMPFM k1 v1
                k2 = byteStringToHexKey "second"
                v2 = mkMPFHash "2"
                (mRoot, _) = runMPFPure db1 $ do
                    insertMPFM k2 v2
                    getRootHashM
            mRoot `shouldSatisfy` \case
                Just _ -> True
                Nothing -> False

    describe "properties" $ do
        it "sequential transactions compose correctly"
            $ forAll (vectorOf 4 ((,) <$> genKeyBytes <*> genValue))
            $ \rawKvs ->
                let kvs =
                        nubBy
                            (\(k1, _) (k2, _) -> toHexKey k1 == toHexKey k2)
                            rawKvs
                in  length kvs >= 2 ==>
                        let kvHashed =
                                [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                            (half1, half2) = splitAt (length kvHashed `div` 2) kvHashed
                            -- All in one run
                            (rootCombined, _) = runMPFPure' $ do
                                mapM_ (uncurry insertMPFM) kvHashed
                                getRootHashM
                            -- Split across two runs
                            (_, db1) = runMPFPure' $ do
                                mapM_ (uncurry insertMPFM) half1
                            (rootSplit, _) = runMPFPure db1 $ do
                                mapM_ (uncurry insertMPFM) half2
                                getRootHashM
                        in  fmap renderMPFHash rootCombined
                                === fmap renderMPFHash rootSplit

        it "insert-query roundtrip in transactions"
            $ forAll ((,) <$> genKeyBytes <*> genValue)
            $ \(kb, vb) ->
                let key = toHexKey kb
                    val = mkMPFHash vb
                    (verified, _) = runMPFPure' $ do
                        insertMPFM key val
                        verifyMPFM key val
                in  verified === True
