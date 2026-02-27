{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MPF.PropertySpec
-- Description : Property-based tests for MPF (Merkle Patricia Forest)
--
-- QuickCheck properties testing core MPF invariants:
--
-- * Single key insert-verify roundtrip
-- * Multiple key insert-verify roundtrip
-- * Root hash independence from insertion order
-- * Deletion removes key from verification
-- * Deletion preserves sibling proofs
-- * Root hash existence after insertions
module MPF.PropertySpec (spec) where

import Control.Monad (forM, forM_)
import Data.List (nubBy)
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Test.Lib
    ( deleteMPFM
    , genKeyBytes
    , genUniqueKVs
    , genValue
    , getRootHashM
    , insertBatchMPFM
    , insertChunkedMPFM
    , insertMPFM
    , runMPFPure'
    , toHexKey
    , verifyMPFM
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , forAll
    , property
    , shuffle
    , vectorOf
    , (===)
    , (==>)
    )

-- | A key-value pair for testing
data TestKV = TestKV
    { testKey :: !String
    , testVal :: !String
    }
    deriving (Show, Eq)

instance Arbitrary TestKV where
    arbitrary = TestKV . show <$> genKeyBytes <*> (show <$> genValue)

spec :: Spec
spec = do
    describe "MPF Properties" $ do
        describe "insert-verify roundtrip" $ do
            it "inserted key-value can be verified" $ property propInsertVerify

            it "multiple inserts all verifiable"
                $ property propMultipleInsertVerify

        describe "batch insert" $ do
            it "batch insert equals sequential inserts"
                $ property propBatchEqualsSequential

            it "chunked insert equals sequential inserts"
                $ property propChunkedEqualsSequential

        describe "insertion order independence" $ do
            it "same keys in any order produce same root hash"
                $ property propInsertionOrderIndependence

        describe "deletion properties" $ do
            it "deleted key cannot be verified" $ property propDeleteRemovesKey

            it "deletion preserves sibling proofs"
                $ property propDeletePreservesSiblings

        describe "root hash properties" $ do
            it "empty tree has no root hash" $ do
                let (mRoot, _) = runMPFPure' getRootHashM
                mRoot `shouldBe` Nothing

            it "single insert produces root hash"
                $ property propSingleInsertHasRoot

-- | Property: inserted key-value can be verified
propInsertVerify :: Property
propInsertVerify = forAll ((,) <$> genKeyBytes <*> genValue) $ \(keyBs, valBs) ->
    let key = toHexKey keyBs
        value = mkMPFHash valBs
        (verified, _) = runMPFPure' $ do
            insertMPFM key value
            verifyMPFM key value
    in  verified

-- | Property: multiple inserts all verifiable
propMultipleInsertVerify :: Property
propMultipleInsertVerify =
    forAll (vectorOf 3 ((,) <$> genKeyBytes <*> genValue)) $ \rawKvs ->
        let kvs = nubBy (\(k1, _) (k2, _) -> toHexKey k1 == toHexKey k2) rawKvs
        in  length kvs == 3 ==>
                let kvHashed = [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                    (verifyResults, _) = runMPFPure' $ do
                        forM_ kvHashed $ uncurry insertMPFM
                        forM kvHashed $ \(k, v) -> do
                            r <- verifyMPFM k v
                            pure (r, k)
                    allPassed = all fst verifyResults
                in  allPassed

-- | Property: same keys in any order produce same root hash
propInsertionOrderIndependence :: Property
propInsertionOrderIndependence = forAll genUniqueKVs $ \kvs ->
    length kvs >= 2 ==>
        forAll (shuffle kvs) $ \shuffled ->
            let kvHashed = [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                shuffledHashed = [(toHexKey k, mkMPFHash v) | (k, v) <- shuffled]
                (root1, _) = runMPFPure' $ do
                    forM_ kvHashed $ uncurry insertMPFM
                    getRootHashM
                (root2, _) = runMPFPure' $ do
                    forM_ shuffledHashed $ uncurry insertMPFM
                    getRootHashM
            in  fmap renderMPFHash root1 === fmap renderMPFHash root2

-- | Property: deleted key cannot be verified
propDeleteRemovesKey :: Property
propDeleteRemovesKey = forAll ((,) <$> genKeyBytes <*> genValue) $ \(keyBs, valBs) ->
    let key = toHexKey keyBs
        value = mkMPFHash valBs
        (verified, _) = runMPFPure' $ do
            insertMPFM key value
            deleteMPFM key
            verifyMPFM key value
    in  not verified

-- | Property: deletion preserves sibling proofs
propDeletePreservesSiblings :: Property
propDeletePreservesSiblings =
    forAll (vectorOf 3 ((,) <$> genKeyBytes <*> genValue)) $ \rawKvs ->
        let kvs = nubBy (\(k1, _) (k2, _) -> toHexKey k1 == toHexKey k2) rawKvs
        in  length kvs == 3 ==>
                let kvHashed = [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                    (keepKey, keepVal) = head kvHashed
                    deleteKey = fst (kvHashed !! 1)
                    (verified, _) = runMPFPure' $ do
                        forM_ kvHashed $ uncurry insertMPFM
                        deleteMPFM deleteKey
                        verifyMPFM keepKey keepVal
                in  verified

-- | Property: single insert produces a root hash
propSingleInsertHasRoot :: Property
propSingleInsertHasRoot = forAll ((,) <$> genKeyBytes <*> genValue) $ \(keyBs, valBs) ->
    let key = toHexKey keyBs
        value = mkMPFHash valBs
        (mRoot, _) = runMPFPure' $ do
            insertMPFM key value
            getRootHashM
    in  case mRoot of
            Just _ -> True
            Nothing -> False

-- | Property: batch insert produces same root hash as sequential inserts
propBatchEqualsSequential :: Property
propBatchEqualsSequential =
    forAll (vectorOf 5 ((,) <$> genKeyBytes <*> genValue)) $ \rawKvs ->
        let kvs = nubBy (\(k1, _) (k2, _) -> toHexKey k1 == toHexKey k2) rawKvs
        in  length kvs >= 2 ==>
                let kvHashed = [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                    (rootSeq, _) = runMPFPure' $ do
                        forM_ kvHashed $ uncurry insertMPFM
                        getRootHashM
                    (rootBatch, _) = runMPFPure' $ do
                        insertBatchMPFM kvHashed
                        getRootHashM
                in  fmap renderMPFHash rootSeq === fmap renderMPFHash rootBatch

-- | Property: chunked insert produces same root hash as sequential inserts
propChunkedEqualsSequential :: Property
propChunkedEqualsSequential =
    forAll (vectorOf 5 ((,) <$> genKeyBytes <*> genValue)) $ \rawKvs ->
        let kvs = nubBy (\(k1, _) (k2, _) -> toHexKey k1 == toHexKey k2) rawKvs
        in  length kvs >= 2 ==>
                let kvHashed = [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                    (rootSeq, _) = runMPFPure' $ do
                        forM_ kvHashed $ uncurry insertMPFM
                        getRootHashM
                    (rootChunked, _) = runMPFPure' $ do
                        _ <- insertChunkedMPFM 2 kvHashed
                        getRootHashM
                in  fmap renderMPFHash rootSeq === fmap renderMPFHash rootChunked
