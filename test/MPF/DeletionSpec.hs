{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MPF.DeletionSpec
-- Description : Tests for MPF deletion operations
--
-- Unit tests and property tests for key deletion from the MPF trie.
module MPF.DeletionSpec (spec) where

import Control.Monad (forM_)
import Data.List (nubBy)
import MPF.Backend.Pure (emptyMPFInMemoryDB)
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( deleteMPF
    , deleteMPFM
    , fruitsTestData
    , genKeyBytes
    , genValue
    , getRootHashM
    , insertByteStringM
    , insertMPFM
    , runMPFPure'
    , toHexKey
    , verifyMPFM
    )
import Test.Hspec
import Test.QuickCheck
    ( forAll
    , property
    , vectorOf
    , (===)
    , (==>)
    )

spec :: Spec
spec = describe "MPF.Deletion" $ do
    describe "single deletion" $ do
        it "removes inserted key from verification" $ do
            let key = byteStringToHexKey "hello"
                value = mkMPFHash "world"
                (verified, _) = runMPFPure' $ do
                    insertMPFM key value
                    deleteMPFM key
                    verifyMPFM key value
            verified `shouldBe` False

        it "returns empty trie after deleting only key" $ do
            let key = byteStringToHexKey "only"
                value = mkMPFHash "value"
                (mRoot, _) = runMPFPure' $ do
                    insertMPFM key value
                    deleteMPFM key
                    getRootHashM
            mRoot `shouldBe` Nothing

    describe "deletion from empty trie" $ do
        it "is a no-op on empty database" $ do
            let key = byteStringToHexKey "ghost"
                db = deleteMPF emptyMPFInMemoryDB key
            db `shouldBe` emptyMPFInMemoryDB

    describe "deletion of non-existent key" $ do
        it "preserves existing entries" $ do
            let k1 = byteStringToHexKey "exists"
                v1 = mkMPFHash "value"
                k2 = byteStringToHexKey "ghost"
                (verified, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    deleteMPFM k2
                    verifyMPFM k1 v1
            verified `shouldBe` True

        it "preserves root hash" $ do
            let k1 = byteStringToHexKey "exists"
                v1 = mkMPFHash "value"
                k2 = byteStringToHexKey "ghost"
                (rootBefore, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    getRootHashM
                (rootAfter, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    deleteMPFM k2
                    getRootHashM
            fmap renderMPFHash rootAfter
                `shouldBe` fmap renderMPFHash rootBefore

    describe "root hash after deletion" $ do
        it "changes root hash when a key is deleted" $ do
            let k1 = byteStringToHexKey "key1"
                v1 = mkMPFHash "value1"
                k2 = byteStringToHexKey "key2"
                v2 = mkMPFHash "value2"
                (rootBefore, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    insertMPFM k2 v2
                    getRootHashM
                (rootAfter, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    insertMPFM k2 v2
                    deleteMPFM k2
                    getRootHashM
            fmap renderMPFHash rootAfter
                `shouldNotBe` fmap renderMPFHash rootBefore

        it
            "root hash equals single-insert hash after deleting second key"
            $ do
                let k1 = byteStringToHexKey "key1"
                    v1 = mkMPFHash "value1"
                    k2 = byteStringToHexKey "key2"
                    v2 = mkMPFHash "value2"
                    (rootSingle, _) = runMPFPure' $ do
                        insertMPFM k1 v1
                        getRootHashM
                    (rootDeleted, _) = runMPFPure' $ do
                        insertMPFM k1 v1
                        insertMPFM k2 v2
                        deleteMPFM k2
                        getRootHashM
                fmap renderMPFHash rootDeleted
                    `shouldBe` fmap renderMPFHash rootSingle

    describe "delete and re-insert" $ do
        it "restores the original root hash" $ do
            let k1 = byteStringToHexKey "key1"
                v1 = mkMPFHash "value1"
                (rootOriginal, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    getRootHashM
                (rootRestored, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    deleteMPFM k1
                    insertMPFM k1 v1
                    getRootHashM
            fmap renderMPFHash rootRestored
                `shouldBe` fmap renderMPFHash rootOriginal

    describe "sibling preservation" $ do
        it "preserves all remaining siblings" $ do
            let k1 = byteStringToHexKey "alpha"
                v1 = mkMPFHash "a"
                k2 = byteStringToHexKey "bravo"
                v2 = mkMPFHash "b"
                k3 = byteStringToHexKey "charlie"
                v3 = mkMPFHash "c"
                ((r1, r3), _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    insertMPFM k2 v2
                    insertMPFM k3 v3
                    deleteMPFM k2
                    (,)
                        <$> verifyMPFM k1 v1
                        <*> verifyMPFM k3 v3
            r1 `shouldBe` True
            r3 `shouldBe` True

    describe "multiple deletions" $ do
        it "can delete all keys one by one" $ do
            let k1 = byteStringToHexKey "x"
                v1 = mkMPFHash "1"
                k2 = byteStringToHexKey "y"
                v2 = mkMPFHash "2"
                k3 = byteStringToHexKey "z"
                v3 = mkMPFHash "3"
                (mRoot, _) = runMPFPure' $ do
                    insertMPFM k1 v1
                    insertMPFM k2 v2
                    insertMPFM k3 v3
                    deleteMPFM k1
                    deleteMPFM k2
                    deleteMPFM k3
                    getRootHashM
            mRoot `shouldBe` Nothing

    describe "fruits dataset deletion" $ do
        it "can delete first fruit and verify second" $ do
            case fruitsTestData of
                ((k1, _) : (k2, v2) : _) -> do
                    let (verified, _) = runMPFPure' $ do
                            forM_ fruitsTestData
                                $ uncurry insertByteStringM
                            let secondKey =
                                    byteStringToHexKey
                                        $ renderMPFHash
                                        $ mkMPFHash k2
                                secondVal = mkMPFHash v2
                            deleteMPFM
                                ( byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash k1
                                )
                            verifyMPFM secondKey secondVal
                    verified `shouldBe` True
                _ -> expectationFailure "need >= 2 fruits"

        it "changes root hash after deleting a fruit" $ do
            case fruitsTestData of
                ((k, _) : _) -> do
                    let (rootBefore, _) = runMPFPure' $ do
                            forM_ fruitsTestData
                                $ uncurry insertByteStringM
                            getRootHashM
                        (rootAfter, _) = runMPFPure' $ do
                            forM_ fruitsTestData
                                $ uncurry insertByteStringM
                            deleteMPFM
                                ( byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash k
                                )
                            getRootHashM
                    fmap renderMPFHash rootAfter
                        `shouldNotBe` fmap renderMPFHash rootBefore
                _ -> expectationFailure "need >= 1 fruit"

        it "matches inserting without deleted fruit" $ do
            case fruitsTestData of
                ((k, _) : rest) -> do
                    let (rootWithout, _) = runMPFPure' $ do
                            forM_ rest
                                $ uncurry insertByteStringM
                            getRootHashM
                        (rootDeleted, _) = runMPFPure' $ do
                            forM_ fruitsTestData
                                $ uncurry insertByteStringM
                            deleteMPFM
                                ( byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash k
                                )
                            getRootHashM
                    fmap renderMPFHash rootDeleted
                        `shouldBe` fmap renderMPFHash rootWithout
                _ -> expectationFailure "need >= 1 fruit"

    describe "properties" $ do
        it "insert-delete-reinsert roundtrip"
            $ forAll ((,) <$> genKeyBytes <*> genValue)
            $ \(kb, vb) ->
                let key = toHexKey kb
                    val = mkMPFHash vb
                    (rootSingle, _) = runMPFPure' $ do
                        insertMPFM key val
                        getRootHashM
                    (rootReinsert, _) = runMPFPure' $ do
                        insertMPFM key val
                        deleteMPFM key
                        insertMPFM key val
                        getRootHashM
                in  fmap renderMPFHash rootReinsert === fmap renderMPFHash rootSingle

        it "deletion is idempotent"
            $ forAll ((,) <$> genKeyBytes <*> genValue)
            $ \(kb, vb) ->
                let key = toHexKey kb
                    val = mkMPFHash vb
                    (root1, _) = runMPFPure' $ do
                        insertMPFM key val
                        deleteMPFM key
                        getRootHashM
                    (root2, _) = runMPFPure' $ do
                        insertMPFM key val
                        deleteMPFM key
                        deleteMPFM key
                        getRootHashM
                in  fmap renderMPFHash root1 === fmap renderMPFHash root2

        it "delete non-existent is no-op"
            $ forAll
                ((,,) <$> genKeyBytes <*> genKeyBytes <*> genValue)
            $ \(kb1, kb2, vb) ->
                toHexKey kb1 /= toHexKey kb2 ==>
                    let key = toHexKey kb1
                        ghost = toHexKey kb2
                        val = mkMPFHash vb
                        (rootBefore, _) = runMPFPure' $ do
                            insertMPFM key val
                            getRootHashM
                        (rootAfter, _) = runMPFPure' $ do
                            insertMPFM key val
                            deleteMPFM ghost
                            getRootHashM
                    in  fmap renderMPFHash rootAfter === fmap renderMPFHash rootBefore

        it "mass deletion empties trie"
            $ forAll (vectorOf 5 ((,) <$> genKeyBytes <*> genValue))
            $ \rawKvs ->
                let kvs =
                        nubBy
                            (\(k1, _) (k2, _) -> toHexKey k1 == toHexKey k2)
                            rawKvs
                in  length kvs >= 2 ==>
                        let kvHashed =
                                [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                            (mRoot, _) = runMPFPure' $ do
                                forM_ kvHashed $ uncurry insertMPFM
                                forM_ kvHashed $ deleteMPFM . fst
                                getRootHashM
                        in  mRoot === Nothing

        it "deletion preserves unrelated keys"
            $ forAll (vectorOf 3 ((,) <$> genKeyBytes <*> genValue))
            $ \rawKvs ->
                let kvs =
                        nubBy
                            (\(k1, _) (k2, _) -> toHexKey k1 == toHexKey k2)
                            rawKvs
                in  length kvs == 3 ==>
                        let kvHashed =
                                [(toHexKey k, mkMPFHash v) | (k, v) <- kvs]
                            (keepKey, keepVal) = head kvHashed
                            deleteKey = fst (kvHashed !! 1)
                            (verified, _) = runMPFPure' $ do
                                forM_ kvHashed $ uncurry insertMPFM
                                deleteMPFM deleteKey
                                verifyMPFM keepKey keepVal
                        in  verified === True
