{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MPF.DeletionSpec
-- Description : Tests for MPF deletion operations
--
-- Unit tests for key deletion from the MPF trie:
--
-- * Deleting a key removes it from the trie
-- * Deleting from empty trie is a no-op
-- * Deleting a non-existent key is a no-op
-- * Root hash changes after deletion
-- * Root hash returns to null after deleting all keys
-- * Deletion and re-insertion restores the original root hash
module MPF.DeletionSpec (spec) where

import Control.Monad (forM_)
import MPF.Backend.Pure (emptyMPFInMemoryDB)
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( deleteMPF
    , deleteMPFM
    , fruitsTestData
    , getRootHashM
    , insertByteStringM
    , insertMPFM
    , runMPFPure'
    , verifyMPFM
    )
import Test.Hspec

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
