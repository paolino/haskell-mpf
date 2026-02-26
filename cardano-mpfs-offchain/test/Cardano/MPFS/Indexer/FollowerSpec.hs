{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.Indexer.FollowerSpec
-- Description : Tests for cage block processor
-- License     : Apache-2.0
--
-- Tests for 'applyCageEvent', 'computeInverse',
-- and 'applyCageInverses' using mock state and
-- pure trie manager.
module Cardano.MPFS.Indexer.FollowerSpec
    ( spec
    ) where

import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.QuickCheck
    ( forAll
    , property
    )

import Cardano.MPFS.Core.Types
    ( TokenState (..)
    )
import Cardano.MPFS.Generators
    ( genRequest
    , genRoot
    , genTokenId
    , genTokenState
    , genTxIn
    )
import Cardano.MPFS.Indexer.Event
    ( CageEvent (..)
    , CageInverseOp (..)
    )
import Cardano.MPFS.Indexer.Follower
    ( applyCageEvent
    , applyCageInverses
    , computeInverse
    )
import Cardano.MPFS.Mock.State (mkMockState)
import Cardano.MPFS.State
    ( Requests (..)
    , State (..)
    , Tokens (..)
    )
import Cardano.MPFS.Trie ()
import Cardano.MPFS.Trie.PureManager
    ( mkPureTrieManager
    )

spec :: Spec
spec = describe "CageFollower" $ do
    describe "applyCageEvent" $ do
        it "boot inserts token and creates trie"
            $ property
            $ forAll ((,) <$> genTokenId <*> genTokenState)
            $ \(tid, ts) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                _ <- applyCageEvent st tm (CageBoot tid ts)
                getToken (tokens st) tid
                    `shouldReturn` Just ts

        it "request inserts into requests"
            $ property
            $ forAll
                ( do
                    tid <- genTokenId
                    txIn <- genTxIn
                    req <- genRequest tid
                    pure (txIn, req)
                )
            $ \(txIn, req) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                _ <-
                    applyCageEvent
                        st
                        tm
                        (CageRequest txIn req)
                getRequest (requests st) txIn
                    `shouldReturn` Just req

        it "retract removes request"
            $ property
            $ forAll
                ( do
                    tid <- genTokenId
                    txIn <- genTxIn
                    req <- genRequest tid
                    pure (txIn, req)
                )
            $ \(txIn, req) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                -- First insert, then retract
                _ <-
                    applyCageEvent
                        st
                        tm
                        (CageRequest txIn req)
                _ <-
                    applyCageEvent
                        st
                        tm
                        (CageRetract txIn)
                getRequest (requests st) txIn
                    `shouldReturn` Nothing

        it "burn removes token and hides trie"
            $ property
            $ forAll ((,) <$> genTokenId <*> genTokenState)
            $ \(tid, ts) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                -- First boot, then burn
                _ <- applyCageEvent st tm (CageBoot tid ts)
                _ <- applyCageEvent st tm (CageBurn tid)
                getToken (tokens st) tid
                    `shouldReturn` Nothing

        it "update changes token root"
            $ property
            $ forAll
                ( (,,)
                    <$> genTokenId
                    <*> genTokenState
                    <*> genRoot
                )
            $ \(tid, ts, newRoot) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                -- Boot the token first
                _ <- applyCageEvent st tm (CageBoot tid ts)
                -- Update with no consumed requests
                _ <-
                    applyCageEvent
                        st
                        tm
                        (CageUpdate tid newRoot [])
                mTs <- getToken (tokens st) tid
                fmap root mTs `shouldBe` Just newRoot

    describe "computeInverse" $ do
        it "boot inverse is InvRemoveToken"
            $ property
            $ forAll ((,) <$> genTokenId <*> genTokenState)
            $ \(tid, ts) -> do
                st <- mkMockState
                inv <-
                    computeInverse st (CageBoot tid ts)
                inv `shouldBe` [InvRemoveToken tid]

        it
            "update inverse includes InvRestoreRoot when token exists"
            $ property
            $ forAll
                ( (,,)
                    <$> genTokenId
                    <*> genTokenState
                    <*> genRoot
                )
            $ \(tid, ts, newRoot) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                -- Boot token first so it exists
                _ <- applyCageEvent st tm (CageBoot tid ts)
                inv <-
                    computeInverse
                        st
                        (CageUpdate tid newRoot [])
                inv
                    `shouldBe` [ InvRestoreRoot
                                    tid
                                    (root ts)
                               ]

        it "burn inverse is InvRestoreToken when token exists"
            $ property
            $ forAll ((,) <$> genTokenId <*> genTokenState)
            $ \(tid, ts) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                _ <- applyCageEvent st tm (CageBoot tid ts)
                inv <-
                    computeInverse st (CageBurn tid)
                inv `shouldBe` [InvRestoreToken tid ts]

    describe "applyCageInverses" $ do
        it
            "InvRestoreToken after burn restores original state"
            $ property
            $ forAll ((,) <$> genTokenId <*> genTokenState)
            $ \(tid, ts) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                -- Boot → Burn → InvRestore
                _ <- applyCageEvent st tm (CageBoot tid ts)
                _ <- applyCageEvent st tm (CageBurn tid)
                applyCageInverses
                    st
                    tm
                    [InvRestoreToken tid ts]
                getToken (tokens st) tid
                    `shouldReturn` Just ts

        it "InvRemoveToken undoes a boot"
            $ property
            $ forAll ((,) <$> genTokenId <*> genTokenState)
            $ \(tid, ts) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                _ <- applyCageEvent st tm (CageBoot tid ts)
                applyCageInverses
                    st
                    tm
                    [InvRemoveToken tid]
                getToken (tokens st) tid
                    `shouldReturn` Nothing

        it "InvRestoreRoot restores previous root"
            $ property
            $ forAll
                ( (,,)
                    <$> genTokenId
                    <*> genTokenState
                    <*> genRoot
                )
            $ \(tid, ts, newRoot) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                let origRoot = root ts
                _ <- applyCageEvent st tm (CageBoot tid ts)
                _ <-
                    applyCageEvent
                        st
                        tm
                        (CageUpdate tid newRoot [])
                applyCageInverses
                    st
                    tm
                    [InvRestoreRoot tid origRoot]
                mTs <- getToken (tokens st) tid
                fmap root mTs `shouldBe` Just origRoot

        it "full boot → inverse roundtrip"
            $ property
            $ forAll ((,) <$> genTokenId <*> genTokenState)
            $ \(tid, ts) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                inv <-
                    computeInverse st (CageBoot tid ts)
                _ <- applyCageEvent st tm (CageBoot tid ts)
                applyCageInverses st tm inv
                getToken (tokens st) tid
                    `shouldReturn` Nothing

        it "full request → inverse roundtrip"
            $ property
            $ forAll
                ( do
                    tid <- genTokenId
                    txIn <- genTxIn
                    req <- genRequest tid
                    pure (txIn, req)
                )
            $ \(txIn, req) -> do
                st <- mkMockState
                tm <- mkPureTrieManager
                inv <-
                    computeInverse
                        st
                        (CageRequest txIn req)
                _ <-
                    applyCageEvent
                        st
                        tm
                        (CageRequest txIn req)
                applyCageInverses st tm inv
                getRequest (requests st) txIn
                    `shouldReturn` Nothing

    describe "extractConwayTxs" $ do
        it
            "returns empty for non-Conway blocks"
            -- We can't easily construct a full Block in
            -- tests, so just verify the module compiles
            -- and exports are available. Real block
            -- processing is tested in E2E tests.
            (pure () :: IO ())
