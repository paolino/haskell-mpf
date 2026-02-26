-- |
-- Module      : Cardano.MPFS.Indexer.EventSpec
-- Description : Tests for CageEvent inverse operations
-- License     : Apache-2.0
module Cardano.MPFS.Indexer.EventSpec
    ( spec
    ) where

import Data.Map.Strict qualified as Map
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Gen
    , forAll
    , listOf1
    )

import Cardano.MPFS.Core.Types
    ( Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    , TxIn
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
    , inversesOf
    )

spec :: Spec
spec = describe "CageEvent" $ do
    describe "inversesOf" $ do
        it "boot produces InvRemoveToken"
            $ forAll genBoot
            $ \(tid, ts) ->
                inversesOf noTokens noReqs (CageBoot tid ts)
                    `shouldBe` [InvRemoveToken tid]

        it "request produces InvRemoveRequest"
            $ forAll genReqEvent
            $ \(txIn, req) ->
                inversesOf noTokens noReqs (CageRequest txIn req)
                    `shouldBe` [InvRemoveRequest txIn]

        it "retract with known request produces InvRestoreRequest"
            $ forAll genRetractKnown
            $ \(txIn, req) ->
                let reqs = Map.singleton txIn req
                in  inversesOf
                        noTokens
                        (`Map.lookup` reqs)
                        (CageRetract txIn)
                        `shouldBe` [InvRestoreRequest txIn req]

        it "retract with unknown request produces empty"
            $ forAll genTxIn
            $ \txIn ->
                inversesOf noTokens noReqs (CageRetract txIn)
                    `shouldBe` []

        it "burn with known token produces InvRestoreToken"
            $ forAll genBurnKnown
            $ \(tid, ts) ->
                let tokens = Map.singleton tid ts
                in  inversesOf
                        (`Map.lookup` tokens)
                        noReqs
                        (CageBurn tid)
                        `shouldBe` [InvRestoreToken tid ts]

        it "burn with unknown token produces empty"
            $ forAll genTokenId
            $ \tid ->
                inversesOf noTokens noReqs (CageBurn tid)
                    `shouldBe` []

        it
            "update with known token+requests produces InvRestoreRoot + InvRestoreRequest"
            $ forAll genUpdateKnownFull
            $ \(tid, ts, newRoot, txIn, req) ->
                let tokMap = Map.singleton tid ts
                    reqMap = Map.singleton txIn req
                    result =
                        inversesOf
                            (`Map.lookup` tokMap)
                            (`Map.lookup` reqMap)
                            ( CageUpdate
                                tid
                                newRoot
                                [txIn]
                            )
                in  result
                        `shouldBe` [ InvRestoreRoot
                                        tid
                                        (root ts)
                                   , InvRestoreRequest
                                        txIn
                                        req
                                   ]

        it
            "update with unknown token and no requests produces empty"
            $ forAll genUpdateUnknown
            $ \(tid, newRoot, consumed) ->
                inversesOf
                    noTokens
                    noReqs
                    (CageUpdate tid newRoot consumed)
                    `shouldBe` []

-- Helpers

noTokens :: TokenId -> Maybe TokenState
noTokens = const Nothing

noReqs :: TxIn -> Maybe Request
noReqs = const Nothing

-- Generators

genBoot :: Gen (TokenId, TokenState)
genBoot = (,) <$> genTokenId <*> genTokenState

genReqEvent :: Gen (TxIn, Request)
genReqEvent = do
    tid <- genTokenId
    txIn <- genTxIn
    req <- genRequest tid
    pure (txIn, req)

genBurnKnown :: Gen (TokenId, TokenState)
genBurnKnown = (,) <$> genTokenId <*> genTokenState

genRetractKnown :: Gen (TxIn, Request)
genRetractKnown = do
    tid <- genTokenId
    txIn <- genTxIn
    req <- genRequest tid
    pure (txIn, req)

genUpdateKnownFull
    :: Gen (TokenId, TokenState, Root, TxIn, Request)
genUpdateKnownFull = do
    tid <- genTokenId
    ts <- genTokenState
    newRoot <- genRoot
    txIn <- genTxIn
    req <- genRequest tid
    pure (tid, ts, newRoot, txIn, req)

genUpdateUnknown :: Gen (TokenId, Root, [TxIn])
genUpdateUnknown =
    (,,)
        <$> genTokenId
        <*> genRoot
        <*> listOf1 genTxIn
