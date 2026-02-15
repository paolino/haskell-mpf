-- |
-- Module      : Cardano.MPFS.StateSpec
-- Description : Property tests for State interfaces
-- License     : Apache-2.0
module Cardano.MPFS.StateSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.TxIn (TxIn)

import Cardano.MPFS.Generators
    ( genBlockId
    , genRequest
    , genSlotNo
    , genTokenId
    , genTokenState
    , genTxIn
    )
import Cardano.MPFS.State
    ( Checkpoints (..)
    , Requests (..)
    , Tokens (..)
    )
import Cardano.MPFS.Types
    ( BlockId
    , Request (..)
    , TokenId
    , TokenState
    )

-- -----------------------------------------------------------------
-- Mock implementations
-- -----------------------------------------------------------------

-- | Create mock 'Tokens' backed by an 'IORef'.
mkMockTokens :: IO (Tokens IO)
mkMockTokens = do
    ref <- newIORef (Map.empty :: Map TokenId TokenState)
    pure
        Tokens
            { getToken = \tid ->
                Map.lookup tid <$> readIORef ref
            , putToken = \tid ts ->
                modifyIORef' ref (Map.insert tid ts)
            , removeToken = \tid ->
                modifyIORef' ref (Map.delete tid)
            , listTokens =
                Map.keys <$> readIORef ref
            }

-- | Create mock 'Requests' backed by an 'IORef'.
mkMockRequests :: IO (Requests IO)
mkMockRequests = do
    ref <- newIORef (Map.empty :: Map TxIn Request)
    pure
        Requests
            { getRequest = \txin ->
                Map.lookup txin <$> readIORef ref
            , putRequest = \txin req ->
                modifyIORef' ref (Map.insert txin req)
            , removeRequest = \txin ->
                modifyIORef' ref (Map.delete txin)
            , requestsByToken = \tid ->
                filter
                    (\r -> requestToken r == tid)
                    . Map.elems
                    <$> readIORef ref
            }

-- | Create mock 'Checkpoints' backed by an 'IORef'.
mkMockCheckpoints :: IO (Checkpoints IO)
mkMockCheckpoints = do
    ref <-
        newIORef
            (Nothing :: Maybe (SlotNo, BlockId))
    pure
        Checkpoints
            { getCheckpoint = readIORef ref
            , putCheckpoint = \s b ->
                modifyIORef' ref (const (Just (s, b)))
            }

-- -----------------------------------------------------------------
-- Specs
-- -----------------------------------------------------------------

spec :: Spec
spec = do
    describe "Tokens" tokensSpec
    describe "Requests" requestsSpec
    describe "Checkpoints" checkpointsSpec

tokensSpec :: Spec
tokensSpec = do
    prop "get on empty returns Nothing"
        $ forAll genTokenId
        $ \tid ->
            monadicIO $ do
                tok <- run mkMockTokens
                r <- run $ getToken tok tid
                assert (r == Nothing)

    prop "put/get round-trip"
        $ forAll genTokenId
        $ \tid ->
            forAll genTokenState $ \ts ->
                monadicIO $ do
                    tok <- run mkMockTokens
                    run $ putToken tok tid ts
                    r <- run $ getToken tok tid
                    assert (r == Just ts)

    prop "put/remove/get returns Nothing"
        $ forAll genTokenId
        $ \tid ->
            forAll genTokenState $ \ts ->
                monadicIO $ do
                    tok <- run mkMockTokens
                    run $ putToken tok tid ts
                    run $ removeToken tok tid
                    r <- run $ getToken tok tid
                    assert (r == Nothing)

    prop "put appears in listTokens"
        $ forAll genTokenId
        $ \tid ->
            forAll genTokenState $ \ts ->
                monadicIO $ do
                    tok <- run mkMockTokens
                    run $ putToken tok tid ts
                    ids <- run $ listTokens tok
                    assert (tid `elem` ids)

    prop "put overwrites previous"
        $ forAll genTokenId
        $ \tid ->
            forAll genTokenState $ \ts1 ->
                forAll genTokenState $ \ts2 ->
                    monadicIO $ do
                        tok <- run mkMockTokens
                        run $ putToken tok tid ts1
                        run $ putToken tok tid ts2
                        r <- run $ getToken tok tid
                        assert (r == Just ts2)

    prop "remove on empty doesn't crash"
        $ forAll genTokenId
        $ \tid ->
            monadicIO $ do
                tok <- run mkMockTokens
                run $ removeToken tok tid
                assert True

requestsSpec :: Spec
requestsSpec = do
    prop "put/get round-trip"
        $ forAll genTxIn
        $ \txin ->
            forAll genTokenId $ \tid ->
                forAll (genRequest tid) $ \req ->
                    monadicIO $ do
                        rs <- run mkMockRequests
                        run $ putRequest rs txin req
                        r <- run $ getRequest rs txin
                        assert (r == Just req)

    prop "put/remove/get returns Nothing"
        $ forAll genTxIn
        $ \txin ->
            forAll genTokenId $ \tid ->
                forAll (genRequest tid) $ \req ->
                    monadicIO $ do
                        rs <- run mkMockRequests
                        run $ putRequest rs txin req
                        run $ removeRequest rs txin
                        r <- run $ getRequest rs txin
                        assert (r == Nothing)

    prop "requestsByToken filters correctly"
        $ forAll genTokenId
        $ \tid1 ->
            forAll genTokenId $ \tid2 ->
                tid1 /= tid2 ==>
                    forAll genTxIn $ \txin1 ->
                        forAll genTxIn $ \txin2 ->
                            txin1 /= txin2 ==>
                                forAll (genRequest tid1) $ \req1 ->
                                    forAll (genRequest tid2) $ \req2 ->
                                        monadicIO $ do
                                            rs <- run mkMockRequests
                                            run $ putRequest rs txin1 req1
                                            run $ putRequest rs txin2 req2
                                            r <- run $ requestsByToken rs tid1
                                            assert (r == [req1])

    prop "requestsByToken on empty returns []"
        $ forAll genTokenId
        $ \tid ->
            monadicIO $ do
                rs <- run mkMockRequests
                r <- run $ requestsByToken rs tid
                assert (null r)

checkpointsSpec :: Spec
checkpointsSpec = do
    it "get on empty returns Nothing" $ do
        cp <- mkMockCheckpoints
        getCheckpoint cp `shouldReturn` Nothing

    prop "put/get round-trip"
        $ forAll genSlotNo
        $ \s ->
            forAll genBlockId $ \b ->
                monadicIO $ do
                    cp <- run mkMockCheckpoints
                    run $ putCheckpoint cp s b
                    r <- run $ getCheckpoint cp
                    assert (r == Just (s, b))

    prop "put overwrites previous"
        $ forAll genSlotNo
        $ \s1 ->
            forAll genBlockId $ \b1 ->
                forAll genSlotNo $ \s2 ->
                    forAll genBlockId $ \b2 ->
                        monadicIO $ do
                            cp <- run mkMockCheckpoints
                            run $ putCheckpoint cp s1 b1
                            run $ putCheckpoint cp s2 b2
                            r <- run $ getCheckpoint cp
                            assert (r == Just (s2, b2))
