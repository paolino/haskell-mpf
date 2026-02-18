{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.MPFS.TxBuilderSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set
import Lens.Micro ((^.))
import System.Environment (lookupEnv)

import Test.Hspec
    ( Spec
    , describe
    , it
    , runIO
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck (generate)

import Cardano.Crypto.Hash
    ( Blake2b_224
    , hashFromStringAsHex
    , hashToBytes
    )
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api.PParams (emptyPParams)
import Cardano.Ledger.Api.Tx
    ( Tx
    , bodyTxL
    , witsTxL
    )
import Cardano.Ledger.Api.Tx.Body
    ( inputsTxBodyL
    , mintTxBodyL
    , outputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Out
    ( TxOut
    , coinTxOutL
    , mkBasicTxOut
    )
import Cardano.Ledger.Api.Tx.Wits
    ( Redeemers (..)
    , rdmrsTxWitsL
    , scriptTxWitsL
    )
import Cardano.Ledger.BaseTypes
    ( Inject (..)
    , Network (..)
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    , StakeReference (..)
    )
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys
    ( KeyHash (..)
    , KeyRole (..)
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset (..)
    , PolicyID (..)
    )
import Cardano.Ledger.Plutus.ExUnits
    ( ExUnits (..)
    )
import Cardano.Ledger.TxIn (TxIn)

import Cardano.MPFS.Blueprint
    ( extractCompiledCode
    , loadBlueprint
    )
import Cardano.MPFS.Generators (genTxIn)
import Cardano.MPFS.Mock.State (mkMockState)
import Cardano.MPFS.OnChain
    ( cageAddr
    , cagePolicyId
    , cageScriptHash
    )
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State (State (..), Tokens (..))
import Cardano.MPFS.Trie (TrieManager (..))
import Cardano.MPFS.TxBuilder (TxBuilder (..))
import Cardano.MPFS.TxBuilder.Config (CageConfig (..))
import Cardano.MPFS.TxBuilder.Real (mkRealTxBuilder)
import Cardano.MPFS.Types
    ( AssetName (..)
    , Coin (..)
    , ConwayEra
    , PParams
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    )

-- ---------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------

-- | Testnet address from a payment key hash.
testAddr :: KeyHash 'Payment -> Addr
testAddr kh =
    Addr Testnet (KeyHashObj kh) StakeRefNull

-- | A fixed test key hash.
testKh :: KeyHash 'Payment
testKh =
    KeyHash
        $ fromJust
        $ hashFromStringAsHex @Blake2b_224
            "00000000000000000000000000\
            \00000000000000000000000000000a"

-- | Zero-fee PParams for deterministic balancing.
zeroPP :: PParams ConwayEra
zeroPP = emptyPParams

-- | Cage config for testing with testnet.
-- requestInsert\/requestDelete don't use the script
-- bytes so any value works.
testCageConfig :: CageConfig
testCageConfig =
    CageConfig
        { cageScriptBytes = SBS.toShort "dummy"
        , processTime = 300_000
        , retractTime = 600_000
        , defaultMaxFee = Coin 1_000_000
        , network = Testnet
        }

-- | Build a Provider that returns a fixed UTxO set
-- and zero-fee PParams.
mkTestProvider
    :: [(TxIn, TxOut ConwayEra)] -> Provider IO
mkTestProvider utxos =
    Provider
        { queryUTxOs = \_ -> pure utxos
        , queryProtocolParams = pure zeroPP
        , evaluateTx = \_ ->
            pure (ExUnits 0 0)
        }

-- | Dummy TrieManager that errors on use.
dummyTrieManager :: TrieManager IO
dummyTrieManager =
    TrieManager
        { withTrie = \_ _ ->
            error "dummyTrieManager: withTrie"
        , createTrie = \_ ->
            error "dummyTrieManager: createTrie"
        , deleteTrie = \_ ->
            error "dummyTrieManager: deleteTrie"
        }

-- ---------------------------------------------------------
-- Spec
-- ---------------------------------------------------------

spec :: Spec
spec = describe "Cardano.MPFS.TxBuilder.Real" $ do
    cageIdentitySpec
    requestInsertSpec
    requestDeleteSpec
    bootTokenSpec

-- ---------------------------------------------------------
-- Cage identity
-- ---------------------------------------------------------

cageIdentitySpec :: Spec
cageIdentitySpec =
    describe "cage identity" $ do
        it "cageScriptHash is 28 bytes"
            $ BS.length cageScriptHash
            `shouldBe` 28

        it "cagePolicyId wraps the script hash" $ do
            let PolicyID (ScriptHash h) = cagePolicyId
            hashToBytes h `shouldBe` cageScriptHash

        it "cageAddr is a script address"
            $ case cageAddr Testnet of
                Addr net cred _stake -> do
                    net `shouldBe` Testnet
                    cred
                        `shouldSatisfy` \case
                            ScriptHashObj _ -> True
                            _ -> False
                _ ->
                    error
                        "expected Addr, got Bootstrap"

        it "cageAddr Mainnet uses Mainnet"
            $ case cageAddr Mainnet of
                Addr net _ _ ->
                    net `shouldBe` Mainnet
                _ ->
                    error
                        "expected Addr, got Bootstrap"

-- ---------------------------------------------------------
-- requestInsert
-- ---------------------------------------------------------

requestInsertSpec :: Spec
requestInsertSpec =
    describe "requestInsert" $ do
        it "builds a balanced tx" $ do
            tx <- runRequestInsert
            let outList = toOutList tx
            -- cage output + change output
            length outList
                `shouldSatisfy` (>= 2)

        it "cage output has maxFee + 2 ADA" $ do
            tx <- runRequestInsert
            let cageOut = head (toOutList tx)
                outCoin = cageOut ^. coinTxOutL
            -- maxFee=1M + minAda=2M = 3M
            outCoin `shouldBe` Coin 3_000_000

        it "has no mint field" $ do
            tx <- runRequestInsert
            let mint = tx ^. bodyTxL . mintTxBodyL
            mint `shouldBe` mempty

        it "has no script witnesses" $ do
            tx <- runRequestInsert
            let scripts =
                    tx ^. witsTxL . scriptTxWitsL
            Map.size scripts `shouldBe` 0

        it "adds fee input to tx inputs" $ do
            (tx, feeIn) <- runRequestInsertWith
            let ins = tx ^. bodyTxL . inputsTxBodyL
            Set.member feeIn ins `shouldBe` True

-- ---------------------------------------------------------
-- requestDelete
-- ---------------------------------------------------------

requestDeleteSpec :: Spec
requestDeleteSpec =
    describe "requestDelete" $ do
        it "builds a balanced tx" $ do
            tx <- runRequestDelete
            let outList = toOutList tx
            length outList
                `shouldSatisfy` (>= 2)

        it "cage output has maxFee + 2 ADA" $ do
            tx <- runRequestDelete
            let cageOut = head (toOutList tx)
                outCoin = cageOut ^. coinTxOutL
            outCoin `shouldBe` Coin 3_000_000

        it "has no mint field" $ do
            tx <- runRequestDelete
            let mint = tx ^. bodyTxL . mintTxBodyL
            mint `shouldBe` mempty

        it "has no script witnesses" $ do
            tx <- runRequestDelete
            let scripts =
                    tx ^. witsTxL . scriptTxWitsL
            Map.size scripts `shouldBe` 0

-- ---------------------------------------------------------
-- bootToken (blueprint-dependent)
-- ---------------------------------------------------------

bootTokenSpec :: Spec
bootTokenSpec =
    describe "bootToken" $ do
        mPath <-
            runIO $ lookupEnv "MPFS_BLUEPRINT"
        case mPath of
            Nothing ->
                it
                    "skipped (MPFS_BLUEPRINT not set)"
                    (pure () :: IO ())
            Just path -> do
                ebp <- runIO $ loadBlueprint path
                case ebp of
                    Left err ->
                        it
                            ( "loads blueprint: "
                                <> err
                            )
                            ( error
                                "blueprint parse failed"
                                :: IO ()
                            )
                    Right bp -> do
                        let mCode =
                                extractCompiledCode
                                    "cage."
                                    bp
                        it "extractCompiledCode succeeds"
                            $ mCode
                            `shouldSatisfy` isJust
                        for_
                            mCode
                            bootTokenWithScript

-- | bootToken tests that require real script bytes.
bootTokenWithScript :: SBS.ShortByteString -> Spec
bootTokenWithScript scriptBytes = do
    let cfg =
            CageConfig
                { cageScriptBytes = scriptBytes
                , processTime = 300_000
                , retractTime = 600_000
                , defaultMaxFee = Coin 1_000_000
                , network = Testnet
                }

    it "builds a balanced tx" $ do
        tx <- runBootToken cfg
        let outList = toOutList tx
        length outList
            `shouldSatisfy` (>= 1)

    it "mints exactly one token" $ do
        tx <- runBootToken cfg
        let MultiAsset ma =
                tx ^. bodyTxL . mintTxBodyL
            mPolicy = Map.lookup cagePolicyId ma
        mPolicy `shouldSatisfy` isJust
        let assets = fromJust mPolicy
        -- exactly one asset minted
        Map.size assets `shouldBe` 1
        -- quantity is +1
        let qty = head (Map.elems assets)
        qty `shouldBe` 1

    it "has a script witness" $ do
        tx <- runBootToken cfg
        let scripts =
                tx ^. witsTxL . scriptTxWitsL
        Map.size scripts `shouldBe` 1

    it "has a minting redeemer" $ do
        tx <- runBootToken cfg
        let (Redeemers rdmrs) =
                tx ^. witsTxL . rdmrsTxWitsL
        Map.size rdmrs `shouldBe` 1

    it "cage output has 2 ADA" $ do
        tx <- runBootToken cfg
        let cageOut = head (toOutList tx)
            outCoin = cageOut ^. coinTxOutL
        outCoin `shouldBe` Coin 2_000_000

-- ---------------------------------------------------------
-- Runners
-- ---------------------------------------------------------

-- | Convert tx outputs to a list.
toOutList :: Tx ConwayEra -> [TxOut ConwayEra]
toOutList tx =
    foldr (:) []
        $ tx ^. bodyTxL . outputsTxBodyL

-- | Set up state + provider, run requestInsert.
runRequestInsert :: IO (Tx ConwayEra)
runRequestInsert = fst <$> runRequestInsertWith

-- | Same but also return the fee TxIn.
runRequestInsertWith
    :: IO (Tx ConwayEra, TxIn)
runRequestInsertWith = do
    (_st, _prov, builder, txIn) <- mkTestFixture
    let feeAddr = testAddr testKh
    tx <-
        requestInsert
            builder
            testTid
            "mykey"
            "myvalue"
            feeAddr
    pure (tx, txIn)

-- | Set up state + provider, run requestDelete.
runRequestDelete :: IO (Tx ConwayEra)
runRequestDelete = do
    (_st, _prov, builder, _txIn) <- mkTestFixture
    let feeAddr = testAddr testKh
    requestDelete builder testTid "mykey" feeAddr

-- | Run bootToken with a given CageConfig.
runBootToken :: CageConfig -> IO (Tx ConwayEra)
runBootToken cfg = do
    txIn1 <- generate genTxIn
    txIn2 <- generate genTxIn
    let feeAddr = testAddr testKh
        utxo1 =
            mkBasicTxOut
                feeAddr
                (inject (Coin 50_000_000))
        utxo2 =
            mkBasicTxOut
                feeAddr
                (inject (Coin 50_000_000))
        prov =
            mkTestProvider
                [(txIn1, utxo1), (txIn2, utxo2)]
    st <- mkMockState
    let builder =
            mkRealTxBuilder
                cfg
                prov
                st
                dummyTrieManager
    bootToken builder feeAddr

-- | Token ID used across tests.
testTid :: TokenId
testTid = TokenId (AssetName "test-token")

-- | Common fixture: mock state with a token,
-- provider with a 50-ADA UTxO, and a wired builder.
mkTestFixture
    :: IO (State IO, Provider IO, TxBuilder IO, TxIn)
mkTestFixture = do
    st <- mkMockState
    let ts =
            TokenState
                { owner = testKh
                , root = Root (BS.replicate 32 0)
                , maxFee = Coin 1_000_000
                }
    putToken (tokens st) testTid ts
    txIn <- generate genTxIn
    let feeAddr = testAddr testKh
        utxo =
            mkBasicTxOut
                feeAddr
                (inject (Coin 50_000_000))
        prov = mkTestProvider [(txIn, utxo)]
        builder =
            mkRealTxBuilder
                testCageConfig
                prov
                st
                dummyTrieManager
    pure (st, prov, builder, txIn)
