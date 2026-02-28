{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.MPFS.TxBuilderSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set qualified as Set
import Lens.Micro ((&), (.~), (^.))
import System.Environment (lookupEnv)

import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
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
    , referenceInputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Out
    ( TxOut
    , coinTxOutL
    , datumTxOutL
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
    ( MaryValue (..)
    , MultiAsset (..)
    , PolicyID (..)
    )
import Cardano.Ledger.TxIn (TxIn)

import Cardano.MPFS.Core.Blueprint
    ( extractCompiledCode
    , loadBlueprint
    )
import Cardano.MPFS.Core.OnChain
    ( CageDatum (..)
    , OnChainOperation (..)
    , OnChainRequest (..)
    , OnChainRoot (..)
    , OnChainTokenId (..)
    , OnChainTokenState (..)
    , cageAddr
    , cagePolicyId
    , cageScriptHash
    , cageScriptHashLedger
    )
import Cardano.MPFS.Core.Types
    ( AssetName (..)
    , Coin (..)
    , ConwayEra
    , Operation (..)
    , PParams
    , Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    )
import Cardano.MPFS.Generators (genTxIn)
import Cardano.MPFS.Mock.State (mkMockState)
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State
    ( Requests (..)
    , State (..)
    , Tokens (..)
    )
import Cardano.MPFS.Trie
    ( Trie (..)
    , TrieManager (..)
    )
import Cardano.MPFS.Trie.PureManager
    ( mkPureTrieManager
    )
import Cardano.MPFS.TxBuilder (TxBuilder (..))
import Cardano.MPFS.TxBuilder.Config (CageConfig (..))
import Cardano.MPFS.TxBuilder.Real
    ( computeScriptHash
    , mkInlineDatum
    , mkRealTxBuilder
    , toPlcData
    )
import PlutusTx.Builtins.Internal
    ( BuiltinByteString (..)
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
        , cfgScriptHash = cageScriptHashLedger
        , defaultProcessTime = 300_000
        , defaultRetractTime = 600_000
        , defaultMaxFee = Coin 1_000_000
        , network = Testnet
        , systemStartPosixMs = 0
        , slotLengthMs = 100
        }

-- | Build a Provider that returns a fixed UTxO set
-- and zero-fee PParams.
mkTestProvider
    :: [(TxIn, TxOut ConwayEra)] -> Provider IO
mkTestProvider utxos =
    Provider
        { queryUTxOs = \_ -> pure utxos
        , queryProtocolParams = pure zeroPP
        , evaluateTx = \_ -> pure Map.empty
        }

-- | Dummy TrieManager that errors on use.
dummyTrieManager :: TrieManager IO
dummyTrieManager =
    TrieManager
        { withTrie = \_ _ ->
            error "dummyTrieManager: withTrie"
        , withSpeculativeTrie = \_ _ ->
            error
                "dummyTrieManager: \
                \withSpeculativeTrie"
        , createTrie = \_ ->
            error "dummyTrieManager: createTrie"
        , deleteTrie = \_ ->
            error "dummyTrieManager: deleteTrie"
        , registerTrie = \_ ->
            error "dummyTrieManager: registerTrie"
        , hideTrie = \_ ->
            error "dummyTrieManager: hideTrie"
        , unhideTrie = \_ ->
            error "dummyTrieManager: unhideTrie"
        }

-- | Build a Provider that returns different UTxOs
-- for different addresses.
mkRoutingProvider
    :: [(Addr, [(TxIn, TxOut ConwayEra)])]
    -> Provider IO
mkRoutingProvider routes =
    Provider
        { queryUTxOs = \addr ->
            pure
                $ fromMaybe
                    []
                    ( Prelude.lookup
                        addr
                        routes
                    )
        , queryProtocolParams = pure zeroPP
        , evaluateTx = \_ -> pure Map.empty
        }

-- | Build a state TxOut with the cage token.
mkStateTxOut :: TxOut ConwayEra
mkStateTxOut =
    let tokenMA =
            MultiAsset
                $ Map.singleton
                    cagePolicyId
                $ Map.singleton
                    (unTokenId testTid)
                    1
        val = MaryValue (Coin 2_000_000) tokenMA
        datum =
            StateDatum
                OnChainTokenState
                    { stateOwner =
                        BuiltinByteString
                            ( hashToBytes
                                $ let KeyHash h = testKh
                                  in  h
                            )
                    , stateRoot =
                        OnChainRoot
                            (BS.replicate 32 0)
                    , stateMaxFee = 1_000_000
                    , stateProcessTime = 300_000
                    , stateRetractTime = 600_000
                    }
    in  mkBasicTxOut (cageAddr Testnet) val
            & datumTxOutL
                .~ mkInlineDatum (toPlcData datum)

-- | Build a request TxOut.
mkRequestTxOut :: TxOut ConwayEra
mkRequestTxOut =
    let val = inject (Coin 3_000_000)
        datum =
            RequestDatum
                OnChainRequest
                    { requestToken =
                        OnChainTokenId
                            $ BuiltinByteString
                            $ SBS.fromShort
                            $ let AssetName sbs =
                                    unTokenId testTid
                              in  sbs
                    , requestOwner =
                        BuiltinByteString
                            ( hashToBytes
                                $ let KeyHash h = testKh
                                  in  h
                            )
                    , requestKey = "mykey"
                    , requestValue =
                        OpInsert "myvalue"
                    , requestFee = 1_000_000
                    , requestSubmittedAt = 0
                    }
    in  mkBasicTxOut (cageAddr Testnet) val
            & datumTxOutL
                .~ mkInlineDatum (toPlcData datum)

-- ---------------------------------------------------------
-- Spec
-- ---------------------------------------------------------

spec :: Spec
spec = describe "Cardano.MPFS.TxBuilder.Real" $ do
    cageIdentitySpec
    requestInsertSpec
    requestDeleteSpec
    retractRequestSpec
    updateTokenSpec
    endTokenSpec
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
            case toOutList tx of
                (cageOut : _) -> do
                    let outCoin = cageOut ^. coinTxOutL
                    -- maxFee=1M + minAda=2M = 3M
                    outCoin `shouldBe` Coin 3_000_000
                [] -> expectationFailure "no outputs"

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
            case toOutList tx of
                (cageOut : _) -> do
                    let outCoin = cageOut ^. coinTxOutL
                    outCoin `shouldBe` Coin 3_000_000
                [] -> expectationFailure "no outputs"

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
-- retractRequest
-- ---------------------------------------------------------

retractRequestSpec :: Spec
retractRequestSpec =
    describe "retractRequest" $ do
        it "builds a balanced tx" $ do
            tx <- runRetractRequest
            let outList = toOutList tx
            length outList
                `shouldSatisfy` (>= 1)

        it "has state UTxO as reference input" $ do
            (tx, _, stateIn) <-
                runRetractRequestWith
            let refs =
                    tx
                        ^. bodyTxL
                            . referenceInputsTxBodyL
            Set.member stateIn refs
                `shouldBe` True

        it "has a spending redeemer" $ do
            (tx, _, _) <- runRetractRequestWith
            let (Redeemers rdmrs) =
                    tx ^. witsTxL . rdmrsTxWitsL
            Map.size rdmrs `shouldBe` 1

        it "has cage script witness" $ do
            (tx, _, _) <- runRetractRequestWith
            let scripts =
                    tx ^. witsTxL . scriptTxWitsL
            Map.size scripts `shouldBe` 1

        it "consumes the request UTxO" $ do
            (tx, reqIn, _) <-
                runRetractRequestWith
            let ins =
                    tx ^. bodyTxL . inputsTxBodyL
            Set.member reqIn ins
                `shouldBe` True

-- ---------------------------------------------------------
-- updateToken
-- ---------------------------------------------------------

updateTokenSpec :: Spec
updateTokenSpec =
    describe "updateToken" $ do
        it "builds a balanced tx" $ do
            tx <- runUpdateToken
            let outList = toOutList tx
            length outList
                `shouldSatisfy` (>= 1)

        it "mints nothing" $ do
            tx <- runUpdateToken
            let mint =
                    tx ^. bodyTxL . mintTxBodyL
            mint `shouldBe` mempty

        it "has cage script witness" $ do
            tx <- runUpdateToken
            let scripts =
                    tx ^. witsTxL . scriptTxWitsL
            Map.size scripts `shouldBe` 1

        it "consumes state and request UTxOs" $ do
            (tx, stateIn, reqIn) <-
                runUpdateTokenWith
            let ins =
                    tx ^. bodyTxL . inputsTxBodyL
            Set.member stateIn ins
                `shouldBe` True
            Set.member reqIn ins
                `shouldBe` True

        it "has redeemers for state and request" $ do
            (tx, _, _) <- runUpdateTokenWith
            let (Redeemers rdmrs) =
                    tx ^. witsTxL . rdmrsTxWitsL
            -- one Modify + one Contribute
            Map.size rdmrs `shouldBe` 2

-- ---------------------------------------------------------
-- endToken
-- ---------------------------------------------------------

endTokenSpec :: Spec
endTokenSpec =
    describe "endToken" $ do
        it "builds a balanced tx" $ do
            tx <- runEndToken
            let outList = toOutList tx
            length outList
                `shouldSatisfy` (>= 1)

        it "burns exactly one token" $ do
            tx <- runEndToken
            let MultiAsset ma =
                    tx ^. bodyTxL . mintTxBodyL
                mPolicy =
                    Map.lookup cagePolicyId ma
            mPolicy `shouldSatisfy` isJust
            let assets = fromJust mPolicy
            Map.size assets `shouldBe` 1
            case Map.elems assets of
                [qty] -> qty `shouldBe` (-1)
                _ -> expectationFailure "expected 1 asset"

        it "has a script witness" $ do
            tx <- runEndToken
            let scripts =
                    tx ^. witsTxL . scriptTxWitsL
            Map.size scripts `shouldBe` 1

        it "has spending and minting redeemers" $ do
            tx <- runEndToken
            let (Redeemers rdmrs) =
                    tx ^. witsTxL . rdmrsTxWitsL
            -- one spending (End) + one minting (Burning)
            Map.size rdmrs `shouldBe` 2

        it "consumes the state UTxO" $ do
            (tx, stateIn) <- runEndTokenWith
            let ins =
                    tx ^. bodyTxL . inputsTxBodyL
            Set.member stateIn ins
                `shouldBe` True

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
                , cfgScriptHash =
                    computeScriptHash scriptBytes
                , defaultProcessTime = 300_000
                , defaultRetractTime = 600_000
                , defaultMaxFee = Coin 1_000_000
                , network = Testnet
                , systemStartPosixMs = 0
                , slotLengthMs = 100
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
        case Map.elems assets of
            [qty] -> qty `shouldBe` 1
            _ -> expectationFailure "expected 1 asset"

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
        case toOutList tx of
            (cageOut : _) -> do
                let outCoin = cageOut ^. coinTxOutL
                outCoin `shouldBe` Coin 2_000_000
            [] -> expectationFailure "no outputs"

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

-- | Run retractRequest.
runRetractRequest :: IO (Tx ConwayEra)
runRetractRequest = do
    (tx, _, _) <- runRetractRequestWith
    pure tx

-- | Run retractRequest and return details.
runRetractRequestWith
    :: IO (Tx ConwayEra, TxIn, TxIn)
runRetractRequestWith = do
    st <- mkMockState
    let ts =
            TokenState
                { owner = testKh
                , root = Root (BS.replicate 32 0)
                , maxFee = Coin 1_000_000
                , processTime = 300_000
                , retractTime = 600_000
                }
    putToken (tokens st) testTid ts
    -- Generate TxIns
    reqIn <- generate genTxIn
    stateIn <- generate genTxIn
    feeIn <- generate genTxIn
    -- Store request in state
    let req =
            Request
                { requestToken = testTid
                , requestOwner = testKh
                , requestKey = "mykey"
                , requestValue = Insert "myvalue"
                , requestFee = Coin 1_000_000
                , requestSubmittedAt = 0
                }
    putRequest (requests st) reqIn req
    -- Build cage UTxOs
    let scriptAddr = cageAddr Testnet
        feeAddr = testAddr testKh
        cageUtxos =
            [ (reqIn, mkRequestTxOut)
            , (stateIn, mkStateTxOut)
            ]
        walletUtxos =
            [
                ( feeIn
                , mkBasicTxOut
                    feeAddr
                    (inject (Coin 50_000_000))
                )
            ]
        prov =
            mkRoutingProvider
                [ (scriptAddr, cageUtxos)
                , (feeAddr, walletUtxos)
                ]
        builder =
            mkRealTxBuilder
                testCageConfig
                prov
                st
                dummyTrieManager
    tx <-
        retractRequest builder reqIn feeAddr
    pure (tx, reqIn, stateIn)

-- | Run updateToken.
runUpdateToken :: IO (Tx ConwayEra)
runUpdateToken = do
    (tx, _, _) <- runUpdateTokenWith
    pure tx

-- | Run updateToken and return details.
runUpdateTokenWith
    :: IO (Tx ConwayEra, TxIn, TxIn)
runUpdateTokenWith = do
    st <- mkMockState
    let ts =
            TokenState
                { owner = testKh
                , root = Root (BS.replicate 32 0)
                , maxFee = Coin 1_000_000
                , processTime = 300_000
                , retractTime = 600_000
                }
    putToken (tokens st) testTid ts
    -- Generate TxIns
    stateIn <- generate genTxIn
    reqIn <- generate genTxIn
    feeIn <- generate genTxIn
    -- Build cage UTxOs
    let scriptAddr = cageAddr Testnet
        feeAddr = testAddr testKh
        cageUtxos =
            [ (stateIn, mkStateTxOut)
            , (reqIn, mkRequestTxOut)
            ]
        walletUtxos =
            [
                ( feeIn
                , mkBasicTxOut
                    feeAddr
                    (inject (Coin 50_000_000))
                )
            ]
        prov =
            mkRoutingProvider
                [ (scriptAddr, cageUtxos)
                , (feeAddr, walletUtxos)
                ]
    -- Build TrieManager with data
    trieManager <- mkPureTrieManager
    createTrie trieManager testTid
    -- Insert the key so proof exists
    withTrie trieManager testTid $ \trie -> do
        _ <- insert trie "mykey" "existing"
        pure ()
    let builder =
            mkRealTxBuilder
                testCageConfig
                prov
                st
                trieManager
    tx <-
        updateToken builder testTid feeAddr
    pure (tx, stateIn, reqIn)

-- | Run endToken.
runEndToken :: IO (Tx ConwayEra)
runEndToken = fst <$> runEndTokenWith

-- | Run endToken and return details.
runEndTokenWith :: IO (Tx ConwayEra, TxIn)
runEndTokenWith = do
    st <- mkMockState
    let ts =
            TokenState
                { owner = testKh
                , root = Root (BS.replicate 32 0)
                , maxFee = Coin 1_000_000
                , processTime = 300_000
                , retractTime = 600_000
                }
    putToken (tokens st) testTid ts
    stateIn <- generate genTxIn
    feeIn <- generate genTxIn
    let scriptAddr = cageAddr Testnet
        feeAddr = testAddr testKh
        cageUtxos =
            [(stateIn, mkStateTxOut)]
        walletUtxos =
            [
                ( feeIn
                , mkBasicTxOut
                    feeAddr
                    (inject (Coin 50_000_000))
                )
            ]
        prov =
            mkRoutingProvider
                [ (scriptAddr, cageUtxos)
                , (feeAddr, walletUtxos)
                ]
        builder =
            mkRealTxBuilder
                testCageConfig
                prov
                st
                dummyTrieManager
    tx <- endToken builder testTid feeAddr
    pure (tx, stateIn)

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
                , processTime = 300_000
                , retractTime = 600_000
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
