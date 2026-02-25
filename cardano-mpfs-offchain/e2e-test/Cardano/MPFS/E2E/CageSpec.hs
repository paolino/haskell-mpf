{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Cardano.MPFS.E2E.CageSpec
-- Description : E2E tests for the full cage protocol
-- License     : Apache-2.0
module Cardano.MPFS.E2E.CageSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word8)
import Lens.Micro ((^.))
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , runIO
    , shouldSatisfy
    )

import Cardano.Ledger.Api.Tx
    ( Tx
    , bodyTxL
    , txIdTx
    )
import Cardano.Ledger.Api.Tx.Body
    ( inputsTxBodyL
    , mintTxBodyL
    , outputsTxBodyL
    , referenceInputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Out (TxOut)
import Cardano.Ledger.BaseTypes
    ( Network (..)
    , TxIx (..)
    )
import Cardano.Ledger.Binary (serialize)
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Mary.Value
    ( MultiAsset (..)
    )
import Cardano.Ledger.TxIn (TxIn (..))

import Cardano.MPFS.Application
    ( AppConfig (..)
    , withApplication
    )
import Cardano.MPFS.Blueprint
    ( applyVersion
    , extractCompiledCode
    , loadBlueprint
    )
import Cardano.MPFS.Context (Context (..))
import Cardano.MPFS.E2E.Devnet (withCardanoNode)
import Cardano.MPFS.E2E.Setup
    ( addKeyWitness
    , devnetMagic
    , genesisAddr
    , genesisDir
    , genesisSignKey
    , keyHashFromSignKey
    )
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State
    ( Requests (..)
    , State (..)
    , Tokens (..)
    )
import Cardano.MPFS.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )
import Cardano.MPFS.Trie (TrieManager (..))
import Cardano.MPFS.TxBuilder (TxBuilder (..))
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )
import Cardano.MPFS.TxBuilder.Real.Internal
    ( cageAddrFromCfg
    , cagePolicyIdFromCfg
    , computeScriptHash
    )
import Cardano.MPFS.Types
    ( Coin (..)
    , ConwayEra
    , Operation (..)
    , Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    )

-- | Full cage protocol E2E test spec.
-- Skips when @MPFS_BLUEPRINT@ is not set.
spec :: Spec
spec = describe "Cage E2E" $ do
    mPath <-
        runIO $ lookupEnv "MPFS_BLUEPRINT"
    case mPath of
        Nothing ->
            it
                "skipped (MPFS_BLUEPRINT \
                \not set)"
                (pure () :: IO ())
        Just path -> do
            ebp <-
                runIO $ loadBlueprint path
            case ebp of
                Left err ->
                    it
                        ( "blueprint error: "
                            <> err
                        )
                        (expectationFailure err)
                Right bp ->
                    case extractCompiledCode
                        "cage."
                        bp of
                        Nothing ->
                            it "no compiled code"
                                $ expectationFailure
                                    "cage script not \
                                    \found in blueprint"
                        Just scriptBytes ->
                            let applied =
                                    applyVersion
                                        1
                                        scriptBytes
                            in  cageFlowSpec
                                    path
                                    applied

-- ---------------------------------------------------------
-- Test implementation
-- ---------------------------------------------------------

-- | Full cage flow: boot, request, update,
-- and retract.
cageFlowSpec
    :: FilePath -> SBS.ShortByteString -> Spec
cageFlowSpec bpPath scriptBytes =
    it "boot, request, update, retract"
        $ withE2E scriptBytes
        $ \sock cfg ctx -> do
            let scriptAddr =
                    cageAddrFromCfg cfg Testnet

            -- Debug: check initial genesis UTxOs
            genesisUtxosBefore <-
                queryUTxOs
                    (provider ctx)
                    genesisAddr
            putStrLn
                $ "Genesis UTxOs before boot: "
                    <> show
                        (length genesisUtxosBefore)
            mapM_
                ( \(tin, _) ->
                    putStrLn
                        $ "  " <> show tin
                )
                genesisUtxosBefore

            -- Step 1: Boot token
            unsignedBoot <-
                bootToken
                    (txBuilder ctx)
                    genesisAddr
            let signedBoot =
                    addKeyWitness
                        genesisSignKey
                        unsignedBoot

            -- Debug: print boot tx outputs
            let bootOuts =
                    signedBoot
                        ^. bodyTxL
                            . outputsTxBodyL
            putStrLn
                $ "Boot tx outputs ("
                    <> show (length bootOuts)
                    <> "):"
            mapM_
                ( \o ->
                    putStrLn
                        $ "  " <> show o
                )
                bootOuts

            bootResult <-
                submitTx
                    (submitter ctx)
                    signedBoot
            putStrLn
                $ "Submit result: "
                    <> show bootResult
            assertSubmitted bootResult
            putStrLn
                $ "Boot txId: "
                    <> show (txIdTx signedBoot)
            putStrLn
                $ "Cage address: "
                    <> show scriptAddr
            awaitTx
            -- Read node log for submit events
            let nodeLogPath =
                    takeDirectory sock
                        </> "node.log"
            nodeLog <-
                BS.readFile nodeLogPath
            let logLines = BS8.lines nodeLog
                submitLines =
                    filter
                        ( \l ->
                            BS8.isInfixOf
                                "Submit"
                                l
                                || BS8.isInfixOf
                                    "Reject"
                                    l
                                || BS8.isInfixOf
                                    "Received"
                                    l
                                || BS8.isInfixOf
                                    "Mempool"
                                    l
                        )
                        logLines
            putStrLn
                "=== Submit-related node log ==="
            mapM_ BS8.putStrLn submitLines

            -- Debug: check genesis UTxOs
            genesisUtxosAfter <-
                queryUTxOs
                    (provider ctx)
                    genesisAddr
            putStrLn
                $ "Genesis UTxOs after boot: "
                    <> show
                        (length genesisUtxosAfter)
            mapM_
                ( \(tin, tout) ->
                    putStrLn
                        $ "  "
                            <> show tin
                            <> " -> "
                            <> show tout
                )
                genesisUtxosAfter

            -- Extract TokenId from mint field
            let tokenId =
                    extractTokenId cfg signedBoot

            -- Register in mock state + trie
            createTrie
                (trieManager ctx)
                tokenId
            let ts =
                    TokenState
                        { owner =
                            keyHashFromSignKey
                                genesisSignKey
                        , root =
                            Root
                                ( BS.replicate
                                    32
                                    0
                                )
                        , maxFee =
                            Coin 1_000_000
                        , processTime =
                            30_000
                        , retractTime =
                            30_000
                        }
            putToken
                (tokens (state ctx))
                tokenId
                ts

            -- Assert: cage address has UTxO
            cageUtxos <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            putStrLn
                $ "Cage UTxOs: "
                    <> show (length cageUtxos)
            cageUtxos
                `shouldSatisfy` (not . null)

            -- Step 2: Request insert
            unsignedReq <-
                requestInsert
                    (txBuilder ctx)
                    tokenId
                    "hello"
                    "world"
                    genesisAddr
            let signedReq =
                    addKeyWitness
                        genesisSignKey
                        unsignedReq
            -- Debug: print request tx
            let reqBody = signedReq ^. bodyTxL
            putStrLn
                $ "Request tx inputs: "
                    <> show
                        (reqBody ^. inputsTxBodyL)
            putStrLn
                $ "Request tx outputs: "
                    <> show
                        ( reqBody
                            ^. outputsTxBodyL
                        )
            reqResult <-
                submitTx
                    (submitter ctx)
                    signedReq
            putStrLn
                $ "Request submit: "
                    <> show reqResult
            assertSubmitted reqResult
            awaitTx

            -- Assert: cage has more UTxOs now
            cageUtxos2 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            length cageUtxos2
                `shouldSatisfy` (> length cageUtxos)

            -- Step 3: Update token
            unsignedUpdate <-
                updateToken
                    (txBuilder ctx)
                    tokenId
                    genesisAddr
            let signedUpdate =
                    addKeyWitness
                        genesisSignKey
                        unsignedUpdate
            -- Dump for aiken simulate (before
            -- submit so UTxOs still exist)
            dumpTxForAiken
                (provider ctx)
                cfg
                bpPath
                "update"
                signedUpdate
            updateResult <-
                submitTx
                    (submitter ctx)
                    signedUpdate
            assertSubmitted updateResult
            awaitTx

            -- Assert: still has cage UTxOs but
            -- request was consumed
            cageUtxos3 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            cageUtxos3
                `shouldSatisfy` (not . null)
            -- Fewer UTxOs: state remains,
            -- request consumed
            length cageUtxos3
                `shouldSatisfy` (< length cageUtxos2)

            -- Step 4: Request + retract
            -- Submit a second request
            unsignedReq2 <-
                requestInsert
                    (txBuilder ctx)
                    tokenId
                    "bye"
                    "moon"
                    genesisAddr
            let signedReq2 =
                    addKeyWitness
                        genesisSignKey
                        unsignedReq2
            req2Result <-
                submitTx
                    (submitter ctx)
                    signedReq2
            assertSubmitted req2Result
            awaitTx

            -- Extract request TxIn (cage output
            -- is at index 0 in balanced tx)
            let req2TxIn =
                    TxIn
                        (txIdTx signedReq2)
                        (TxIx 0)
            -- Register in mock state
            let req2 =
                    Request
                        { requestToken = tokenId
                        , requestOwner =
                            keyHashFromSignKey
                                genesisSignKey
                        , requestKey = "bye"
                        , requestValue =
                            Insert "moon"
                        , requestFee =
                            Coin 1_000_000
                        , requestSubmittedAt = 0
                        }
            putRequest
                (requests (state ctx))
                req2TxIn
                req2

            cageUtxos4 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            -- Has request + state UTxOs
            length cageUtxos4
                `shouldSatisfy` (> length cageUtxos3)

            -- Wait for Phase 2 (process_time =
            -- 30s after request submitted_at)
            threadDelay 32_000_000

            -- Retract the second request
            unsignedRetract <-
                retractRequest
                    (txBuilder ctx)
                    req2TxIn
                    genesisAddr
            let signedRetract =
                    addKeyWitness
                        genesisSignKey
                        unsignedRetract
            retractResult <-
                submitTx
                    (submitter ctx)
                    signedRetract
            assertSubmitted retractResult
            awaitTx

            -- Assert: request UTxO gone
            cageUtxos5 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            length cageUtxos5
                `shouldSatisfy` (< length cageUtxos4)

-- ---------------------------------------------------------
-- Bracket
-- ---------------------------------------------------------

-- | Start a devnet node, wire a full 'Context IO',
-- wait for N2C to connect, then run the action.
-- Uses the exact system start time from the
-- genesis to avoid slot/POSIX conversion drift.
withE2E
    :: SBS.ShortByteString
    -> ( FilePath
         -> CageConfig
         -> Context IO
         -> IO a
       )
    -> IO a
withE2E scriptBytes action = do
    gDir <- genesisDir
    sysTmp <- getTemporaryDirectory
    let rocksDir =
            sysTmp </> "cardano-mpfs-e2e-rocks"
    removePathForcibly rocksDir
    createDirectoryIfMissing True rocksDir
    withCardanoNode gDir $ \sock startMs -> do
        let cfg = cageCfg scriptBytes startMs
            appCfg =
                AppConfig
                    { networkMagic =
                        devnetMagic
                    , socketPath = sock
                    , dbPath = rocksDir
                    , channelCapacity = 16
                    , cageConfig = cfg
                    , bootstrapFile = Nothing
                    }
        withApplication appCfg $ \ctx -> do
            _ <-
                queryProtocolParams
                    (provider ctx)
            action sock cfg ctx

-- ---------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------

-- | Assert that a submit result is 'Submitted'.
assertSubmitted :: SubmitResult -> IO ()
assertSubmitted (Submitted _) = pure ()
assertSubmitted (Rejected reason) =
    expectationFailure
        $ "Tx rejected: " <> show reason

-- | Extract the 'TokenId' from a boot
-- transaction's mint field.
extractTokenId
    :: CageConfig -> Tx ConwayEra -> TokenId
extractTokenId cfg tx =
    let MultiAsset ma =
            tx ^. bodyTxL . mintTxBodyL
        [(an, _)] =
            Map.toList
                ( ma
                    Map.! cagePolicyIdFromCfg cfg
                )
    in  TokenId an

-- | Wait for a transaction to be confirmed
-- (~50 devnet blocks at 0.1s slots).
awaitTx :: IO ()
awaitTx = threadDelay 5_000_000

-- ---------------------------------------------------------
-- Aiken debug dump
-- ---------------------------------------------------------

-- | Dump a signed transaction and its resolved
-- inputs to files for @aiken tx simulate@.
--
-- Resolves all spent + reference inputs by
-- querying wallet and cage addresses, then
-- calls @aiken tx simulate@ with the correct
-- slot configuration and blueprint.
dumpTxForAiken
    :: Provider IO
    -> CageConfig
    -> FilePath
    -> String
    -> Tx ConwayEra
    -> IO ()
dumpTxForAiken prov cfg bpPath label tx = do
    let ver = eraProtVerLow @ConwayEra
    -- 1. Collect all TxIns (spent + ref)
    let spentIns =
            Set.toAscList
                ( tx
                    ^. bodyTxL
                        . inputsTxBodyL
                )
        refIns =
            Set.toAscList
                ( tx
                    ^. bodyTxL
                        . referenceInputsTxBodyL
                )
        allIns = spentIns <> refIns
    -- 2. Resolve UTxOs from chain
    let scriptAddr =
            cageAddrFromCfg cfg (network cfg)
    walletUtxos <-
        queryUTxOs prov genesisAddr
    cageUtxos <-
        queryUTxOs prov scriptAddr
    let utxoMap =
            Map.fromList
                (walletUtxos <> cageUtxos)
        resolve tin =
            case Map.lookup tin utxoMap of
                Just out -> (tin, out)
                Nothing ->
                    error
                        $ "dumpTxForAiken: \
                          \unresolved "
                            <> show tin
        resolved = map resolve allIns
        txIns :: [TxIn]
        txIns = map fst resolved
        txOuts :: [TxOut ConwayEra]
        txOuts = map snd resolved
    -- 3. Write CBOR hex files
    let prefix =
            "/tmp/aiken-" <> label
    BS.writeFile
        (prefix <> "-tx.hex")
        $ toHex
        $ BSL.toStrict
        $ serialize ver tx
    BS.writeFile
        (prefix <> "-inputs.hex")
        $ toHex
        $ BSL.toStrict
        $ serialize ver txIns
    BS.writeFile
        (prefix <> "-outputs.hex")
        $ toHex
        $ BSL.toStrict
        $ serialize ver txOuts
    -- 4. Run aiken tx simulate (optional)
    putStrLn
        $ "=== aiken simulate ("
            <> label
            <> ") ==="
    result <-
        try
            $ readProcessWithExitCode
                "aiken"
                [ "tx"
                , "simulate"
                , prefix <> "-tx.hex"
                , prefix <> "-inputs.hex"
                , prefix <> "-outputs.hex"
                , "--slot-length"
                , show (slotLengthMs cfg)
                , "--zero-time"
                , show (systemStartPosixMs cfg)
                , "--zero-slot"
                , "0"
                , "--blueprint"
                , bpPath
                ]
                ""
    case result of
        Left (e :: SomeException) ->
            putStrLn
                $ "aiken not available: "
                    <> show e
        Right (exitCode, stdout', stderr') -> do
            putStrLn
                $ "Exit: " <> show exitCode
            putStrLn
                $ "Stdout:\n" <> stdout'
            putStrLn
                $ "Stderr:\n" <> stderr'

-- | Encode a 'ByteString' to hex.
toHex :: BS.ByteString -> BS.ByteString
toHex =
    BS.concatMap
        ( \w ->
            BS.pack
                [hexChar (w `div` 16), hexChar (w `mod` 16)]
        )
  where
    hexChar :: Word8 -> Word8
    hexChar n
        | n < 10 =
            n + fromIntegral (fromEnum '0')
        | otherwise =
            n
                - 10
                + fromIntegral (fromEnum 'a')

-- ---------------------------------------------------------
-- Config
-- ---------------------------------------------------------

-- | Build a 'CageConfig' from applied script bytes
-- and the system start time.
cageCfg
    :: SBS.ShortByteString -> Integer -> CageConfig
cageCfg scriptBytes startMs =
    CageConfig
        { cageScriptBytes = scriptBytes
        , cfgScriptHash =
            computeScriptHash scriptBytes
        , defaultProcessTime = 30_000
        , defaultRetractTime = 30_000
        , defaultMaxFee = Coin 1_000_000
        , network = Testnet
        , systemStartPosixMs = startMs
        , slotLengthMs = 100
        }
