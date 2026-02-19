{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Cardano.MPFS.TxBuilder.Real.Request
-- Description : Request insert/delete transactions
-- License     : Apache-2.0
module Cardano.MPFS.TxBuilder.Real.Request
    ( requestInsertImpl
    , requestDeleteImpl
    ) where

import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Sequence.Strict qualified as StrictSeq
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lens.Micro ((&), (.~), (^.))

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Api.Tx
    ( Tx
    , mkBasicTx
    )
import Cardano.Ledger.Api.Tx.Body
    ( mkBasicTxBody
    , outputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Out
    ( coinTxOutL
    , datumTxOutL
    , mkBasicTxOut
    )
import Cardano.Ledger.BaseTypes (Inject (..))

import Cardano.MPFS.Balance (balanceTx)
import Cardano.MPFS.OnChain
    ( OnChainOperation (..)
    )
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State (State (..), Tokens (..))
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )
import Cardano.MPFS.TxBuilder.Real.Internal
import Cardano.MPFS.Types
    ( Coin (..)
    , ConwayEra
    , TokenId
    , TokenState (..)
    )

-- | Build a request-insert transaction.
--
-- No script execution — just pays to the cage
-- address with an inline 'RequestDatum'.
requestInsertImpl
    :: CageConfig
    -> Provider IO
    -> State IO
    -> TokenId
    -> ByteString
    -> ByteString
    -> Addr
    -> IO (Tx ConwayEra)
requestInsertImpl cfg prov st tid key value addr =
    do
        mTs <- getToken (tokens st) tid
        TokenState{maxFee = Coin mf} <- case mTs of
            Nothing ->
                error
                    "requestInsert: unknown token"
            Just x -> pure x
        pp <- queryProtocolParams prov
        utxos <- queryUTxOs prov addr
        feeUtxo <- case sortOn
            (Down . (^. coinTxOutL) . snd)
            utxos of
            [] ->
                error "requestInsert: no UTxOs"
            (u : _) -> pure u
        now <- currentPosixMs
        let datum =
                mkRequestDatum
                    tid
                    addr
                    key
                    (OpInsert value)
                    mf
                    now
            scriptAddr = cageAddrFromCfg cfg (network cfg)
            minAda = Coin (2_000_000 + mf)
            txOut =
                mkBasicTxOut
                    scriptAddr
                    (inject minAda)
                    & datumTxOutL
                        .~ mkInlineDatum datum
            body =
                mkBasicTxBody
                    & outputsTxBodyL
                        .~ StrictSeq.singleton
                            txOut
            tx = mkBasicTx body
        case balanceTx pp [feeUtxo] addr tx of
            Left err ->
                error
                    $ "requestInsert: "
                        <> show err
            Right balanced -> pure balanced

-- | Build a request-delete transaction.
-- Same structure as requestInsert with 'OpDelete'.
requestDeleteImpl
    :: CageConfig
    -> Provider IO
    -> State IO
    -> TokenId
    -> ByteString
    -> Addr
    -> IO (Tx ConwayEra)
requestDeleteImpl cfg prov st tid key addr = do
    mTs <- getToken (tokens st) tid
    TokenState{maxFee = Coin mf} <- case mTs of
        Nothing ->
            error "requestDelete: unknown token"
        Just x -> pure x
    pp <- queryProtocolParams prov
    utxos <- queryUTxOs prov addr
    feeUtxo <- case sortOn
        (Down . (^. coinTxOutL) . snd)
        utxos of
        [] -> error "requestDelete: no UTxOs"
        (u : _) -> pure u
    now <- currentPosixMs
    let datum =
            mkRequestDatum
                tid
                addr
                key
                (OpDelete key)
                mf
                now
        scriptAddr = cageAddrFromCfg cfg (network cfg)
        minAda = Coin (2_000_000 + mf)
        txOut =
            mkBasicTxOut
                scriptAddr
                (inject minAda)
                & datumTxOutL
                    .~ mkInlineDatum datum
        body =
            mkBasicTxBody
                & outputsTxBodyL
                    .~ StrictSeq.singleton txOut
        tx = mkBasicTx body
    case balanceTx pp [feeUtxo] addr tx of
        Left err ->
            error
                $ "requestDelete: "
                    <> show err
        Right balanced -> pure balanced

-- | Get current POSIX time in milliseconds.
currentPosixMs :: IO Integer
currentPosixMs = do
    t <- getPOSIXTime
    pure $ floor (t * 1000)
