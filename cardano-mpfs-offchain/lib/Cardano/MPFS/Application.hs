-- |
-- Module      : Cardano.MPFS.Application
-- Description : Application wiring and lifecycle
-- License     : Apache-2.0
--
-- Top-level wiring module that assembles all
-- service interfaces into a fully operational
-- 'Context IO'. The bracket 'withApplication' opens
-- a shared RocksDB database with 10 column families
-- (4 UTxO + 6 cage\/trie), connects to a local
-- Cardano node via two N2C connections, and builds
-- the production 'Provider', 'Submitter', persistent
-- 'State', persistent 'TrieManager', real
-- 'TxBuilder', and a 'CageFollower' that processes
-- blocks from ChainSync. On exit it cancels both
-- connection threads and closes the database.
--
-- Connection 1: ChainSync via cardano-utxo-csmt —
-- blocks processed by 'CageFollower'.
-- Connection 2: LocalStateQuery + LocalTxSubmission
-- for UTxO queries, protocol params, and tx
-- submission.
--
-- Optionally seeds a fresh database from a CBOR
-- bootstrap file (see "Cardano.MPFS.Core.Bootstrap")
-- so chain sync can resume from the bootstrap point
-- rather than genesis.
module Cardano.MPFS.Application
    ( -- * Configuration
      AppConfig (..)

      -- * Lifecycle
    , withApplication

      -- * RocksDB setup
    , dbConfig
    , allColumnFamilies
    , cageColumnFamilies
    ) where

import Control.Concurrent.Async
    ( async
    , cancel
    , link
    )
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Tracer (nullTracer)

import Data.ByteString.Lazy qualified as BSL

import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction
    ( newRunTransaction
    )
import Database.RocksDB
    ( Config (..)
    , DB (..)
    , withDBCF
    )
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Point (WithOrigin (..))

import Cardano.UTxOCSMT.Application.ChainSyncN2C
    ( mkN2CChainSyncApplication
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    , insertCSMT
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( State (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRocksDBState
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( armageddonParams
    , context
    , prisms
    , slotHash
    )
import Cardano.UTxOCSMT.Ouroboros.ConnectionN2C
    ( runLocalNodeApplication
    )
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Point
    )

import Ouroboros.Network.Block qualified as Network

import Cardano.MPFS.Context (Context (..))
import Cardano.MPFS.Core.Bootstrap
    ( BootstrapHeader (..)
    , foldBootstrapEntries
    )
import Cardano.MPFS.Core.Types
    ( BlockId (..)
    , SlotNo (..)
    )
import Cardano.MPFS.Indexer.CageFollower
    ( mkCageIntersector
    )
import Cardano.MPFS.Indexer.Codecs (allCodecs)
import Cardano.MPFS.Indexer.Persistent
    ( mkPersistentState
    )
import Cardano.MPFS.Mock.Skeleton
    ( mkSkeletonIndexer
    )
import Cardano.MPFS.Provider.NodeClient
    ( mkNodeClientProvider
    )
import Cardano.MPFS.State qualified as CageSt
import Cardano.MPFS.Submitter.N2C (mkN2CSubmitter)
import Cardano.MPFS.Trie.Persistent
    ( mkPersistentTrieManager
    )
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )
import Cardano.MPFS.TxBuilder.Real
    ( mkRealTxBuilder
    )
import Cardano.Node.Client.N2C.Connection
    ( newLSQChannel
    , newLTxSChannel
    , runNodeClient
    )

-- | Application configuration.
data AppConfig = AppConfig
    { networkMagic :: !NetworkMagic
    -- ^ Network magic (e.g. mainnet, preview)
    , socketPath :: !FilePath
    -- ^ Path to the cardano-node Unix socket
    , dbPath :: !FilePath
    -- ^ Path to the RocksDB database directory
    , channelCapacity :: !Int
    -- ^ TBQueue capacity for N2C channels
    , cageConfig :: !CageConfig
    -- ^ Cage script and protocol parameters
    , bootstrapFile :: !(Maybe FilePath)
    -- ^ CBOR bootstrap file for fresh DB seeding
    , followerEnabled :: !Bool
    -- ^ Start CageFollower ChainSync processing
    }

-- | Default RocksDB configuration.
dbConfig :: Config
dbConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | All column families: 4 UTxO (cardano-utxo-csmt)
-- followed by 6 cage\/trie. Order matters —
-- cardano-utxo-csmt consumes the first 4 via its
-- internal 'Columns' GADT, and our 'AllColumns'
-- GADT consumes the remaining 6.
-- | All column families: 4 UTxO (cardano-utxo-csmt)
-- followed by 6 cage\/trie.
allColumnFamilies :: [(String, Config)]
allColumnFamilies =
    utxoColumnFamilies <> cageColumnFamilies
  where
    utxoColumnFamilies =
        [ ("kv", dbConfig)
        , ("csmt", dbConfig)
        , ("rollbacks", dbConfig)
        , ("config", dbConfig)
        ]

-- | Cage-only column families (6). Used by tests
-- that don't need the UTxO index.
cageColumnFamilies :: [(String, Config)]
cageColumnFamilies =
    [ ("tokens", dbConfig)
    , ("requests", dbConfig)
    , ("cage-cfg", dbConfig)
    , ("cage-rollbacks", dbConfig)
    , ("trie-nodes", dbConfig)
    , ("trie-kv", dbConfig)
    ]

-- | Run an action with a fully wired 'Context IO'.
--
-- Opens RocksDB with 10 column families, creates
-- the UTxO state machine and cage state, starts
-- two N2C connections (ChainSync + LSQ\/LTxS),
-- and tears down on exit.
withApplication
    :: AppConfig
    -- ^ Application configuration
    -> (Context IO -> IO a)
    -- ^ Action receiving the fully wired context
    -> IO a
withApplication cfg action =
    withDBCF
        (dbPath cfg)
        dbConfig
        allColumnFamilies
        $ \db -> do
            -- Cage state: columns 5–10 (skip 4 UTxO CFs)
            let cageCols =
                    mkColumns
                        (drop 4 $ columnFamilies db)
                        allCodecs
                cageDb =
                    mkRocksDBDatabase db cageCols
            rt <- newRunTransaction cageDb
            let st = mkPersistentState rt
            -- Trie: columns 9–10 (skip 8)
            case drop 8 (columnFamilies db) of
                (nodesCF : kvCF : _) -> do
                    tm <-
                        mkPersistentTrieManager
                            db
                            nodesCF
                            kvCF
                    -- UTxO state machine (columns 1–4)
                    ( (utxoUpdate, availPts)
                        , runner
                        ) <-
                        newRocksDBState
                            nullTracer
                            db
                            prisms
                            context
                            slotHash
                            (\_ _ -> pure ())
                            armageddonParams

                    -- Bootstrap seeding on fresh DB
                    seedBootstrap
                        (bootstrapFile cfg)
                        st
                        runner

                    let startPts :: [Point]
                        startPts =
                            if null availPts
                                then
                                    [ Network.Point
                                        Origin
                                    ]
                                else availPts

                    -- Connection 1: ChainSync
                    -- (optional, controlled by
                    -- followerEnabled)
                    mChainThread <-
                        if followerEnabled cfg
                            then do
                                let cageIntersector =
                                        mkCageIntersector
                                            ( cfgScriptHash
                                                $ cageConfig
                                                    cfg
                                            )
                                            st
                                            tm
                                            rt
                                            (\_ -> pure Nothing)
                                            (Syncing utxoUpdate)
                                    chainSyncApp =
                                        mkN2CChainSyncApplication
                                            nullTracer
                                            nullTracer
                                            nullTracer
                                            (\_ -> pure ())
                                            (pure ())
                                            Nothing
                                            cageIntersector
                                            startPts
                                t <-
                                    async $ do
                                        er <-
                                            runLocalNodeApplication
                                                (networkMagic cfg)
                                                (socketPath cfg)
                                                chainSyncApp
                                        case er of
                                            Left e ->
                                                throwIO e
                                            Right () ->
                                                pure ()
                                link t
                                pure (Just t)
                            else pure Nothing

                    -- Connection 2: LSQ + LTxS
                    idx <- mkSkeletonIndexer
                    lsqCh <-
                        newLSQChannel
                            (channelCapacity cfg)
                    ltxsCh <-
                        newLTxSChannel
                            (channelCapacity cfg)
                    nodeThread <-
                        async
                            $ runNodeClient
                                (networkMagic cfg)
                                (socketPath cfg)
                                lsqCh
                                ltxsCh
                    let prov =
                            mkNodeClientProvider
                                lsqCh
                        ctx =
                            Context
                                { provider = prov
                                , submitter =
                                    mkN2CSubmitter
                                        ltxsCh
                                , state = st
                                , trieManager = tm
                                , txBuilder =
                                    mkRealTxBuilder
                                        ( cageConfig
                                            cfg
                                        )
                                        prov
                                        st
                                        tm
                                , indexer = idx
                                }
                    result <- action ctx
                    mapM_ cancel mChainThread
                    cancel nodeThread
                    pure result
                _ ->
                    error
                        "Expected at least 10 \
                        \column families"

-- | Seed a fresh database from a bootstrap CBOR
-- file. Sets the initial checkpoint and inserts
-- genesis UTxOs into the CSMT so chain sync can
-- resume from the bootstrap point. No-op if the
-- database already has a checkpoint or no
-- bootstrap file is configured.
seedBootstrap
    :: Maybe FilePath
    -> CageSt.State IO
    -> RunCSMTTransaction cf op slot hash BSL.ByteString BSL.ByteString IO
    -> IO ()
seedBootstrap Nothing _ _ = pure ()
seedBootstrap (Just fp) st runner = do
    existing <-
        CageSt.getCheckpoint
            (CageSt.checkpoints st)
    when (isNothing existing) $ do
        foldBootstrapEntries
            fp
            onHeader
            onEntry
  where
    isNothing Nothing = True
    isNothing _ = False
    onHeader BootstrapHeader{..} =
        case bootstrapBlockHash of
            Nothing ->
                -- Genesis bootstrap: no block hash,
                -- no checkpoint. ChainSync starts
                -- from Origin.
                pure ()
            Just h ->
                CageSt.putCheckpoint
                    (CageSt.checkpoints st)
                    (SlotNo bootstrapSlot)
                    (BlockId h)
                    []
    onEntry k v =
        txRunTransaction runner
            $ insertCSMT
                (BSL.fromStrict k)
                (BSL.fromStrict v)
