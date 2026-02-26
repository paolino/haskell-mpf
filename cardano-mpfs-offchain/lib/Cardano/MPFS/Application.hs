-- |
-- Module      : Cardano.MPFS.Application
-- Description : Application wiring and lifecycle
-- License     : Apache-2.0
--
-- Wires all service interfaces into a 'Context IO'
-- backed by a real N2C node connection, persistent
-- RocksDB state, and persistent trie management.
module Cardano.MPFS.Application
    ( -- * Configuration
      AppConfig (..)

      -- * Lifecycle
    , withApplication

      -- * RocksDB setup
    , dbConfig
    , cageColumnFamilies
    ) where

import Control.Concurrent.Async (async, cancel)
import Control.Monad (when)

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

import Cardano.MPFS.Context (Context (..))
import Cardano.MPFS.Core.Bootstrap
    ( BootstrapHeader (..)
    , foldBootstrapEntries
    )
import Cardano.MPFS.Core.Types
    ( BlockId (..)
    , SlotNo (..)
    )
import Cardano.MPFS.Indexer.Codecs (allCodecs)
import Cardano.MPFS.Indexer.Persistent
    ( mkPersistentState
    )
import Cardano.MPFS.Mock.Skeleton
    ( mkSkeletonIndexer
    )
import Cardano.MPFS.NodeClient.Connection
    ( newLSQChannel
    , newLTxSChannel
    , runNodeClient
    )
import Cardano.MPFS.Provider.NodeClient
    ( mkNodeClientProvider
    )
import Cardano.MPFS.State
    ( Checkpoints (..)
    , State (..)
    )
import Cardano.MPFS.Submitter.N2C (mkN2CSubmitter)
import Cardano.MPFS.Trie.Persistent
    ( mkPersistentTrieManager
    )
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig
    )
import Cardano.MPFS.TxBuilder.Real
    ( mkRealTxBuilder
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

-- | Column family names and configs for the cage
-- indexer. Order must match the 'AllColumns' GADT
-- constructor order: CageTokens, CageRequests,
-- CageCfg, CageRollbacks, TrieNodes, TrieKV.
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
-- Bracket pattern: opens RocksDB for persistent
-- state, opens an N2C connection in the background,
-- wires real Provider, Submitter, persistent State
-- and TrieManager, and tears down on exit.
withApplication
    :: AppConfig -> (Context IO -> IO a) -> IO a
withApplication cfg action =
    withDBCF
        (dbPath cfg)
        dbConfig
        cageColumnFamilies
        $ \db -> do
            let columns =
                    mkColumns
                        (columnFamilies db)
                        allCodecs
                database =
                    mkRocksDBDatabase db columns
            rt <- newRunTransaction database
            let st = mkPersistentState rt
            -- Extract trie column families (5th and 6th)
            case drop 4 (columnFamilies db) of
                (nodesCF : kvCF : _) -> do
                    tm <-
                        mkPersistentTrieManager
                            db
                            nodesCF
                            kvCF
                    -- Bootstrap seeding on fresh DB
                    seedBootstrap
                        (bootstrapFile cfg)
                        st
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
                                        (cageConfig cfg)
                                        prov
                                        st
                                        tm
                                , indexer = idx
                                }
                    result <- action ctx
                    cancel nodeThread
                    pure result
                _ ->
                    error
                        "Expected at least 6 \
                        \column families"

-- | Seed a fresh database from a bootstrap CBOR file.
-- Sets the initial checkpoint so chain sync resumes
-- from the bootstrap point. No-op if the database
-- already has a checkpoint or no bootstrap file is
-- configured.
seedBootstrap
    :: Maybe FilePath -> State IO -> IO ()
seedBootstrap Nothing _ = pure ()
seedBootstrap (Just fp) st = do
    existing <-
        getCheckpoint (checkpoints st)
    when (isNothing existing) $ do
        foldBootstrapEntries
            fp
            onHeader
            (\_k _v -> pure ())
  where
    isNothing Nothing = True
    isNothing _ = False
    onHeader BootstrapHeader{..} =
        case bootstrapBlockHash of
            Nothing ->
                error
                    "Bootstrap file has no block \
                    \hash — discoverBlockHash not \
                    \yet implemented"
            Just h ->
                putCheckpoint
                    (checkpoints st)
                    (SlotNo bootstrapSlot)
                    (BlockId h)
                    []
