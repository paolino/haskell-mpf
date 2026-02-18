{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.E2E.Devnet
-- Description : Cardano-node subprocess for E2E tests
-- License     : Apache-2.0
module Cardano.MPFS.E2E.Devnet
    ( withCardanoNode
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception
    ( bracket
    , onException
    )
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , addUTCTime
    , getCurrentTime
    )
import Data.Time.Clock.POSIX
    ( utcTimeToPOSIXSeconds
    )
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    )
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO
    ( BufferMode (..)
    , Handle
    , IOMode (..)
    , hClose
    , hSetBuffering
    , openFile
    )
import System.Process
    ( CreateProcess (..)
    , ProcessHandle
    , StdStream (..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )

-- | Run a @cardano-node@ subprocess using the
-- genesis files from @srcGenesis@. The callback
-- receives the node socket path. The node and its
-- temp directory are cleaned up on exit.
withCardanoNode
    :: FilePath -> (FilePath -> IO a) -> IO a
withCardanoNode srcGenesis action = do
    tmpDir <- prepareTmpDir srcGenesis
    logH <- openFile (tmpDir </> "node.log") WriteMode
    hSetBuffering logH LineBuffering
    bracket
        (launchNode tmpDir logH)
        (cleanup tmpDir logH)
        $ \_ -> do
            let sock = tmpDir </> "node.sock"
            waitForSocket sock 300
            action sock
                `onException` dumpNodeLog
                    (tmpDir </> "node.log")

-- | Prepare a temporary directory with patched
-- genesis files and delegate keys.
prepareTmpDir :: FilePath -> IO FilePath
prepareTmpDir srcGenesis = do
    sysTmp <- getTemporaryDirectory
    let tmpDir = sysTmp </> "cardano-mpfs-e2e"
    removePathForcibly tmpDir
    createDirectoryIfMissing True tmpDir
    createDirectoryIfMissing
        True
        (tmpDir </> "db")
    createDirectoryIfMissing
        True
        (tmpDir </> "delegate-keys")
    -- Copy genesis files
    let cp name =
            copyFile
                (srcGenesis </> name)
                (tmpDir </> name)
    cp "alonzo-genesis.json"
    cp "conway-genesis.json"
    cp "node-config.json"
    cp "topology.json"
    -- Patch genesis start times to a few seconds
    -- in the future so the node has time to
    -- initialise before the first slot.
    now <- getCurrentTime
    let startTime = addUTCTime startOffset now
    patchShelleyGenesis startTime srcGenesis tmpDir
    patchByronGenesis startTime srcGenesis tmpDir
    -- Copy delegate keys
    let srcKeys = srcGenesis </> "delegate-keys"
        dstKeys = tmpDir </> "delegate-keys"
    copyFile
        (srcKeys </> "delegate1.kes.skey")
        (dstKeys </> "delegate1.kes.skey")
    copyFile
        (srcKeys </> "delegate1.vrf.skey")
        (dstKeys </> "delegate1.vrf.skey")
    copyFile
        (srcKeys </> "delegate1.opcert")
        (dstKeys </> "delegate1.opcert")
    pure tmpDir

-- | Copy shelley-genesis.json, replacing
-- @PLACEHOLDER@ with the current UTC time.
patchShelleyGenesis
    :: UTCTime -> FilePath -> FilePath -> IO ()
patchShelleyGenesis now srcDir dstDir = do
    let timeStr =
            BS8.pack
                $ formatTime
                    defaultTimeLocale
                    "%Y-%m-%dT%H:%M:%SZ"
                    now
    content <-
        BS.readFile
            (srcDir </> "shelley-genesis.json")
    let patched =
            replaceSubstring
                "PLACEHOLDER"
                timeStr
                content
    BS.writeFile
        (dstDir </> "shelley-genesis.json")
        patched

-- | Copy byron-genesis.json, replacing
-- @\"startTime\": 0@ with the current UNIX time.
patchByronGenesis
    :: UTCTime -> FilePath -> FilePath -> IO ()
patchByronGenesis now srcDir dstDir = do
    let epoch =
            BS8.pack
                $ show
                    ( floor (utcTimeToPOSIXSeconds now)
                        :: Int
                    )
    content <-
        BS.readFile
            (srcDir </> "byron-genesis.json")
    let patched =
            replaceSubstring
                "\"startTime\": 0"
                ("\"startTime\": " <> epoch)
                content
    BS.writeFile
        (dstDir </> "byron-genesis.json")
        patched

-- | Replace the first occurrence of @needle@ in a
-- ByteString.
replaceSubstring
    :: BS.ByteString
    -> BS.ByteString
    -> BS.ByteString
    -> BS.ByteString
replaceSubstring needle replacement content =
    let (before, after) =
            BS.breakSubstring needle content
    in  if BS.null after
            then content
            else
                before
                    <> replacement
                    <> BS.drop
                        (BS.length needle)
                        after

-- | Launch @cardano-node run@ as a subprocess.
launchNode
    :: FilePath -> Handle -> IO ProcessHandle
launchNode tmpDir logH = do
    let keysDir = tmpDir </> "delegate-keys"
        args =
            [ "run"
            , "--config"
            , tmpDir </> "node-config.json"
            , "--topology"
            , tmpDir </> "topology.json"
            , "--database-path"
            , tmpDir </> "db"
            , "--socket-path"
            , tmpDir </> "node.sock"
            , "--shelley-kes-key"
            , keysDir </> "delegate1.kes.skey"
            , "--shelley-vrf-key"
            , keysDir </> "delegate1.vrf.skey"
            , "--shelley-operational-certificate"
            , keysDir </> "delegate1.opcert"
            ]
        cp =
            (proc "cardano-node" args)
                { std_out = UseHandle logH
                , std_err = UseHandle logH
                }
    (_, _, _, ph) <- createProcess cp
    pure ph

-- | Terminate the node process and clean up.
cleanup
    :: FilePath
    -> Handle
    -> ProcessHandle
    -> IO ()
cleanup tmpDir logH ph = do
    terminateProcess ph
    void $ waitForProcess ph
    hClose logH
    removePathForcibly tmpDir
        `onException` pure ()

-- | Print the last 50 lines of the node log.
dumpNodeLog :: FilePath -> IO ()
dumpNodeLog logPath = do
    logContent <- BS.readFile logPath
    let allLines = BS8.lines logContent
        tailLines =
            drop
                (max 0 (length allLines - 50))
                allLines
    BS8.putStrLn
        ( BS8.unlines
            ( "=== Node log (last 50) ==="
                : tailLines
            )
        )

-- | How far in the future to set the genesis
-- @systemStart@. Gives the node time to
-- initialise before the first slot arrives.
startOffset :: NominalDiffTime
startOffset = 5

-- | Poll for the socket file every 100ms,
-- up to @n@ attempts (30s at 300 attempts).
waitForSocket :: FilePath -> Int -> IO ()
waitForSocket _ 0 =
    error
        "Timed out waiting for \
        \cardano-node socket"
waitForSocket path n = do
    exists <- doesFileExist path
    unless exists $ do
        threadDelay 100_000
        waitForSocket path (n - 1)
