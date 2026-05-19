{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO, writeTVar)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (..), logInfoN, logWarnN)
import Corvus.Handlers.Vm (reattachVmMonitors)
import Corvus.Handlers.VmStatusSink (newDaemonVmStatusSink)
import Corvus.Model (migrateAll)
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NetAgentClient.Spec as Spec
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Rpc.Server (runCapnpServer)
import Corvus.Server (handleGracefulShutdown, handleStartup)
import Corvus.Types
  ( ListenAddress (..)
  , NodeConns (..)
  , ServerState (..)
  , clearNetConn
  , clearNodeConn
  , getDefaultSocketPath
  , newServerState
  , registerNodeConns
  , runFilteredLogging
  )
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Database.Persist (Entity (..), selectList, (==.))
import Database.Persist.Postgresql (createPostgresqlPool, runMigrationUnsafe, runSqlPool)
import Database.Persist.Sql (fromSqlKey)
import Options.Applicative
import Supervisors (withSupervisor)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import System.Posix.User (getRealUserID)

-- | Command line options for the daemon
data Options = Options
  { optSocket :: Maybe FilePath
  , optTcp :: Bool
  , optHost :: String
  , optPort :: Int
  , optDbUri :: String
  , optLogLevel :: LogLevel
  , optSpiceBind :: Maybe String
  }
  deriving (Show)

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( strOption
          ( long "socket"
              <> short 's'
              <> metavar "PATH"
              <> help "Unix socket path (default: $XDG_RUNTIME_DIR/corvus/corvus.sock)"
          )
      )
    <*> switch
      ( long "tcp"
          <> help "Use TCP instead of Unix socket"
      )
    <*> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> value "127.0.0.1"
          <> help "Host to bind to when using --tcp (default: 127.0.0.1)"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 9876
          <> help "Port to listen on when using --tcp (default: 9876)"
      )
    <*> strOption
      ( long "database"
          <> short 'd'
          <> metavar "URI"
          <> help "PostgreSQL connection URI (required). Example: postgresql://user:pass@localhost/corvus"
      )
    <*> option
      parseLogLevel
      ( long "log-level"
          <> short 'l'
          <> metavar "LEVEL"
          <> value LevelInfo
          <> help "Minimum log level: debug, info, warn, error (default: info)"
      )
    <*> optional
      ( strOption
          ( long "spice-bind"
              <> metavar "ADDR"
              <> help "Address QEMU binds SPICE to (default: mirrors --host in --tcp mode, else 127.0.0.1)"
          )
      )

-- | Full parser with info
optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Corvus daemon - VM management RPC server"
        <> header "corvus - a Haskell daemon for QEMU VM management"
    )

main :: IO ()
main = do
  -- Ensure line buffering so logs appear promptly under systemd/journal
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  opts <- execParser optsInfo
  let logLevel = optLogLevel opts
  runFilteredLogging logLevel $ do
    logInfoN $ "Connecting to database: " <> T.pack (optDbUri opts)

    -- Create database connection pool and run migrations
    pool <- createPostgresqlPool (pack $ optDbUri opts) 10

    logInfoN "Running database migrations..."
    -- 'runMigrationUnsafe' applies destructive migrations (DROP
    -- COLUMN, DROP TABLE) without prompting. The multi-node
    -- slice 1a is itself a destructive change; CLAUDE.md's
    -- no-compat-shim policy says wipe-and-reapply is fine.
    liftIO $ runSqlPool (runMigrationUnsafe migrateAll) pool
    logInfoN "Migrations complete."

    -- Initialize server state with database pool
    let spiceBind = case optSpiceBind opts of
          Just a -> T.pack a
          Nothing
            | optTcp opts -> T.pack (optHost opts)
            | otherwise -> "127.0.0.1"
        qemuConfig = defaultQemuConfig {qcSpiceBindAddress = spiceBind}
    state <- liftIO $ newServerState pool qemuConfig
    let state' = state {ssLogLevel = logLevel}

    -- Run startup handler (clean stale state, kill orphan processes)
    logInfoN "Running startup tasks..."
    liftIO $ handleStartup state' 30

    -- Install signal handlers for graceful shutdown
    let shutdownHandler = atomically $ writeTVar (ssShutdownFlag state') True

    liftIO $ installHandler sigTERM (Catch shutdownHandler) Nothing
    liftIO $ installHandler sigINT (Catch shutdownHandler) Nothing

    -- Determine listen address
    listenAddr <- liftIO $ getListenAddr opts

    -- Ensure socket directory exists for Unix sockets
    case listenAddr of
      UnixAddress path -> liftIO $ createDirectoryIfMissing True (takeDirectory path)
      TcpAddress _ _ -> pure ()

    logInfoN $ "Starting Cap'n Proto RPC server on " <> formatListenAddr listenAddr

    -- Start the Cap'n Proto RPC server in a background thread.
    capnpThread <- liftIO $ async $ runCapnpServer state' listenAddr

    -- For each node registered in the DB, fork a supervisor
    -- that holds both the nodeagent + netd connections open and
    -- reconciles per-node state (running networks, VM monitors)
    -- on every (re)connect. The supervisor list lives in
    -- 'ssAgents'; cancelling the daemon cancels each supervisor
    -- which in turn closes its Cap'n Proto brackets.
    nodeSupervisors <- liftIO $ spawnAllNodeSupervisors state'

    -- Wait for shutdown signal
    liftIO $ waitForShutdown state'

    logInfoN "Shutting down..."
    liftIO $ cancel capnpThread
    liftIO $ mapM_ cancel nodeSupervisors

    -- Run graceful shutdown handler (stop VMs, networks)
    liftIO $ handleGracefulShutdown state'

    liftIO exitSuccess

-- | Determine listen address from options
getListenAddr :: Options -> IO ListenAddress
getListenAddr opts
  | optTcp opts = pure $ TcpAddress (optHost opts) (optPort opts)
  | otherwise = case optSocket opts of
      Just path -> pure $ UnixAddress path
      Nothing -> UnixAddress <$> getDefaultSocketPath

-- | Block until shutdown flag is set
waitForShutdown :: ServerState -> IO ()
waitForShutdown state = do
  shouldShutdown <- readTVarIO (ssShutdownFlag state)
  unless shouldShutdown $ do
    threadDelay 100000 -- 100ms
    waitForShutdown state

-- | Parse a log level string
parseLogLevel :: ReadM LogLevel
parseLogLevel = eitherReader $ \s -> case s of
  "debug" -> Right LevelDebug
  "info" -> Right LevelInfo
  "warn" -> Right LevelWarn
  "error" -> Right LevelError
  _ -> Left $ "Invalid log level: " ++ s ++ " (use debug, info, warn, error)"

-- | Format listen address for logging
formatListenAddr :: ListenAddress -> T.Text
formatListenAddr (TcpAddress host port) = T.pack host <> ":" <> T.pack (show port)
formatListenAddr (UnixAddress path) = "unix:" <> T.pack path

-- | Boot-time: read every 'Node' row and fork its supervisor.
-- Returns the list of supervisor 'Async's so the daemon can
-- cancel them at shutdown. Each supervisor also self-registers
-- in 'ssAgents' (see 'spawnNodeSupervisor') so handlers can
-- find them at runtime.
spawnAllNodeSupervisors :: ServerState -> IO [Async ()]
spawnAllNodeSupervisors state = do
  nodes <- runSqlPool (selectList [] []) (ssDbPool state)
  runFilteredLogging (ssLogLevel state) $
    logInfoN $
      "Spawning supervisors for " <> T.pack (show (length nodes)) <> " node(s)"
  mapM (spawnNodeSupervisor state) nodes

-- | Fork one reconnect-loop supervisor for a single 'Node' row.
-- The supervisor maintains two parallel connect-and-hold async
-- children: one for the node's @corvus-nodeagent@, one for its
-- @corvus-netd@. Both pump updates into the 'NodeConns' entry
-- in 'ssAgents'.
--
-- The returned 'Async' is what the daemon (or @crv node delete@)
-- cancels to tear everything down for this node. The
-- 'NodeConns' entry is also stamped with this same async so
-- runtime code can locate the supervisor for cancellation.
spawnNodeSupervisor :: ServerState -> Entity M.Node -> IO (Async ())
spawnNodeSupervisor state (Entity nodeKey node) = do
  -- Pre-register an empty 'NodeConns' so handlers that look up
  -- this node before either child has finished its first dial
  -- see "agent unavailable" rather than "node not registered".
  -- The 'ncSupervisor' field is filled in by the bracket on
  -- 'sup' below — temporarily a self-reference via 'mfix'-like
  -- pattern won't work for plain async, so we install a
  -- placeholder first and patch in the real handle after fork.
  sup <- async (runNodeSupervisor state nodeKey node)
  registerNodeConns
    state
    nodeKey
    NodeConns {ncNodeAgent = Nothing, ncNetAgent = Nothing, ncSupervisor = sup}
  pure sup

-- | Per-node supervisor body: fan out two reconnect loops, one
-- for nodeagent, one for netd. Block until shutdown so the
-- daemon's 'cancel' on this 'Async' tears down both children.
runNodeSupervisor :: ServerState -> M.NodeId -> M.Node -> IO ()
runNodeSupervisor state nodeKey node = do
  uid <- getRealUserID
  let owner = T.pack (show uid)
      host = T.unpack (M.nodeHost node)
      noaPort = M.nodeNodeAgentPort node
      netPort = M.nodeNetAgentPort node
      nodeLabel = M.nodeName node
  runFilteredLogging (ssLogLevel state) $
    logInfoN $
      "node "
        <> nodeLabel
        <> " supervisor starting (nodeagent "
        <> T.pack (show noaPort)
        <> ", netd "
        <> T.pack (show netPort)
        <> ")"
  nodeAgentChild <-
    async $ runNodeAgentLoop state nodeKey nodeLabel host noaPort owner
  netdChild <-
    async $ runNetdLoop state nodeKey nodeLabel host netPort owner
  -- Block until the daemon's shutdown flag flips, then cancel
  -- both children. The outer cancel from the daemon also
  -- propagates and unwinds the connect-and-hold brackets.
  blockUntilShutdown state
  cancel nodeAgentChild
  cancel netdChild

-- | Single-node nodeagent connect-and-hold loop. Same shape as
-- the legacy daemon-wide loop, but scoped to one node.
runNodeAgentLoop
  :: ServerState
  -> M.NodeId
  -> T.Text
  -- ^ node display name (for logs)
  -> String
  -> Int
  -> T.Text
  -- ^ owner
  -> IO ()
runNodeAgentLoop state nodeKey nodeLabel host port owner = loop
  where
    loop = do
      result <-
        NOA.withNodeAgentClient host port owner onConnect
      shouldStop <- readTVarIO (ssShutdownFlag state)
      if shouldStop
        then pure ()
        else do
          case result of
            Right () -> pure ()
            Left _ -> threadDelay 5000000
          loop

    onConnect (Left e) = do
      runFilteredLogging (ssLogLevel state) $
        logWarnN $
          "node "
            <> nodeLabel
            <> " nodeagent dial failed: "
            <> T.pack (show e)
            <> "; retrying in 5s"
      pure (Left ())
    onConnect (Right noac) = do
      -- Liveness probe — sessionPing proves the cap is wired.
      pingResult <- NOA.sessionPing noac
      case pingResult of
        Left e -> do
          runFilteredLogging (ssLogLevel state) $
            logWarnN $
              "node "
                <> nodeLabel
                <> " nodeagent session ping failed: "
                <> T.pack (show e)
          pure (Left ())
        Right () -> do
          runFilteredLogging (ssLogLevel state) $
            logInfoN $
              "node "
                <> nodeLabel
                <> " nodeagent dial succeeded, owner="
                <> NOA.nacOwner noac
          atomically $
            modifyTVar' (ssAgents state) $ \m -> case Map.lookup nodeKey m of
              Nothing -> m
              Just nc -> Map.insert nodeKey nc {ncNodeAgent = Just noac} m
          withSupervisor $ \sup -> do
            sinkClient <-
              C.export @CGNA.VmStatusSink sup (newDaemonVmStatusSink state)
            subResult <- NOA.subscribeVmStatus noac sinkClient
            case subResult of
              Left e ->
                runFilteredLogging (ssLogLevel state) $
                  logWarnN $
                    "node "
                      <> nodeLabel
                      <> " subscribeVmStatus failed: "
                      <> T.pack (show e)
              Right () ->
                runFilteredLogging (ssLogLevel state) $
                  logInfoN $
                    "node " <> nodeLabel <> " subscribed to VM status push"
            -- Re-attach monitor threads. The function walks all
            -- VMs in the DB and looks up each VM's node — VMs
            -- whose node isn't this one are silently skipped.
            reattachVmMonitors state
            blockUntilShutdown state
          clearNodeConn state nodeKey
          pure (Right ())

-- | Single-node netd connect-and-hold loop.
runNetdLoop
  :: ServerState
  -> M.NodeId
  -> T.Text
  -- ^ node display name
  -> String
  -> Int
  -> T.Text
  -- ^ owner
  -> IO ()
runNetdLoop state nodeKey nodeLabel host port owner = loop
  where
    loop = do
      result <- NA.withNetAgentClient host port owner onConnect
      shouldStop <- readTVarIO (ssShutdownFlag state)
      if shouldStop
        then pure ()
        else do
          case result of
            Right () -> pure ()
            Left _ -> threadDelay 5000000
          loop

    onConnect (Left e) = do
      runFilteredLogging (ssLogLevel state) $
        logWarnN $
          "node "
            <> nodeLabel
            <> " netd dial failed: "
            <> T.pack (show e)
            <> "; retrying in 5s"
      pure (Left ())
    onConnect (Right nac) = do
      runFilteredLogging (ssLogLevel state) $
        logInfoN $
          "node "
            <> nodeLabel
            <> " netd dial succeeded, owner="
            <> NA.nacOwner nac
      atomically $
        modifyTVar' (ssAgents state) $ \m -> case Map.lookup nodeKey m of
          Nothing -> m
          Just nc -> Map.insert nodeKey nc {ncNetAgent = Just nac} m
      reapplyRunningNetworks state nodeKey nodeLabel nac
      blockUntilShutdown state
      clearNetConn state nodeKey
      pure (Right ())

-- | Re-apply networks belonging to this node. The agent is
-- stateless, so this rebuilds its kernel-side ledger to match
-- the daemon's intent for this node.
reapplyRunningNetworks
  :: ServerState -> M.NodeId -> T.Text -> NA.NetAgentClient -> IO ()
reapplyRunningNetworks state nodeKey nodeLabel nac = do
  nets <-
    runSqlPool
      ( selectList
          [M.NetworkRunning ==. True, M.NetworkNodeId ==. nodeKey]
          []
      )
      (ssDbPool state)
  forM_ nets $ \(Entity nwKey nw) ->
    case Spec.networkToSpec (fromSqlKey nwKey) nw of
      Left err ->
        runFilteredLogging (ssLogLevel state) $
          logWarnN $
            "node "
              <> nodeLabel
              <> " re-apply skip network "
              <> T.pack (show (fromSqlKey nwKey))
              <> ": "
              <> err
      Right spec -> do
        result <- NA.applyNetwork nac spec
        case result of
          Right _ ->
            runFilteredLogging (ssLogLevel state) $
              logInfoN $
                "node "
                  <> nodeLabel
                  <> " re-applied network "
                  <> M.networkName nw
          Left e ->
            runFilteredLogging (ssLogLevel state) $
              logWarnN $
                "node "
                  <> nodeLabel
                  <> " re-apply failed for "
                  <> M.networkName nw
                  <> ": "
                  <> T.pack (show e)

-- | Spin until 'ssShutdownFlag' flips. Lets the per-node loops
-- block their bracket bodies so the TCP sockets stay open until
-- daemon shutdown.
blockUntilShutdown :: ServerState -> IO ()
blockUntilShutdown state = do
  shouldStop <- readTVarIO (ssShutdownFlag state)
  unless shouldStop $ do
    threadDelay 200000
    blockUntilShutdown state
