{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (..), logInfoN, logWarnN)
import Corvus.Model (migrateAll)
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NetAgentClient.Spec as Spec
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Rpc.Server (runCapnpServer)
import Corvus.Server (handleGracefulShutdown, handleStartup)
import Corvus.Types (ListenAddress (..), ServerState (..), getDefaultSocketPath, newServerState, runFilteredLogging)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import Database.Persist (Entity (..), selectList, (==.))
import Database.Persist.Postgresql (createPostgresqlPool, runMigration, runSqlPool)
import Database.Persist.Sql (fromSqlKey)
import Options.Applicative
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
  , optNetdHost :: String
  , optNetdPort :: Int
  , optNoNetd :: Bool
  , optNodeAgentHost :: String
  , optNodeAgentPort :: Int
  , optNoNodeAgent :: Bool
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
    <*> strOption
      ( long "netd-host"
          <> metavar "HOST"
          <> value (fst NA.defaultNetAgentAddress)
          <> showDefault
          <> help "Hostname or IP of the corvus-netd privileged agent"
      )
    <*> option
      auto
      ( long "netd-port"
          <> metavar "PORT"
          <> value (snd NA.defaultNetAgentAddress)
          <> showDefault
          <> help "TCP port of the corvus-netd privileged agent"
      )
    <*> switch
      ( long "no-netd"
          <> help "Skip dialling corvus-netd at startup (legacy user-ns path)"
      )
    <*> strOption
      ( long "node-agent-host"
          <> metavar "HOST"
          <> value (fst NOA.defaultNodeAgentAddress)
          <> showDefault
          <> help "Hostname or IP of the per-host corvus-nodeagent"
      )
    <*> option
      auto
      ( long "node-agent-port"
          <> metavar "PORT"
          <> value (snd NOA.defaultNodeAgentAddress)
          <> showDefault
          <> help "TCP port of the per-host corvus-nodeagent"
      )
    <*> switch
      ( long "no-node-agent"
          <> help "Skip dialling corvus-nodeagent at startup (Phase 1: agent is unused)"
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
    liftIO $ runSqlPool (runMigration migrateAll) pool
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

    -- Dial corvus-netd in a separate async whose lifetime IS the
    -- Cap'n Proto connection's lifetime. The thread holds the
    -- bracket open and stashes the live cap on ServerState; on
    -- shutdown we cancel it and the bracket tears the connection
    -- down.
    netdThread <-
      if optNoNetd opts
        then do
          logInfoN
            "Starting without corvus-netd (legacy user-ns path will be used for network ops)"
          liftIO $ async $ pure ()
        else
          liftIO $ async $ runNetdConnection state' opts

    -- Same dance for corvus-nodeagent. Phase 1: the connection
    -- is opened and held but no handlers use it yet.
    nodeAgentThread <-
      if optNoNodeAgent opts
        then do
          logInfoN "Starting without corvus-nodeagent (--no-node-agent)"
          liftIO $ async $ pure ()
        else
          liftIO $ async $ runNodeAgentConnection state' opts

    -- Wait for shutdown signal
    liftIO $ waitForShutdown state'

    logInfoN "Shutting down..."
    liftIO $ cancel capnpThread
    liftIO $ cancel netdThread
    liftIO $ cancel nodeAgentThread

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

-- | Hold a connection to corvus-netd for the daemon's lifetime.
--
-- The Cap'n Proto bracket pattern means the connection stays
-- open only while 'NA.withNetAgentClient'\'s body is running.
-- We loop: dial, write the cap to 'ssNetAgent', block until
-- the daemon's shutdown flag flips, then exit the bracket
-- (which closes the TCP socket).
--
-- On connect failure we sleep 5 s and retry. Network handlers
-- read 'ssNetAgent' before each operation, so calls during the
-- retry window fall back to the legacy user-ns path (until that
-- gets ripped out in Phase 4).
runNetdConnection :: ServerState -> Options -> IO ()
runNetdConnection state opts = do
  -- Owner tag = daemon uid as a stringified decimal. The agent
  -- uses this as a ledger partition key, not a security boundary.
  uid <- getRealUserID
  let owner = T.pack (show uid)
  loop owner
  where
    loop owner = do
      result <-
        NA.withNetAgentClient
          (optNetdHost opts)
          (optNetdPort opts)
          owner
          (onConnect owner)
      shouldStop <- readTVarIO (ssShutdownFlag state)
      if shouldStop
        then pure ()
        else do
          case result of
            Right () -> pure ()
            Left _ -> threadDelay 5000000
          loop owner

    onConnect _owner (Left e) = do
      runFilteredLogging (ssLogLevel state) $
        logWarnN
          ("netd dial failed: " <> T.pack (show e) <> "; retrying in 5s")
      pure (Left ())
    onConnect _owner (Right nac) = do
      runFilteredLogging (ssLogLevel state) $
        logInfoN ("netd dial succeeded, owner=" <> NA.nacOwner nac)
      atomically $ writeTVar (ssNetAgent state) (Just nac)
      -- Reapply intent: walk the DB for `running=true` networks
      -- and call NA.applyNetwork on each. Idempotent on the
      -- agent side. Same shape for VMs' TAPs would also belong
      -- here, but VM autostart runs through the usual VmStart
      -- handler, which already plumbs the agent.
      reapplyRunningNetworks state nac
      -- Hold the connection open until shutdown is requested.
      blockUntilShutdown state
      atomically $ writeTVar (ssNetAgent state) Nothing
      pure (Right ())

-- | On (re)connect: re-apply every network the DB says should
-- be running. The agent is stateless, so this rebuilds its
-- kernel-side ledger to match the daemon's intent.
reapplyRunningNetworks :: ServerState -> NA.NetAgentClient -> IO ()
reapplyRunningNetworks state nac = do
  nets <-
    runSqlPool
      (selectList [M.NetworkRunning ==. True] [])
      (ssDbPool state)
  forM_ nets $ \(Entity nwKey nw) ->
    case Spec.networkToSpec (fromSqlKey nwKey) nw of
      Left err ->
        runFilteredLogging (ssLogLevel state) $
          logWarnN
            ( "re-apply skip network "
                <> T.pack (show (fromSqlKey nwKey))
                <> ": "
                <> err
            )
      Right spec -> do
        result <- NA.applyNetwork nac spec
        case result of
          Right _ ->
            runFilteredLogging (ssLogLevel state) $
              logInfoN
                ("re-applied network " <> M.networkName nw)
          Left e ->
            runFilteredLogging (ssLogLevel state) $
              logWarnN
                ( "re-apply failed for "
                    <> M.networkName nw
                    <> ": "
                    <> T.pack (show e)
                )

-- | Spin until 'ssShutdownFlag' flips. Lets the netd-connection
-- thread block the bracket so the TCP socket stays open.
blockUntilShutdown :: ServerState -> IO ()
blockUntilShutdown state = do
  shouldStop <- readTVarIO (ssShutdownFlag state)
  unless shouldStop $ do
    threadDelay 200000
    blockUntilShutdown state

-- | Hold a connection to corvus-nodeagent for the daemon's
-- lifetime. Mirror of 'runNetdConnection'.
--
-- Phase 1: the cap is opened, exercised once via 'sessionPing'
-- to prove the path is live, and stashed in 'ssNodeAgent' for
-- later phases. No re-apply step yet — nothing in the DB cares
-- about the node agent.
--
-- Reconnect loop: on dial / session-open failure, sleep 5 s and
-- retry until shutdown.
runNodeAgentConnection :: ServerState -> Options -> IO ()
runNodeAgentConnection state opts = do
  uid <- getRealUserID
  let owner = T.pack (show uid)
  loop owner
  where
    loop owner = do
      result <-
        NOA.withNodeAgentClient
          (optNodeAgentHost opts)
          (optNodeAgentPort opts)
          owner
          (onConnect owner)
      shouldStop <- readTVarIO (ssShutdownFlag state)
      if shouldStop
        then pure ()
        else do
          case result of
            Right () -> pure ()
            Left _ -> threadDelay 5000000
          loop owner

    onConnect _owner (Left e) = do
      runFilteredLogging (ssLogLevel state) $
        logWarnN
          ("nodeagent dial failed: " <> T.pack (show e) <> "; retrying in 5s")
      pure (Left ())
    onConnect _owner (Right noac) = do
      -- Liveness probe: prove the session cap really is live
      -- before declaring the agent reachable.
      pingResult <- NOA.sessionPing noac
      case pingResult of
        Left e -> do
          runFilteredLogging (ssLogLevel state) $
            logWarnN
              ("nodeagent session ping failed: " <> T.pack (show e))
          pure (Left ())
        Right () -> do
          runFilteredLogging (ssLogLevel state) $
            logInfoN ("nodeagent dial succeeded, owner=" <> NOA.nacOwner noac)
          atomically $ writeTVar (ssNodeAgent state) (Just noac)
          blockUntilShutdown state
          atomically $ writeTVar (ssNodeAgent state) Nothing
          pure (Right ())
