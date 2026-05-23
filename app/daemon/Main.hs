{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (..), logInfoN)
import Corvus.Model (migrateAll)
import Corvus.NodeSupervisor (spawnAllNodeSupervisors)
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Rpc.Server (runCapnpServer)
import Corvus.Server (handleGracefulShutdown, handleStartup)
import Corvus.Tls
  ( CertSearchPath (..)
  , TlsConfig (..)
  , TlsRole (..)
  , defaultCertSearchPath
  , loadTlsConfig
  )
import Corvus.Types
  ( ListenAddress (..)
  , NodeConns (..)
  , ServerState (..)
  , getDefaultSocketPath
  , newServerState
  , runFilteredLogging
  )
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.Persist.Postgresql (createPostgresqlPool, runMigrationUnsafe, runSqlPool)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | Command line options for the daemon.
--
-- The daemon listens on BOTH a Unix socket and TCP by default;
-- '--no-unix' / '--no-tcp' disable each individually. TLS over
-- TCP keeps a network-reachable listener safe (the listener
-- refuses to start without certs unless '--no-tls' was passed).
data Options = Options
  { optSocket :: Maybe FilePath
  , optNoUnix :: Bool
  , optNoTcp :: Bool
  , optHost :: String
  , optPort :: Int
  , optDbUri :: String
  , optLogLevel :: LogLevel
  , optSpiceBind :: Maybe String
  , optNoTls :: Bool
  , optTlsCertDir :: Maybe FilePath
  }
  deriving (Show)

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
      ( long "no-unix"
          <> help "Do not listen on the Unix socket"
      )
    <*> switch
      ( long "no-tcp"
          <> help "Do not listen on TCP"
      )
    <*> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> value "0.0.0.0"
          <> help "Host to bind to for the TCP listener (default: 0.0.0.0)"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 9876
          <> help "Port to listen on for the TCP listener (default: 9876)"
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
              <> help "Address QEMU binds SPICE to (default: mirrors --host when the TCP listener is enabled, else 127.0.0.1)"
          )
      )
    <*> switch
      ( long "no-tls"
          <> help "Disable mutual TLS on the daemon's TCP listener and on outbound nodeagent/netd dials. Unix socket connections are not affected."
      )
    <*> optional
      ( strOption
          ( long "tls-cert-dir"
              <> metavar "DIR"
              <> help "Directory containing ca.crt + corvus-daemon.crt + corvus-daemon.key (default: search $XDG_CONFIG_HOME/corvus then /etc/corvus)"
          )
      )

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
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  opts <- execParser optsInfo
  let logLevel = optLogLevel opts
  runFilteredLogging logLevel $ do
    logInfoN $ "Connecting to database: " <> T.pack (optDbUri opts)
    pool <- createPostgresqlPool (pack $ optDbUri opts) 10

    logInfoN "Running database migrations..."
    liftIO $ runSqlPool (runMigrationUnsafe migrateAll) pool
    logInfoN "Migrations complete."

    -- SPICE bind: defaults to the TCP listener's bind host when the
    -- TCP listener is enabled (so remote clients reaching the
    -- daemon over TCP can also reach SPICE), and to 127.0.0.1
    -- otherwise (Unix-only daemon means SPICE is local-only too).
    let spiceBind = case optSpiceBind opts of
          Just a -> T.pack a
          Nothing
            | not (optNoTcp opts) -> T.pack (optHost opts)
            | otherwise -> "127.0.0.1"
        qemuConfig = defaultQemuConfig {qcSpiceBindAddress = spiceBind}
    state <- liftIO $ newServerState pool qemuConfig
    tlsCfg <- liftIO $ resolveDaemonTls opts
    let state' = state {ssLogLevel = logLevel, ssTlsConfig = tlsCfg}
    case tlsCfg of
      Just c ->
        logInfoN $
          "TLS enabled; cert dir "
            <> T.pack (tcCertDir c)
            <> ", CN "
            <> tcOwnCN c
      Nothing -> logInfoN "TLS disabled (--no-tls); TCP listener and dials are plaintext"

    logInfoN "Running startup tasks..."
    liftIO $ handleStartup state' 30

    let shutdownHandler = atomically $ writeTVar (ssShutdownFlag state') True
    liftIO $ installHandler sigTERM (Catch shutdownHandler) Nothing
    liftIO $ installHandler sigINT (Catch shutdownHandler) Nothing

    listenAddrs <- liftIO $ getListenAddrs opts
    when (null listenAddrs) $
      liftIO $
        error "Corvus daemon: both --no-unix and --no-tcp set; no listener to start."
    liftIO $
      mapM_
        ( \case
            UnixAddress path -> createDirectoryIfMissing True (takeDirectory path)
            TcpAddress _ _ -> pure ()
        )
        listenAddrs

    mapM_
      (logInfoN . ("Starting Cap'n Proto RPC server on " <>) . formatListenAddr)
      listenAddrs
    capnpThreads <-
      liftIO $
        mapM (async . runCapnpServer state') listenAddrs

    -- Boot-time: spawn one supervisor per registered Node. The
    -- 'crv node add' / 'crv node delete' handlers spawn/cancel
    -- additional supervisors at runtime.
    nodeSupervisors <- liftIO $ spawnAllNodeSupervisors state'

    -- Wait for either a shutdown signal OR ANY listener thread
    -- dying. If a listener exits for any reason while we're not
    -- shutting down, the process must exit non-zero so systemd
    -- ('Restart=on-failure' / '=always') restarts us cleanly.
    liftIO $ waitForShutdownOrListenerDeath state' capnpThreads

    -- Re-check the listeners: surface a fatal log line for any
    -- that died BEFORE the rest of the teardown so the journal
    -- makes the cause obvious.
    listenerStatuses <- liftIO $ mapM poll capnpThreads
    mapM_
      ( \case
          Just (Left e) ->
            logInfoN $
              "Listener thread died unexpectedly: "
                <> T.pack (show e)
                <> "; exiting so systemd can restart the daemon"
          Just (Right _) ->
            logInfoN "Listener thread exited cleanly; exiting"
          Nothing -> pure ()
      )
      listenerStatuses

    logInfoN "Shutting down..."
    liftIO $ mapM_ cancel capnpThreads
    -- Cancel every supervisor in the registry — covers both
    -- the boot-time set and any added at runtime via @crv node
    -- add@. 'cancel' is idempotent on already-cancelled
    -- asyncs, so the boot list passed back from
    -- 'spawnAllNodeSupervisors' gets retired here too.
    liftIO $ do
      m <- readTVarIO (ssAgents state')
      mapM_ (cancel . ncSupervisor) (Map.elems m)
    -- Belt-and-braces: any supervisor we somehow missed gets
    -- cancelled here too.
    liftIO $ mapM_ cancel nodeSupervisors

    liftIO $ handleGracefulShutdown state'
    -- Exit non-zero if any listener died and we're here only
    -- because of that. Otherwise normal shutdown is success.
    finalStatuses <- liftIO $ mapM poll capnpThreads
    if any isCrash finalStatuses
      then liftIO (exitWith (ExitFailure 1))
      else liftIO exitSuccess
  where
    isCrash (Just (Left _)) = True
    isCrash _ = False

-- | Listen-address list derived from CLI flags. Both Unix and
-- TCP listeners are enabled by default; '--no-unix' / '--no-tcp'
-- disable each individually. The caller (main) refuses to start
-- if both are disabled.
getListenAddrs :: Options -> IO [ListenAddress]
getListenAddrs opts = do
  unix <-
    if optNoUnix opts
      then pure Nothing
      else
        Just
          <$> case optSocket opts of
            Just path -> pure (UnixAddress path)
            Nothing -> UnixAddress <$> getDefaultSocketPath
  let tcp =
        if optNoTcp opts
          then Nothing
          else Just (TcpAddress (optHost opts) (optPort opts))
  pure $ catMaybes [unix, tcp]

waitForShutdown :: ServerState -> IO ()
waitForShutdown state = do
  shouldShutdown <- readTVarIO (ssShutdownFlag state)
  unless shouldShutdown $ do
    threadDelay 100000
    waitForShutdown state

-- | Like 'waitForShutdown', but also returns if ANY of the
-- supplied listener 'Async's exits for any reason. The exit code
-- path in 'main' polls the asyncs after this returns to decide
-- between clean exit and a non-zero failure.
waitForShutdownOrListenerDeath :: ServerState -> [Async a] -> IO ()
waitForShutdownOrListenerDeath state listeners = go
  where
    go = do
      shouldShutdown <- readTVarIO (ssShutdownFlag state)
      if shouldShutdown
        then pure ()
        else do
          listenerStatuses <- mapM poll listeners
          if any isJust listenerStatuses
            then pure () -- one listener exited; let main exit
            else do
              threadDelay 100000
              go
    isJust (Just _) = True
    isJust Nothing = False

parseLogLevel :: ReadM LogLevel
parseLogLevel = eitherReader $ \s -> case s of
  "debug" -> Right LevelDebug
  "info" -> Right LevelInfo
  "warn" -> Right LevelWarn
  "error" -> Right LevelError
  _ -> Left $ "Invalid log level: " ++ s ++ " (use debug, info, warn, error)"

formatListenAddr :: ListenAddress -> T.Text
formatListenAddr (TcpAddress host port) = T.pack host <> ":" <> T.pack (show port)
formatListenAddr (UnixAddress path) = "unix:" <> T.pack path

-- | Resolve the daemon's TLS config given the CLI flags. 'Nothing'
-- when @--no-tls@ was passed (every TCP listener and outbound
-- dial then falls back to plain sockets). When TLS is on, refuses
-- to start if the cert trio can't be found / loaded — the
-- diagnostic includes the search path so operators know where
-- the loader looked.
resolveDaemonTls :: Options -> IO (Maybe TlsConfig)
resolveDaemonTls opts
  | optNoTls opts = pure Nothing
  | otherwise = do
      sp <- case optTlsCertDir opts of
        Just d -> pure (CertSearchPath [d])
        Nothing -> defaultCertSearchPath
      r <- loadTlsConfig sp RoleDaemon RoleClient Nothing
      case r of
        Right cfg -> pure (Just cfg)
        Left e ->
          error $
            "Corvus daemon: failed to load TLS material: "
              <> show e
              <> "\n  searched: "
              <> show (certSearchDirs sp)
              <> "\nFix with `corvus-admin deploy daemon ...` or pass --no-tls."
