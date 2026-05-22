{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Monad (unless)
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
import qualified Data.Text as T
import Database.Persist.Postgresql (createPostgresqlPool, runMigrationUnsafe, runSqlPool)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | Command line options for the daemon
data Options = Options
  { optSocket :: Maybe FilePath
  , optTcp :: Bool
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

    let spiceBind = case optSpiceBind opts of
          Just a -> T.pack a
          Nothing
            | optTcp opts -> T.pack (optHost opts)
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

    listenAddr <- liftIO $ getListenAddr opts
    case listenAddr of
      UnixAddress path -> liftIO $ createDirectoryIfMissing True (takeDirectory path)
      TcpAddress _ _ -> pure ()

    logInfoN $ "Starting Cap'n Proto RPC server on " <> formatListenAddr listenAddr
    capnpThread <- liftIO $ async $ runCapnpServer state' listenAddr

    -- Boot-time: spawn one supervisor per registered Node. The
    -- 'crv node add' / 'crv node delete' handlers spawn/cancel
    -- additional supervisors at runtime.
    nodeSupervisors <- liftIO $ spawnAllNodeSupervisors state'

    -- Wait for either a shutdown signal OR the listener thread
    -- dying. If 'capnpThread' exits for any reason while we're
    -- not shutting down, the process must exit non-zero so
    -- systemd ('Restart=on-failure' / '=always') restarts us
    -- cleanly. The previous code waited only on 'ssShutdownFlag'
    -- and so could end up in a zombie state — process alive,
    -- listener dead — with the Unix-socket file unlinked by
    -- 'runCapnpServer's bracket cleanup. Operators saw this as
    -- "ENOENT on the daemon socket" with no obvious way to
    -- recover short of manually 'systemctl restart'-ing.
    liftIO $ waitForShutdownOrListenerDeath state' capnpThread

    -- Re-check the listener: if it died, surface a fatal log
    -- line BEFORE the rest of the teardown so the journal makes
    -- the cause obvious. Then exit non-zero.
    listenerDied <- liftIO $ poll capnpThread
    case listenerDied of
      Just (Left e) ->
        logInfoN $
          "Listener thread died unexpectedly: "
            <> T.pack (show e)
            <> "; exiting so systemd can restart the daemon"
      Just (Right _) ->
        logInfoN "Listener thread exited cleanly; exiting"
      Nothing -> pure ()

    logInfoN "Shutting down..."
    liftIO $ cancel capnpThread
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
    -- Exit non-zero if the listener died and we're here only
    -- because of that. Otherwise normal shutdown is success.
    listenerDied' <- liftIO $ poll capnpThread
    case listenerDied' of
      Just (Left _) -> liftIO (exitWith (ExitFailure 1))
      _ -> liftIO exitSuccess

getListenAddr :: Options -> IO ListenAddress
getListenAddr opts
  | optTcp opts = pure $ TcpAddress (optHost opts) (optPort opts)
  | otherwise = case optSocket opts of
      Just path -> pure $ UnixAddress path
      Nothing -> UnixAddress <$> getDefaultSocketPath

waitForShutdown :: ServerState -> IO ()
waitForShutdown state = do
  shouldShutdown <- readTVarIO (ssShutdownFlag state)
  unless shouldShutdown $ do
    threadDelay 100000
    waitForShutdown state

-- | Like 'waitForShutdown', but also returns if the supplied
-- listener 'Async' exits for any reason. The exit code path
-- in 'main' polls the async after this returns to decide
-- between clean exit and a non-zero failure.
waitForShutdownOrListenerDeath :: ServerState -> Async a -> IO ()
waitForShutdownOrListenerDeath state listener = go
  where
    go = do
      shouldShutdown <- readTVarIO (ssShutdownFlag state)
      if shouldShutdown
        then pure ()
        else do
          listenerStatus <- poll listener
          case listenerStatus of
            Just _ -> pure () -- listener exited; let main exit
            Nothing -> do
              threadDelay 100000
              go

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
