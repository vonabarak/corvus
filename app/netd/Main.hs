{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for `corvus-netd`, the privileged network agent.
--
-- Phase 2.5 lifecycle:
--
--   1. Parse CLI flags + configure logging.
--   2. Install SIGTERM / SIGINT handler that signals the
--      shutdown MVar.
--   3. Spawn the server async; block on the shutdown MVar in
--      the main thread.
--   4. On shutdown signal: cancel the server async; run
--      'cleanupCorvusKernelState'; exit 0.
--
-- systemd drives shutdown via SIGTERM with TimeoutStopSec=30s.
-- The agent's own startup pass also runs cleanup, so each
-- (re)start begins from an empty kernel state.
module Main where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Logger (LogLevel (..), logInfoN)
import Corvus.Netd.Cleanup (cleanupCorvusKernelState)
import Corvus.Netd.Server (defaultNetdHost, defaultNetdPort, runNetdServer)
import Corvus.Tls
  ( CertSearchPath (..)
  , TlsConfig (..)
  , TlsRole (..)
  , defaultCertSearchPath
  , loadTlsConfig
  )
import Corvus.Types (runFilteredLogging)
import qualified Data.Text as T
import Options.Applicative
import System.Exit (exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Posix.Signals
  ( Handler (Catch)
  , installHandler
  , sigINT
  , sigTERM
  )

data Options = Options
  { optHost :: String
  , optPort :: Int
  , optLogLevel :: LogLevel
  , optNoTls :: Bool
  , optTlsCertDir :: Maybe FilePath
  }
  deriving (Show)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case s of
  "debug" -> Right LevelDebug
  "info" -> Right LevelInfo
  "warn" -> Right LevelWarn
  "error" -> Right LevelError
  _ -> Left $ "unknown log level: " <> s

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> value defaultNetdHost
          <> showDefault
          <> help "Host to bind to"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value defaultNetdPort
          <> showDefault
          <> help "TCP port to listen on"
      )
    <*> option
      (eitherReader parseLogLevel)
      ( long "log-level"
          <> metavar "LEVEL"
          <> value LevelInfo
          <> showDefault
          <> help "Log level (debug|info|warn|error)"
      )
    <*> switch
      ( long "no-tls"
          <> help "Disable mutual TLS on the listener (dev-only)."
      )
    <*> optional
      ( strOption
          ( long "tls-cert-dir"
              <> metavar "DIR"
              <> help "Directory containing ca.crt + corvus-netd.crt + corvus-netd.key (default: search /etc/corvus then $XDG_CONFIG_HOME/corvus)"
          )
      )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Corvus privileged network agent"
            <> header "corvus-netd — privileged side of the Corvus VM daemon"
        )

  -- SIGTERM/SIGINT signal the shutdown MVar; the main thread
  -- blocks on it until a signal arrives, then unwinds.
  stopMV <- newEmptyMVar
  let raiseStop = putMVar stopMV ()
  void $ installHandler sigTERM (Catch raiseStop) Nothing
  void $ installHandler sigINT (Catch raiseStop) Nothing

  tlsCfg <- resolveNetdTls opts
  runFilteredLogging (optLogLevel opts) $ do
    logInfoN $
      "corvus-netd starting on "
        <> T.pack (optHost opts)
        <> ":"
        <> T.pack (show (optPort opts))
    case tlsCfg of
      Just c ->
        logInfoN $
          "TLS enabled; cert dir "
            <> T.pack (tcCertDir c)
            <> ", CN "
            <> tcOwnCN c
      Nothing -> logInfoN "TLS disabled (--no-tls)"

  -- Run the server async. The server itself performs startup
  -- cleanup (in Corvus.Netd.Server.runNetdServer) before it
  -- binds the listener.
  serverThread <-
    async $ runNetdServer (optHost opts) (optPort opts) tlsCfg

  -- Block until SIGTERM/SIGINT.
  takeMVar stopMV
  runFilteredLogging (optLogLevel opts) $
    logInfoN "corvus-netd shutting down; running cleanup"
  cancel serverThread
  cleanupCorvusKernelState
  runFilteredLogging (optLogLevel opts) $
    logInfoN "corvus-netd cleanup complete"
  exitSuccess

-- | Resolve corvus-netd's TLS config from CLI flags. Same pattern
-- as the daemon / nodeagent — refuses to start on load failure
-- with a diagnostic pointing at the search path and at
-- @corvus-admin deploy netd ...@.
resolveNetdTls :: Options -> IO (Maybe TlsConfig)
resolveNetdTls opts
  | optNoTls opts = pure Nothing
  | otherwise = do
      sp <- case optTlsCertDir opts of
        Just d -> pure (CertSearchPath [d])
        Nothing -> defaultCertSearchPath
      r <- loadTlsConfig sp RoleNetd RoleDaemon Nothing
      case r of
        Right cfg -> pure (Just cfg)
        Left e ->
          error $
            "corvus-netd: failed to load TLS material: "
              <> show e
              <> "\n  searched: "
              <> show (certSearchDirs sp)
              <> "\nFix with `corvus-admin deploy netd ...` or pass --no-tls."
