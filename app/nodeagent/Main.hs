{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for `corvus-nodeagent`, the per-host agent that
-- owns local-machine state (QEMU + virtiofsd supervision, disk
-- I/O, console buffers, guest-agent polling). Phase 1 ships the
-- bootstrap skeleton only.
--
-- Lifecycle (mirror of `corvus-netd`):
--
--   1. Parse CLI flags + configure logging.
--   2. Install SIGTERM / SIGINT handler that signals the
--      shutdown MVar.
--   3. Spawn the server async; block on the shutdown MVar in
--      the main thread.
--   4. On shutdown signal: cancel the server async; run
--      'cleanupCorvusProcesses'; exit 0.
--
-- systemd drives shutdown via SIGTERM with TimeoutStopSec=30s.
-- The agent's own startup pass also runs cleanup, so each
-- (re)start begins from an empty local state.
module Main where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Logger (LogLevel (..), logInfoN)
import Corvus.Node.Cleanup (cleanupCorvusProcesses)
import Corvus.Node.Server (defaultNodeAgentHost, defaultNodeAgentPort, runNodeAgentServer)
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
          <> value defaultNodeAgentHost
          <> showDefault
          <> help "Host to bind to"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value defaultNodeAgentPort
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Corvus per-host node agent"
            <> header "corvus-nodeagent — local-machine side of the Corvus VM daemon"
        )

  -- SIGTERM/SIGINT signal the shutdown MVar; the main thread
  -- blocks on it until a signal arrives, then unwinds.
  stopMV <- newEmptyMVar
  let raiseStop = putMVar stopMV ()
  void $ installHandler sigTERM (Catch raiseStop) Nothing
  void $ installHandler sigINT (Catch raiseStop) Nothing

  runFilteredLogging (optLogLevel opts) $
    logInfoN $
      "corvus-nodeagent starting on "
        <> T.pack (optHost opts)
        <> ":"
        <> T.pack (show (optPort opts))

  -- Run the server async. The server itself performs startup
  -- cleanup (in Corvus.Node.Server.runNodeAgentServer) before
  -- it binds the listener.
  serverThread <-
    async $ runNodeAgentServer (optHost opts) (optPort opts)

  -- Block until SIGTERM/SIGINT.
  takeMVar stopMV
  runFilteredLogging (optLogLevel opts) $
    logInfoN "corvus-nodeagent shutting down; running cleanup"
  cancel serverThread
  cleanupCorvusProcesses
  runFilteredLogging (optLogLevel opts) $
    logInfoN "corvus-nodeagent cleanup complete"
  exitSuccess
