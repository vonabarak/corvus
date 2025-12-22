{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Corvus.Model (migrateAll)
import Corvus.Server (runServer)
import Corvus.Types
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import Database.Persist.Postgresql (createPostgresqlPool, runMigration, runSqlPool)
import Options.Applicative
import System.Exit (exitSuccess)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | Command line options for the daemon
data Options = Options
  { optHost :: String,
    optPort :: Int,
    optDbUri :: String
  }
  deriving (Show)

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> value "127.0.0.1"
          <> help "Host to bind to (default: 127.0.0.1)"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 9876
          <> help "Port to listen on (default: 9876)"
      )
    <*> strOption
      ( long "database"
          <> short 'd'
          <> metavar "URI"
          <> help "PostgreSQL connection URI (required). Example: postgresql://user:pass@localhost/corvus"
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
main = runStdoutLoggingT $ do
  opts <- liftIO $ execParser optsInfo

  logInfoN $ "Connecting to database: " <> T.pack (optDbUri opts)

  -- Create database connection pool and run migrations
  pool <- createPostgresqlPool (pack $ optDbUri opts) 10

  logInfoN "Running database migrations..."
  liftIO $ runSqlPool (runMigration migrateAll) pool
  logInfoN "Migrations complete."

  -- Initialize server state with database pool
  state <- liftIO $ newServerState pool

  -- Install signal handlers for graceful shutdown
  let shutdownHandler = atomically $ writeTVar (ssShutdownFlag state) True

  liftIO $ installHandler sigTERM (Catch shutdownHandler) Nothing
  liftIO $ installHandler sigINT (Catch shutdownHandler) Nothing

  logInfoN $ "Starting corvus daemon on " <> T.pack (optHost opts) <> ":" <> T.pack (show (optPort opts))

  -- Start the server in a separate thread
  serverThread <- liftIO $ async $ runServer state (optHost opts) (optPort opts)

  -- Wait for shutdown signal
  liftIO $ waitForShutdown state

  logInfoN "Shutting down..."
  liftIO $ cancel serverThread
  liftIO exitSuccess

-- | Block until shutdown flag is set
waitForShutdown :: ServerState -> IO ()
waitForShutdown state = do
  shouldShutdown <- readTVarIO (ssShutdownFlag state)
  unless shouldShutdown $ do
    threadDelay 100000 -- 100ms
    waitForShutdown state
