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
import Corvus.Types (ListenAddress (..), ServerState (..), getDefaultSocketPath, newServerState)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import Database.Persist.Postgresql (createPostgresqlPool, runMigration, runSqlPool)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | Command line options for the daemon
data Options = Options
  { optSocket :: Maybe FilePath,
    optTcp :: Bool,
    optHost :: String,
    optPort :: Int,
    optDbUri :: String
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

  -- Determine listen address
  listenAddr <- liftIO $ getListenAddr opts

  -- Ensure socket directory exists for Unix sockets
  case listenAddr of
    UnixAddress path -> liftIO $ createDirectoryIfMissing True (takeDirectory path)
    TcpAddress _ _ -> pure ()

  logInfoN $ "Starting corvus daemon on " <> formatListenAddr listenAddr

  -- Start the server in a separate thread
  serverThread <- liftIO $ async $ runServer state listenAddr

  -- Wait for shutdown signal
  liftIO $ waitForShutdown state

  logInfoN "Shutting down..."
  liftIO $ cancel serverThread
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

-- | Format listen address for logging
formatListenAddr :: ListenAddress -> T.Text
formatListenAddr (TcpAddress host port) = T.pack host <> ":" <> T.pack (show port)
formatListenAddr (UnixAddress path) = "unix:" <> T.pack path
