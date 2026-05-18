{-# LANGUAGE OverloadedStrings #-}

-- | Entry point for `corvus-netd`, the privileged network agent.
--
-- See `Corvus.Netd.Server` for what the agent actually does.
-- This module is the CLI shell: parse flags, configure logging,
-- hand off.
module Main where

import Control.Monad.Logger (LogLevel (..), logInfoN)
import Corvus.Netd.Server
  ( defaultNetdHost
  , defaultNetdPort
  , defaultOrphanGrace
  , runNetdServer
  )
import Corvus.Types (runFilteredLogging)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Options.Applicative
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

data Options = Options
  { optHost :: String
  , optPort :: Int
  , optLogLevel :: LogLevel
  , optOrphanGrace :: NominalDiffTime
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
    <*> option
      (fromInteger <$> auto)
      ( long "orphan-grace"
          <> metavar "SECONDS"
          <> value defaultOrphanGrace
          <> showDefault
          <> help
            "Seconds to hold orphaned resources before sweeping. \
            \Lower for integration tests; default 60s gives a reconnecting \
            \daemon a chance to claim its prior resources."
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
  runFilteredLogging (optLogLevel opts) $
    logInfoN $
      "corvus-netd starting on "
        <> T.pack (optHost opts)
        <> ":"
        <> T.pack (show (optPort opts))
  runNetdServer (optHost opts) (optPort opts) (optOrphanGrace opts)
