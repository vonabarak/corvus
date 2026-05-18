{-# LANGUAGE OverloadedStrings #-}

-- | Entry point for `corvus-netd`, the privileged network agent.
--
-- Phase 1: listens on TCP 127.0.0.1:9877, exports a NetAgent
-- bootstrap cap. `ping`, `version`, and `session(owner)` work
-- end-to-end; the privileged methods on Session are stubs that
-- return @methodUnimplemented@ until Phase 2.
module Main where

import Control.Monad.Logger (LogLevel (..), logInfoN)
import Corvus.Netd.Server (defaultNetdHost, defaultNetdPort, runNetdServer)
import Corvus.Types (runFilteredLogging)
import qualified Data.Text as T
import Options.Applicative
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

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
  runNetdServer (optHost opts) (optPort opts)
