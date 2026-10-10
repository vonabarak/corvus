{-# LANGUAGE OverloadedStrings #-}

-- | Default settings for tests.
-- Contains database and logging configuration.
module Test.Settings
  ( -- * Database settings
    TestDbConfig (..)
  , getTestDbConfig

    -- * Logging settings
  , getTestLogLevel
  )
where

import Control.Monad.Logger (LogLevel (..))
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------
-- Database Configuration
--------------------------------------------------------------------------------

-- | Configuration for test database connections
data TestDbConfig = TestDbConfig
  { tdcHost :: !Text
  -- ^ PostgreSQL host
  , tdcPort :: !Int
  -- ^ PostgreSQL port
  , tdcUser :: !Text
  -- ^ PostgreSQL user
  , tdcPassword :: !Text
  -- ^ PostgreSQL password
  , tdcAdminDb :: !Text
  -- ^ Admin database (used to create/drop test databases)
  , tdcUsePostgresql :: !Bool
  -- ^ Whether to use PostgreSQL instead of the default SQLite test database
  }
  deriving stock (Show)

-- | Get test database configuration from environment variables
getTestDbConfig :: IO TestDbConfig
getTestDbConfig = do
  host <- maybe "localhost" T.pack <$> lookupEnv "TEST_DB_HOST"
  port <- maybe 5432 read <$> lookupEnv "TEST_DB_PORT"
  user <- maybe "corvus" T.pack <$> lookupEnv "TEST_DB_USER"
  password <- maybe "corvus" T.pack <$> lookupEnv "TEST_DB_PASSWORD"
  adminDb <- maybe "postgres" T.pack <$> lookupEnv "TEST_DB_ADMIN"
  explicitBackend <- lookupEnv "TEST_DB_BACKEND"
  explicitHost <- lookupEnv "TEST_DB_HOST"
  let usePostgresql =
        case fmap (map toLower) explicitBackend of
          Just "postgresql" -> True
          Just "postgres" -> True
          Just "sqlite" -> False
          _ -> isJust explicitHost
  pure
    TestDbConfig
      { tdcHost = host
      , tdcPort = port
      , tdcUser = user
      , tdcPassword = password
      , tdcAdminDb = adminDb
      , tdcUsePostgresql = usePostgresql
      }

--------------------------------------------------------------------------------
-- Logging Configuration
--------------------------------------------------------------------------------

-- | Get test log level from CORVUS_TEST_LOG_LEVEL env var (default: info)
getTestLogLevel :: IO LogLevel
getTestLogLevel = do
  mLevel <- lookupEnv "CORVUS_TEST_LOG_LEVEL"
  pure $ case map toLower <$> mLevel of
    Just "debug" -> LevelDebug
    Just "info" -> LevelInfo
    Just "warn" -> LevelWarn
    Just "error" -> LevelError
    _ -> LevelInfo
