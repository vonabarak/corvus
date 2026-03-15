{-# LANGUAGE OverloadedStrings #-}

-- | Test database management.
-- Provides functions to create and destroy isolated test databases.
module Test.Database
  ( -- * Test database lifecycle
    withTestDb,
    setupTestDb,
    teardownTestDb,

    -- * Test environment
    TestEnv (..),

    -- * Database operations
    runDb,
    getPool,

    -- * Configuration
    TestDbConfig (..),
    getTestDbConfig,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Corvus.Model (migrateAll)
import Data.ByteString.Char8 (pack)
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist.Postgresql (SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import System.Process (callCommand)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll)
import Test.Settings

--------------------------------------------------------------------------------
-- Test Environment
--------------------------------------------------------------------------------

-- | Test environment containing database connection
data TestEnv = TestEnv
  { -- | Database connection pool
    tePool :: !(Pool SqlBackend),
    -- | Test database name (for cleanup)
    teDbName :: !Text,
    -- | Configuration used
    teConfig :: !TestDbConfig
  }

-- | Get the database pool from the test environment
getPool :: TestEnv -> Pool SqlBackend
getPool = tePool

-- | Run a database action in the test environment
runDb :: (MonadIO m) => TestEnv -> SqlPersistT IO a -> m a
runDb env action = liftIO $ runSqlPool action (tePool env)

--------------------------------------------------------------------------------
-- Test Database Lifecycle
--------------------------------------------------------------------------------

-- | HSpec wrapper that sets up and tears down a test database
withTestDb :: SpecWith TestEnv -> Spec
withTestDb = beforeAll setupTestDb . afterAll teardownTestDb

-- | Create a fresh test database with migrations applied
setupTestDb :: IO TestEnv
setupTestDb = do
  config <- getTestDbConfig
  dbName <- generateTestDbName

  -- Create the database
  runPsqlAdmin config $ "CREATE DATABASE " <> dbName

  -- Create connection pool and run migrations
  let connStr = pack $ T.unpack $ buildConnString config dbName
  pool <- runNoLoggingT $ createPostgresqlPool connStr 1
  runSqlPool (runMigration migrateAll) pool

  pure
    TestEnv
      { tePool = pool,
        teDbName = dbName,
        teConfig = config
      }

-- | Destroy the test database
teardownTestDb :: TestEnv -> IO ()
teardownTestDb env = do
  -- Close all connections first
  destroyAllResources (tePool env)

  -- Drop the database
  runPsqlAdmin (teConfig env) $ "DROP DATABASE IF EXISTS " <> teDbName env

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Generate a unique test database name
generateTestDbName :: IO Text
generateTestDbName = do
  uuid <- nextRandom
  let shortUuid = T.take 8 $ toText uuid
  pure $ "corvus_test_" <> T.replace "-" "" shortUuid

-- | Build a PostgreSQL connection string
buildConnString :: TestDbConfig -> Text -> Text
buildConnString config dbName =
  "host="
    <> tdcHost config
    <> " port="
    <> T.pack (show (tdcPort config))
    <> " user="
    <> tdcUser config
    <> " password="
    <> tdcPassword config
    <> " dbname="
    <> dbName

-- | Run a psql command against the admin database
runPsqlAdmin :: TestDbConfig -> Text -> IO ()
runPsqlAdmin config sql = do
  let cmd =
        "PGPASSWORD="
          <> T.unpack (tdcPassword config)
          <> " psql -h "
          <> T.unpack (tdcHost config)
          <> " -p "
          <> show (tdcPort config)
          <> " -U "
          <> T.unpack (tdcUser config)
          <> " -d "
          <> T.unpack (tdcAdminDb config)
          <> " -c \""
          <> T.unpack sql
          <> "\""
  callCommand cmd
