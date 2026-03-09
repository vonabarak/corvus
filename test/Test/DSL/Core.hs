{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core test DSL types and primitives.
-- Provides the TestM monad and basic DSL combinators.
module Test.DSL.Core
  ( -- * Test monad
    TestM,
    TestEnv (..),
    runTestM,

    -- * DSL primitives
    given,
    when_,
    then_,

    -- * Environment access
    getDbPool,
    runDb,
    getLastResponse,
    setLastResponse,
    getTempDir,

    -- * HSpec integration
    testCase,
    withFreshDb,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Corvus.Protocol (Response)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Database.Persist.Sql (SqlPersistT, rawExecute)
import qualified Test.Database as DB
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------
-- Test Environment
--------------------------------------------------------------------------------

-- | Test execution environment
data TestEnv = TestEnv
  { -- | Database connection pool
    envDbPool :: !(Pool SqlBackend),
    -- | Last response from handler/RPC
    envLastResponse :: !(IORef (Maybe Response)),
    -- | Temporary directory for test files
    envTempDir :: !FilePath,
    -- | Test database name
    envDbName :: !String
  }

--------------------------------------------------------------------------------
-- Test Monad
--------------------------------------------------------------------------------

-- | Test monad with access to test environment
newtype TestM a = TestM {unTestM :: ReaderT TestEnv IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader TestEnv
    )

-- | Run a TestM action
runTestM :: TestEnv -> TestM a -> IO a
runTestM env action = runReaderT (unTestM action) env

--------------------------------------------------------------------------------
-- Environment Access
--------------------------------------------------------------------------------

-- | Get the database pool
getDbPool :: TestM (Pool SqlBackend)
getDbPool = asks envDbPool

-- | Run a database action
runDb :: SqlPersistT IO a -> TestM a
runDb action = do
  pool <- getDbPool
  liftIO $ runSqlPool action pool

-- | Get the last response (if any)
getLastResponse :: TestM (Maybe Response)
getLastResponse = do
  ref <- asks envLastResponse
  liftIO $ readIORef ref

-- | Set the last response
setLastResponse :: Response -> TestM ()
setLastResponse resp = do
  ref <- asks envLastResponse
  liftIO $ writeIORef ref (Just resp)

-- | Get the temporary directory path
getTempDir :: TestM FilePath
getTempDir = asks envTempDir

--------------------------------------------------------------------------------
-- DSL Primitives
--------------------------------------------------------------------------------

-- | Setup phase - prepare test data
-- This is just a semantic marker for readability
given :: TestM a -> TestM a
given = id

-- | Action phase - execute a command
-- Stores the response for later assertions
when_ :: TestM Response -> TestM Response
when_ action = do
  resp <- action
  setLastResponse resp
  pure resp

-- | Assertion phase - verify results
-- This is just a semantic marker for readability
then_ :: TestM () -> TestM ()
then_ = id

--------------------------------------------------------------------------------
-- HSpec Integration
--------------------------------------------------------------------------------

-- | Create a test case that runs in the TestM monad
testCase :: String -> TestM () -> SpecWith DB.TestEnv
testCase name action = it name $ \dbEnv -> do
  -- Create fresh test environment for this test case
  respRef <- newIORef Nothing
  let pool = DB.getPool dbEnv
  let env =
        TestEnv
          { envDbPool = pool,
            envLastResponse = respRef,
            envTempDir = "/tmp/corvus-test",
            envDbName = "test"
          }
  -- Truncate all tables before each test for isolation
  runSqlPool truncateAllTables pool
  runTestM env action

-- | Truncate all tables to ensure test isolation
truncateAllTables :: SqlPersistT IO ()
truncateAllTables =
  rawExecute
    "TRUNCATE network_interface, drive, snapshot, disk_image, shared_dir, vm RESTART IDENTITY CASCADE"
    []

-- | Run a test with a fresh database (alias for clarity)
withFreshDb :: TestM () -> SpecWith DB.TestEnv -> SpecWith DB.TestEnv
withFreshDb setup specs = specs
