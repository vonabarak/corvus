{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Core test DSL types and primitives.
-- Provides the TestM monad and basic DSL combinators.
module Test.DSL.Core
  ( -- * Test monad
    TestM
  , DB.TestEnv (..)
  , runTestM

    -- * DSL primitives
  , given
  , when_
  , then_

    -- * Environment access
  , getDbPool
  , runDb
  , getLastResponse
  , setLastResponse
  , getTempDir

    -- * HSpec integration
  , testCase
  , withFreshDb
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Corvus.Protocol (Response)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool)
import qualified Test.Database as DB
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------
-- Test Environment
--------------------------------------------------------------------------------

-- TestEnv is now unified in Test.Database

--------------------------------------------------------------------------------
-- Test Monad
--------------------------------------------------------------------------------

-- | Test monad with access to test environment
newtype TestM a = TestM {unTestM :: ReaderT DB.TestEnv IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader DB.TestEnv
    )

-- | Run a TestM action
runTestM :: DB.TestEnv -> TestM a -> IO a
runTestM env action = runReaderT (unTestM action) env

--------------------------------------------------------------------------------
-- Environment Access
--------------------------------------------------------------------------------

-- | Get the database pool
getDbPool :: TestM (Pool SqlBackend)
getDbPool = asks DB.tePool

-- | Run a database action
runDb :: SqlPersistT IO a -> TestM a
runDb action = do
  pool <- getDbPool
  liftIO $ runSqlPool action pool

-- | Get the last response (if any)
getLastResponse :: TestM (Maybe Response)
getLastResponse = do
  ref <- asks DB.teLastResponse
  liftIO $ readIORef ref

-- | Set the last response
setLastResponse :: Response -> TestM ()
setLastResponse resp = do
  ref <- asks DB.teLastResponse
  liftIO $ writeIORef ref (Just resp)

-- | Get the temporary directory path
getTempDir :: TestM FilePath
getTempDir = asks DB.teTempDir

--------------------------------------------------------------------------------
-- DSL Primitives
--------------------------------------------------------------------------------

-- | Setup phase - prepare test data
-- This is just a semantic marker for readability
given :: TestM a -> TestM a
given = id

-- | Action phase - execute a command
-- This is just a semantic marker for readability
when_ :: TestM a -> TestM a
when_ action = action

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
  -- Create fresh last response ref for this test case
  respRef <- newIORef Nothing
  let env = dbEnv {DB.teLastResponse = respRef}
  -- Reset all tables before each test for isolation, then
  -- re-seed the default test node so every test starts with the
  -- 'toSqlKey 1 :: NodeId' FK target satisfied (multi-node
  -- Phase 1 lib still uses that placeholder pervasively).
  DB.resetTestDb env
  runTestM env action

-- | Run a test with a fresh database (alias for clarity)
withFreshDb :: TestM () -> SpecWith DB.TestEnv -> SpecWith DB.TestEnv
withFreshDb setup specs = specs
