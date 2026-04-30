{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LIFO stack of destructors for ephemeral build resources.
--
-- Each step in a build pipeline that allocates a resource (a VM, a disk,
-- a temporary scratch directory, an attached drive) pushes a destructor
-- onto this stack as soon as the resource exists. On normal completion or
-- on exception, 'runCleanup' pops and runs every destructor in reverse
-- order — last-in, first-out, so an attach is detached before the disk
-- it pointed at gets deleted.
--
-- When an artifact graduates from "ephemeral" to "user-visible" (e.g. the
-- target disk is published as a registered Corvus disk), its destructor
-- is removed via 'detachByTag' so even with @cleanup: always@ the result
-- of a successful build survives.
module Corvus.Handlers.Build.Cleanup
  ( -- * Stack
    CleanupStack
  , newCleanupStack
  , push
  , detachByTag
  , runCleanup

    -- * Driving cleanup based on user mode
  , withCleanup
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, askLoggerIO, logWarnN, runLoggingT)
import Corvus.Schema.Build (CleanupMode (..))
import Data.Text (Text)
import qualified Data.Text as T

-- | A single destructor entry: a tag (so the artifact step can remove it)
-- plus the IO action that releases the resource.
data Entry = Entry
  { entryTag :: !Text
  , entryAction :: !(IO ())
  }

-- | A stack of cleanup actions. The most-recently pushed action is the
-- first to run. Wrapped in an 'MVar' so steps in the build pipeline can
-- update it without threading it through every call.
newtype CleanupStack = CleanupStack (MVar [Entry])

newCleanupStack :: IO CleanupStack
newCleanupStack = CleanupStack <$> newMVar []

-- | Register a destructor. The @tag@ is a free-form label used by
-- 'detachByTag' to remove the entry once the resource has graduated to
-- being a user-visible artifact (e.g. @"target-disk"@).
push :: CleanupStack -> Text -> IO () -> IO ()
push (CleanupStack var) tag action =
  modifyMVar_ var $ \xs -> pure (Entry tag action : xs)

-- | Remove (without running) every entry whose tag matches. Used when an
-- artifact is published and should not be torn down by the cleanup pass.
detachByTag :: CleanupStack -> Text -> IO ()
detachByTag (CleanupStack var) tag =
  modifyMVar_ var $ \xs -> pure (filter (\e -> entryTag e /= tag) xs)

-- | Run every registered destructor. Failures inside individual
-- destructors are logged at @warn@ but do not stop the rest of the cleanup.
runCleanup :: CleanupStack -> LoggingT IO ()
runCleanup (CleanupStack var) = do
  entries <- liftIO $ readMVar var
  -- Drain the stack so a second 'runCleanup' is a no-op.
  liftIO $ modifyMVar_ var (\_ -> pure [])
  mapM_ runOne entries
  where
    runOne e = do
      r <- liftIO $ try (entryAction e)
      case r of
        Right () -> pure ()
        Left (ex :: SomeException) ->
          logWarnN $ "build cleanup: '" <> entryTag e <> "' failed: " <> T.pack (show ex)

-- | Run a build body and run cleanup based on whether it succeeded.
-- The body returns @'Either' err a@ where @'Left'@ means the build failed
-- (a value-level failure, not an exception). Synchronous exceptions are
-- also caught and treated as failures.
--
-- Cleanup runs:
--   * on success — if mode is 'CleanupAlways' or 'CleanupOnSuccess';
--   * on failure — if mode is 'CleanupAlways' (so failed builds with
--     @onSuccess@ leave ephemeral resources around for inspection);
--   * never — if mode is 'CleanupNever'.
--
-- Failures inside destructors don't stop the cleanup pass; they're logged.
withCleanup
  :: CleanupMode
  -> CleanupStack
  -> LoggingT IO (Either err a)
  -> LoggingT IO (Either SomeException (Either err a))
withCleanup mode stack action = do
  logger <- askLoggerIO
  result <- liftIO $ try (runLoggingT action logger)
  let succeeded = case result of
        Right (Right _) -> True
        _ -> False
  let shouldCleanup = case mode of
        CleanupAlways -> True
        CleanupOnSuccess -> succeeded
        CleanupNever -> False
  when shouldCleanup $ runCleanup stack
  pure result
