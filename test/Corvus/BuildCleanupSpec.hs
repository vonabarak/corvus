{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the build cleanup stack: destructors run in
-- reverse-LIFO order, 'detachByTag' removes entries without running
-- them, and the cleanup-mode policies fire as documented.
module Corvus.BuildCleanupSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad.Logger (LoggingT, runLoggingT)
import Corvus.Handlers.Build.Cleanup
import Corvus.Schema.Build (CleanupMode (..))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Test.Hspec

-- | Run a 'LoggingT' action with a no-op logger so tests stay quiet.
runQuiet :: LoggingT IO a -> IO a
runQuiet m = runLoggingT m (\_ _ _ _ -> pure ())

spec :: Spec
spec = describe "Build.Cleanup" $ do
  describe "runCleanup" $ do
    it "runs destructors in reverse-push (LIFO) order" $ do
      ref <- newIORef ([] :: [Int])
      stack <- newCleanupStack
      push stack "a" (modifyIORef ref (1 :))
      push stack "b" (modifyIORef ref (2 :))
      push stack "c" (modifyIORef ref (3 :))
      runQuiet (runCleanup stack)
      result <- readIORef ref
      -- Push order [a,b,c] → top is c, runs first → ref = 1:2:3:[]
      result `shouldBe` [1, 2, 3]

    it "drains the stack so a second runCleanup is a no-op" $ do
      ref <- newIORef ([] :: [Int])
      stack <- newCleanupStack
      push stack "a" (modifyIORef ref (1 :))
      runQuiet (runCleanup stack)
      runQuiet (runCleanup stack)
      result <- readIORef ref
      length result `shouldBe` 1

    it "continues after a failing destructor (best-effort cleanup)" $ do
      ref <- newIORef ([] :: [Int])
      stack <- newCleanupStack
      push stack "first" (modifyIORef ref (1 :))
      push stack "boom" (throwIO (ErrorCall "destructor failed"))
      push stack "last" (modifyIORef ref (2 :))
      runQuiet (runCleanup stack)
      result <- readIORef ref
      -- last runs first (top), then 'boom' fails (logged), then first runs
      result `shouldBe` [1, 2]

  describe "detachByTag" $ do
    it "removes entries without running them" $ do
      ref <- newIORef ([] :: [Int])
      stack <- newCleanupStack
      push stack "keep" (modifyIORef ref (1 :))
      push stack "drop" (modifyIORef ref (99 :))
      detachByTag stack "drop"
      runQuiet (runCleanup stack)
      result <- readIORef ref
      result `shouldBe` [1]

  describe "withCleanup" $ do
    it "runs cleanup on success when mode=always" $ do
      ref <- newIORef (0 :: Int)
      stack <- newCleanupStack
      push stack "one" (modifyIORef ref succ)
      _ <-
        runQuiet $
          withCleanup
            CleanupAlways
            stack
            (pure (Right ()) :: LoggingT IO (Either String ()))
      readIORef ref `shouldReturn` 1

    it "runs cleanup on failure when mode=always" $ do
      ref <- newIORef (0 :: Int)
      stack <- newCleanupStack
      push stack "one" (modifyIORef ref succ)
      _ <-
        runQuiet $
          withCleanup
            CleanupAlways
            stack
            (pure (Left "fail") :: LoggingT IO (Either String ()))
      readIORef ref `shouldReturn` 1

    it "skips cleanup on failure when mode=onSuccess" $ do
      ref <- newIORef (0 :: Int)
      stack <- newCleanupStack
      push stack "one" (modifyIORef ref succ)
      _ <-
        runQuiet $
          withCleanup
            CleanupOnSuccess
            stack
            (pure (Left "fail") :: LoggingT IO (Either String ()))
      readIORef ref `shouldReturn` 0

    it "runs cleanup on success when mode=onSuccess" $ do
      ref <- newIORef (0 :: Int)
      stack <- newCleanupStack
      push stack "one" (modifyIORef ref succ)
      _ <-
        runQuiet $
          withCleanup
            CleanupOnSuccess
            stack
            (pure (Right ()) :: LoggingT IO (Either String ()))
      readIORef ref `shouldReturn` 1

    it "skips cleanup unconditionally when mode=never" $ do
      ref <- newIORef (0 :: Int)
      stack <- newCleanupStack
      push stack "one" (modifyIORef ref succ)
      _ <-
        runQuiet $
          withCleanup
            CleanupNever
            stack
            (pure (Right ()) :: LoggingT IO (Either String ()))
      readIORef ref `shouldReturn` 0
