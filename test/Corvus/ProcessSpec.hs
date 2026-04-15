{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for "Corvus.Process".
--
-- These exercise the lifecycle helpers against real @sleep@ / @/bin/sh@
-- subprocesses. They're fast (a few hundred ms each) and don't need
-- a database or a daemon — just POSIX signals and file I/O.
module Corvus.ProcessSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Corvus.Process
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Types (ProcessID)
import System.Process (getPid, proc, spawnProcess, terminateProcess, waitForProcess)
import qualified System.Process as P
import Test.Hspec

-- | Spawn a long-running subprocess and pass its PID to the test body.
-- Tries to terminate the child even if the test assertion throws.
withSleeper :: Int -> (ProcessID -> IO a) -> IO a
withSleeper seconds body =
  bracket
    (spawnProcess "sleep" [show seconds])
    (\ph -> terminateProcess ph >> waitForProcess ph)
    ( \ph -> do
        mPid <- getPid ph
        case mPid of
          Just pid -> body pid
          Nothing -> error "failed to get PID of sleep"
    )

runLogged :: NoLoggingT IO a -> IO a
runLogged = runNoLoggingT

spec :: Spec
spec = do
  describe "isProcessAlive" $ do
    it "returns True for a running process" $ do
      withSleeper 10 $ \pid ->
        isProcessAlive pid `shouldReturn` True

    it "returns False for a non-existent PID" $ do
      -- PID 0x7fffffff is reserved / never assigned
      isProcessAlive 2147483645 `shouldReturn` False

  describe "waitForExit" $ do
    it "returns True when the process exits within the timeout" $ do
      withSleeper 1 $ \pid -> do
        -- sleep will exit after 1s; give waitForExit 3s
        waitForExit pid 3 `shouldReturn` True

    it "returns False when the process outlives the timeout" $ do
      withSleeper 10 $ \pid ->
        -- sleep runs 10s; we only wait 1s
        waitForExit pid 1 `shouldReturn` False

  describe "waitForSocketFile" $ do
    it "returns True when the file appears before the timeout" $ do
      withSystemTempDirectory "corvus-proc-spec" $ \dir -> do
        let path = dir </> "sentinel"
        -- Create the file immediately; wait up to 2s for it
        writeFile path ""
        waitForSocketFile path 2000 `shouldReturn` True

    it "returns False when the file never appears" $ do
      withSystemTempDirectory "corvus-proc-spec" $ \dir -> do
        let path = dir </> "never-created"
        waitForSocketFile path 300 `shouldReturn` False

  describe "stopProcess" $ do
    it "returns NotRunning for an already-dead PID" $ do
      result <- runLogged $ stopProcess "ghost" 2147483645 Nothing 1 1
      result `shouldBe` NotRunning

    it "reports StoppedByTerm for a SIGTERM-responsive process" $ do
      -- `sleep` handles SIGTERM by exiting. No graceful hook needed.
      withSleeper 30 $ \pid -> do
        result <- runLogged $ stopProcess "sleeper" pid Nothing 0 3
        result `shouldBe` StoppedByTerm
        isProcessAlive pid `shouldReturn` False

    it "escalates to SIGKILL when SIGTERM is ignored" $ do
      -- Start a shell that traps SIGTERM and keeps sleeping.
      ph <- spawnProcess "sh" ["-c", "trap '' TERM; exec sleep 60"]
      mPid <- getPid ph
      case mPid of
        Nothing -> expectationFailure "failed to get PID of trap-shell"
        Just pid -> do
          -- Give the shell a moment to install its trap
          threadDelay 200000
          result <- runLogged $ stopProcess "stubborn" pid Nothing 0 1
          result `shouldBe` StoppedByKill
          isProcessAlive pid `shouldReturn` False
          _ <- waitForProcess ph
          pure ()

    it "reports StoppedGracefully when the graceful hook succeeds" $ do
      -- Graceful hook here just sends SIGTERM directly (simulating a QMP
      -- quit or equivalent). The process then exits before we escalate.
      withSleeper 30 $ \pid -> do
        let graceful = P.callCommand ("kill -TERM " ++ show pid)
        result <- runLogged $ stopProcess "graceful" pid (Just graceful) 3 1
        result `shouldBe` StoppedGracefully
        isProcessAlive pid `shouldReturn` False
