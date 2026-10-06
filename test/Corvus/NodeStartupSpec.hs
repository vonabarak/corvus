{-# LANGUAGE OverloadedStrings #-}

module Corvus.NodeStartupSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Control.Monad (void)
import Corvus.Node.QemuStartup (waitForVsockOwnership)
import Corvus.Qemu.Config (defaultQemuConfig)
import qualified Data.Text.IO as T
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Hspec

spec :: Spec
spec = describe "QEMU startup failures" $ do
  it "includes stderr even when the reader finishes after process exit" $ do
    (_, _, Just stderrHandle, process) <-
      createProcess (proc "sh" ["-c", "echo 'Could not open vde: No such file or directory' >&2; exit 1"]) {std_err = CreatePipe}
    _ <- waitForProcess process
    tailVar <- newTVarIO ""
    doneVar <- newTVarIO False
    void $ forkIO $ do
      threadDelay 50000
      diagnostic <- T.hGetContents stderrHandle
      atomically $ writeTVar tailVar diagnostic
      hClose stderrHandle
      atomically $ writeTVar doneVar True
    result <- waitForVsockOwnership defaultQemuConfig 1 1004 process tailVar doneVar
    result `shouldBe` Left "QEMU exited with status 1: Could not open vde: No such file or directory"

  it "reports the exit status when stderr is empty" $ do
    (_, _, _, process) <- createProcess (proc "sh" ["-c", "exit 2"])
    _ <- waitForProcess process
    tailVar <- newTVarIO ""
    doneVar <- newTVarIO True
    result <- waitForVsockOwnership defaultQemuConfig 1 1004 process tailVar doneVar
    result `shouldBe` Left "QEMU exited with status 2"
