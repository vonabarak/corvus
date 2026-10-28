{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Capture on the QEMU host; only PNG bytes leave the nodeagent.
module Corvus.Node.Caps.Session.Vm.Screenshot (handleVmScreenshot) where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as N
import qualified Capnp.Gen.Streams as S
import Capnp.Rpc (throwFailed)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (IOException, SomeException, catch, onException, try)
import Control.Monad (void)
import Corvus.Node.Caps.Session.Utils (SessionCap (..), agentQemuConfig, withVmOpLock)
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Qmp as Q
import qualified Corvus.Node.VmSpec as V
import Corvus.Qemu.Config (getEffectiveRuntimeDir)
import Corvus.Rpc.Streams (callSink)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.IO.Temp (withTempDirectory)
import System.Timeout (timeout)

handleVmScreenshot :: SessionCap -> Int64 -> C.Client S.ByteSink -> IO (C.Parsed N.Session'vmScreenshot'results)
handleVmScreenshot sc vmId sink = do
  base <- getEffectiveRuntimeDir agentQemuConfig
  let abort = void (timeout 5000000 (callSink #abort S.ByteSink'abort'params sink)) `catch` \(_ :: SomeException) -> pure ()
  withTempDirectory
    base
    "screenshot"
    ( \dir -> do
        let path = dir </> "screen.png"
        captured <- withVmOpLock sc vmId (capture sc vmId path)
        case captured of
          Left (status, message) -> do
            abort
            pure $ reply status message 0
          Right () -> do
            count <- withBinaryFile path ReadMode $ \handle -> do
              let loop total = do
                    chunk <- BS.hGet handle 32768
                    if BS.null chunk
                      then pure total
                      else do
                        bounded (callSink #write S.ByteSink'write'params {S.chunk = chunk} sink)
                        loop (total + fromIntegral (BS.length chunk))
              loop 0
            bounded (callSink #end S.ByteSink'end'params sink)
            pure $ reply N.ScreenshotStatus'success "" count
    )
    `onException` abort
  where
    bounded action = do
      result <- timeout 60000000 action
      maybe (throwFailed "screenshot sink timed out") pure result
    reply = N.Session'vmScreenshot'results

capture :: SessionCap -> Int64 -> FilePath -> IO (Either (N.ScreenshotStatus, Text) ())
capture sc vmId path = do
  live <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case live of
    Nothing -> pure $ Left (N.ScreenshotStatus'notRunning, "VM is not running")
    Just runtime -> do
      exited <- readTVarIO (L.vlsLastExitCode runtime)
      if isJust exited
        then pure $ Left (N.ScreenshotStatus'notRunning, "VM has exited")
        else
          if V.vsHeadless (L.vlsSpec runtime)
            then pure $ Left (N.ScreenshotStatus'headless, "VM has no graphical display")
            else do
              result <- timeout 30000000 (Q.qmpScreenshot agentQemuConfig vmId path)
              case result of
                Just Q.QmpSuccess -> do
                  signature <- try (withBinaryFile path ReadMode (`BS.hGet` 8))
                  pure $ case signature of
                    Right bytes | bytes == BS.pack [137, 80, 78, 71, 13, 10, 26, 10] -> Right ()
                    Right _ -> failed "QEMU returned an invalid PNG screenshot"
                    Left (err :: IOException) -> failed (T.pack (show err))
                Just (Q.QmpError err) -> pure $ failed ("PNG screenshot requires QEMU >= 7.1 with screenshot support: " <> err)
                Just (Q.QmpConnectionFailed err) -> pure $ failed err
                Nothing -> pure $ failed "QEMU screenshot timed out"
  where
    failed message = Left (N.ScreenshotStatus'failed, message)
