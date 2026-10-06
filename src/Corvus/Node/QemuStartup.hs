{-# LANGUAGE OverloadedStrings #-}

module Corvus.Node.QemuStartup (waitForVsockOwnership) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, check, readTVar, readTVarIO)
import qualified Corvus.Node.Qmp as NQ
import qualified Corvus.Node.VsockCid as VC
import Corvus.Qemu.Config (QemuConfig)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import System.Exit (ExitCode (..))
import System.Process (ProcessHandle, getProcessExitCode)
import qualified System.Timeout

-- | QEMU must answer QMP and retain the VSOCK CID before another start is
-- allowed to probe the host. The probe itself cannot reserve the CID because
-- closing its vhost fd releases it.
waitForVsockOwnership :: QemuConfig -> Int64 -> Word32 -> ProcessHandle -> TVar Text -> TVar Bool -> IO (Either Text ())
waitForVsockOwnership cfg vmId cid qemuPh stderrTail stderrDone = go (40 :: Int) Nothing
  where
    intervalUs = 250000
    go 0 mQmp =
      pure $ Left $ "timed out waiting for QMP and kernel ownership of vsock CID " <> T.pack (show cid) <> maybe "" (": " <>) mQmp
    go remaining mQmp = do
      exited <- getProcessExitCode qemuPh
      case exited of
        Just ExitSuccess -> exitedError "QEMU exited before startup completed"
        Just (ExitFailure n) -> exitedError ("QEMU exited with status " <> T.pack (show n))
        Nothing -> do
          qmp <- NQ.qmpQueryCommands cfg vmId
          free <- VC.isHostFree (fromIntegral cid)
          case (qmp, free) of
            (Right _, False) -> pure (Right ())
            (Left err, _) -> threadDelay intervalUs >> go (remaining - 1) (Just err)
            (_, True) -> threadDelay intervalUs >> go (remaining - 1) mQmp

    -- Process exit can precede the stderr reader reaching EOF. Bound the wait
    -- in case a descendant inherited the pipe and keeps it open.
    exitedError reason = do
      _ <- System.Timeout.timeout 1000000 $ atomically $ readTVar stderrDone >>= check
      diagnostic <- T.strip <$> readTVarIO stderrTail
      pure $ Left $ reason <> if T.null diagnostic then "" else ": " <> diagnostic
