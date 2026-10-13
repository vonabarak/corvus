{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.Node.Caps.Session.Vm.Reboot (handleVmGuestPing, handleVmGuestReboot) where

import qualified Capnp.Gen.Nodeagent as CGNA
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Node.Caps.Session.Utils (SessionCap, agentQemuConfig, scQgaConns, scVmLedger)
import qualified Corvus.Node.GuestAgent as NGA
import Corvus.Node.GuestAgent.Connection (invalidateConn)
import qualified Corvus.Node.Ledger as L
import Corvus.Node.Qmp.Reboot (withRebootObserver)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word32)
import System.Timeout (timeout)

handleVmGuestPing :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmGuestPing'results)
handleVmGuestPing sc vmId = do
  alive <- NGA.guestPing (scQgaConns sc) agentQemuConfig vmId
  pure CGNA.Session'vmGuestPing'results {CGNA.alive = alive}

-- | One deadline covers observer negotiation, dispatch, reset and readiness.
-- Never retry a possibly dispatched shutdown: doing so can reboot twice.
handleVmGuestReboot :: SessionCap -> Int64 -> Word32 -> IO (CGNA.Parsed CGNA.Session'vmGuestReboot'results)
handleVmGuestReboot sc vmId timeoutSec = do
  outcome <- try $ timeout (fromIntegral timeoutSec * 1000000) $ do
    original <- atomically $ L.lookupVm (scVmLedger sc) vmId
    case original of
      Nothing -> pure (Left "VM is not running")
      Just live -> withRebootObserver agentQemuConfig vmId $ \awaitReset -> do
        dispatched <- NGA.guestShutdown (scQgaConns sc) agentQemuConfig vmId "reboot"
        case dispatched of
          Left err -> pure (Left ("Reboot dispatch failed or ambiguous: " <> err))
          Right () -> do
            processRestart <- awaitReset
            when processRestart $ waitForNewProcess (L.vlsQemuPid live)
            invalidateConn (scQgaConns sc) vmId
            waitForPing
            pure (Right ())
  let (kind, message) = case outcome of
        Left (err :: SomeException) -> (CGNA.DiskOpKind'errorGeneric, T.pack (show err))
        Right Nothing -> (CGNA.DiskOpKind'errorGeneric, "Timed out waiting for confirmed reboot and guest-ping")
        Right (Just (Left err)) -> (CGNA.DiskOpKind'errorGeneric, err)
        Right (Just (Right ())) -> (CGNA.DiskOpKind'success, "")
  pure CGNA.Session'vmGuestReboot'results {CGNA.result = CGNA.DiskOpResult {CGNA.kind = kind, CGNA.message = message}}
  where
    waitForNewProcess oldPid = do
      current <- atomically $ L.lookupVm (scVmLedger sc) vmId
      case current of
        Just live | L.vlsQemuPid live /= oldPid -> pure ()
        _ -> threadDelay 200000 >> waitForNewProcess oldPid
    waitForPing = do
      alive <- NGA.guestPing (scQgaConns sc) agentQemuConfig vmId
      if alive then pure () else threadDelay 1000000 >> waitForPing
