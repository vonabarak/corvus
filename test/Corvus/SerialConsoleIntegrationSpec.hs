{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the buffered serial console feature.
-- Verifies that the daemon-side ring buffer correctly preserves serial
-- output across client reconnections, that the RPC protocol upgrade
-- works end-to-end, and that error paths are handled.
--
-- Requirements:
--   - QEMU with KVM support
--   - genisoimage or mkisofs
--   - PostgreSQL for test database
--
-- Run with: stack test --test-arguments="--match SerialConsoleIntegration"
module Corvus.SerialConsoleIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Corvus.Client.Connection (Connection (..), sendRequest)
import Corvus.Protocol (Ref (..), Request (..), Response (..))
import Corvus.Qemu.Qmp (sendQmpCommand)
import Corvus.Types (ServerState (..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.Socket.ByteString (recv)
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common
  ( TestVm (..)
  , VmConfig (..)
  , defaultVmConfig
  , startTestVmAndWait
  , withTestVm
  , withTestVmOnDaemon
  )
import Test.VM.Console (connectSerialConsole, consoleExpect)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection, withTestDaemon)
import Test.VM.Rpc
  ( createTestVmWithOptions
  , deleteTestVm
  , startTestVm
  , stopTestVmAndWait
  )
import Test.VM.Ssh (runInTestVm, waitForTestVmSshWithKey)

spec :: Spec
spec = withTestDb $ do
  describe "Serial Console Integration" $ do
    it "preserves output across reconnections" $ \env -> do
      withTestVm env defaultVmConfig {vmcHeadless = True} $ \vm -> do
        let daemon = tvmDaemon vm
            state = tdState daemon
            qcfg = ssQemuConfig state

        -- First connection: verify login prompt is in the buffer
        bufMap1 <- readTVarIO (ssSerialBuffers state)
        connectSerialConsole qcfg (tvmId vm) (Just bufMap1) $ \console -> do
          _ <- consoleExpect console "login:" 60
          pure ()

        -- Second connection: login prompt should still be in the buffer
        -- (no need to send Enter — it's already in the replayed buffer)
        bufMap2 <- readTVarIO (ssSerialBuffers state)
        connectSerialConsole qcfg (tvmId vm) (Just bufMap2) $ \console -> do
          _ <- consoleExpect console "login:" 10
          pure ()

    it "captures output generated while disconnected" $ \env -> do
      withTestVm env defaultVmConfig {vmcHeadless = True} $ \vm -> do
        let daemon = tvmDaemon vm
            state = tdState daemon
            qcfg = ssQemuConfig state

        -- Write a marker to the serial port via SSH (no client connected).
        -- Use doas (Alpine) to write to the serial port as root.
        (code, _, _) <- runInTestVm vm "doas sh -c 'echo SERIAL-MARKER-12345 > /dev/ttyS0'"
        code `shouldBe` ExitSuccess

        -- Small delay for the marker to be read by the buffer thread
        threadDelay 500000

        -- Connect and verify the marker is in the buffer
        bufMap <- readTVarIO (ssSerialBuffers state)
        connectSerialConsole qcfg (tvmId vm) (Just bufMap) $ \console -> do
          _ <- consoleExpect console "SERIAL-MARKER-12345" 10
          pure ()

    it "RPC protocol upgrade works end-to-end" $ \env -> do
      withTestVm env defaultVmConfig {vmcHeadless = True} $ \vm -> do
        let daemon = tvmDaemon vm
            vmName = T.pack (show (tvmId vm))

        -- Send serial console request via RPC
        result <- withDaemonConnection daemon $ \conn -> do
          resp <- sendRequest conn (ReqSerialConsole (Ref vmName))
          case resp of
            Right RespSerialConsoleOk -> do
              -- Connection is now in raw mode — read buffered output
              let sock = connSocket conn
              -- Read available data (buffer replay)
              chunk <- recv sock 65536
              BS.null chunk `shouldBe` False
              -- The boot output should contain the login prompt
              let output = T.pack (map (toEnum . fromIntegral) (BS.unpack chunk))
              T.isInfixOf "login:" output `shouldBe` True
            Right other -> expectationFailure $ "Expected RespSerialConsoleOk, got: " ++ show other
            Left err -> expectationFailure $ "RPC error: " ++ show err
        case result of
          Left err -> expectationFailure $ "Connection error: " ++ show err
          Right _ -> pure ()

    it "rejects serial console for non-headless VM" $ \env -> do
      withTestDaemon env $ \daemon -> do
        -- Create a non-headless VM
        vmId <- createTestVmWithOptions daemon "non-headless-vm" 1 512 Nothing False False False
        startTestVm daemon vmId
        -- Small delay for VM to start
        threadDelay 3000000

        result <- withDaemonConnection daemon $ \conn -> do
          resp <- sendRequest conn (ReqSerialConsole (Ref (T.pack (show vmId))))
          case resp of
            Right (RespError err) ->
              T.isInfixOf "not headless" err `shouldBe` True
            Right other ->
              expectationFailure $ "Expected RespError, got: " ++ show other
            Left err ->
              expectationFailure $ "RPC error: " ++ show err
        case result of
          Left err -> expectationFailure $ "Connection error: " ++ show err
          Right _ -> pure ()

        -- Cleanup
        stopTestVmAndWait daemon vmId 30
        deleteTestVm daemon vmId

    it "rejects serial console for stopped VM" $ \env -> do
      withTestDaemon env $ \daemon -> do
        -- Create a headless VM but do NOT start it
        vmId <- createTestVmWithOptions daemon "stopped-vm" 1 512 Nothing True False False

        result <- withDaemonConnection daemon $ \conn -> do
          resp <- sendRequest conn (ReqSerialConsole (Ref (T.pack (show vmId))))
          case resp of
            Right (RespError err) ->
              T.isInfixOf "not running" err `shouldBe` True
            Right other ->
              expectationFailure $ "Expected RespError, got: " ++ show other
            Left err ->
              expectationFailure $ "RPC error: " ++ show err
        case result of
          Left err -> expectationFailure $ "Connection error: " ++ show err
          Right _ -> pure ()

        deleteTestVm daemon vmId

    it "serial console works after VM reboot" $ \env -> do
      withTestVm env defaultVmConfig {vmcHeadless = True} $ \vm -> do
        let daemon = tvmDaemon vm
            state = tdState daemon
            qcfg = ssQemuConfig state

        -- Verify serial console works before reboot
        bufMap1 <- readTVarIO (ssSerialBuffers state)
        connectSerialConsole qcfg (tvmId vm) (Just bufMap1) $ \console -> do
          _ <- consoleExpect console "login:" 60
          pure ()

        -- Create a marker file on tmpfs — will be gone after reboot
        (codeMk, _, _) <- runInTestVm vm "touch /tmp/reboot-test-marker"
        codeMk `shouldBe` ExitSuccess

        -- Reset the VM via QMP (deterministic, no SSH/sudo issues)
        _ <- sendQmpCommand qcfg (tvmId vm) "{\"execute\":\"system_reset\"}"

        -- Wait for the VM to reboot and SSH to come back
        threadDelay 5000000
        waitForTestVmSshWithKey
          (tvmSshHost vm)
          (tvmSshPort vm)
          (tvmSshPrivateKey vm)
          (tvmSshUser vm)
          120

        -- Verify the reboot actually happened: marker file should be gone
        (codeChk, _, _) <- runInTestVm vm "test -f /tmp/reboot-test-marker"
        codeChk `shouldBe` ExitFailure 1

        -- Reconnect to serial console — should see the second boot's login prompt
        bufMap2 <- readTVarIO (ssSerialBuffers state)
        connectSerialConsole qcfg (tvmId vm) (Just bufMap2) $ \console -> do
          _ <- consoleExpect console "login:" 10
          pure ()

    it "cleans up buffer after VM stop" $ \env -> do
      withTestDaemon env $ \daemon -> do
        let state = tdState daemon
        -- Use withTestVmOnDaemon so we control the lifecycle
        withTestVmOnDaemon daemon defaultVmConfig {vmcHeadless = True} $ \vm -> do
          -- Verify buffer exists while VM is running
          bufMap1 <- readTVarIO (ssSerialBuffers state)
          Map.member (tvmId vm) bufMap1 `shouldBe` True

          -- Stop the VM
          stopTestVmAndWait daemon (tvmId vm) 30

          -- Small delay for buffer thread to clean up
          threadDelay 2000000

          -- Verify buffer is removed
          bufMap2 <- readTVarIO (ssSerialBuffers state)
          Map.member (tvmId vm) bufMap2 `shouldBe` False
