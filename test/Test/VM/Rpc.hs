{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | RPC call wrappers for the Corvus daemon used by integration
-- tests.
--
-- Rewritten in Phase 5 to drive the daemon over Cap'n Proto via
-- 'Corvus.Client.Capnp.Rpc'. The legacy fine-grained result sums
-- ('VmActionResult', 'DiskResult', ...) are gone — every helper now
-- uses 'try @SomeException' around the cap call and surfaces failures
-- via 'fail'.
module Test.VM.Rpc
  ( -- * VM lifecycle
    createTestVm
  , createTestVmWithGuestAgent
  , createTestVmWithOptions
  , startTestVm
  , startTestVmSync
  , stopTestVm
  , stopTestVmAndWait
  , deleteTestVm

    -- * VM edit
  , editTestVm

    -- * VM details
  , getVmVsockCid

    -- * VM configuration
  , addVmDisk
  , addVmNetIf
  , removeVmNetIf
  , listVmNetIfs
  , addVmSharedDir

    -- * SSH key management
  , setupVmSshKey
  , createSshKey
  , attachSshKey
  , cleanupSshKey

    -- * Guest execution
  , runInVm
  , runInVm_
  , runViaGuestAgent
  , runViaGuestAgent_

    -- * Virtual network management
  , createNetwork
  , createNetworkWithSubnet
  , createNetworkWithNat
  , deleteNetwork
  , startNetwork
  , stopNetwork
  , showNetwork
  , addVmNetIfWithNetwork

    -- * Cloud-init config management
  , setCloudInitConfig
  , getCloudInitConfig
  , deleteCloudInitConfig

    -- * Task history queries
  , listTasks
  , showTask
  , listSubtasks
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Corvus.Client.Capnp.Connection as CC
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Model
import Corvus.Protocol (CloudInitInfo, NetIfInfo, NetworkInfo, TaskInfo, VmDetails (..))
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Runtime (getQmpSocket)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Common (EntityRef, entityRefFromText)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection)
import Test.VM.Ssh (SshKeyPair (..), generateSshKeyPair)
import Test.VM.Types (TestVm (..))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Run a Cap'n Proto call against the daemon, failing on either a
-- transport error or a server-side exception.
withDaemonRpc :: (CC.CapnpConnection -> IO a) -> TestDaemon -> IO a
withDaemonRpc action daemon = do
  r <- withDaemonConnection daemon action
  case r of
    Right a -> pure a
    Left e -> fail $ "Test.VM.Rpc.withDaemonRpc: " <> show e

vmRef :: Int64 -> EntityRef
vmRef = entityRefFromText . T.pack . show

--------------------------------------------------------------------------------
-- VM Lifecycle
--------------------------------------------------------------------------------

createTestVm :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> IO Int64
createTestVm daemon name cpus ram mDesc headless =
  createTestVmFull daemon name cpus ram mDesc headless False

createTestVmWithGuestAgent :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> IO Int64
createTestVmWithGuestAgent daemon name cpus ram mDesc headless =
  createTestVmFull daemon name cpus ram mDesc headless True

createTestVmFull :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> IO Int64
createTestVmFull daemon name cpus ram mDesc headless guestAgent =
  createTestVmWithOptions daemon name cpus ram mDesc headless guestAgent False

createTestVmWithOptions :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> IO Int64
createTestVmWithOptions daemon name cpus ram mDesc headless guestAgent cloudInit =
  withDaemonRpc (\conn -> CR.rpcVmCreate conn name cpus ram mDesc headless guestAgent cloudInit False) daemon

startTestVm :: TestDaemon -> Int64 -> IO ()
startTestVm daemon vmId =
  withDaemonRpc (\conn -> CR.rpcVmStart conn (vmRef vmId) False) daemon

startTestVmSync :: TestDaemon -> Int64 -> IO ()
startTestVmSync daemon vmId =
  withDaemonRpc (\conn -> CR.rpcVmStart conn (vmRef vmId) True) daemon

stopTestVm :: TestDaemon -> Int64 -> IO ()
stopTestVm daemon vmId =
  withDaemonRpc (\conn -> CR.rpcVmStop conn (vmRef vmId) False) daemon

stopTestVmAndWait :: TestDaemon -> Int64 -> Int -> IO ()
stopTestVmAndWait daemon vmId timeoutSec = do
  stopTestVm daemon vmId
  let go 0 = do
        _ <- try (withDaemonRpc (\conn -> CR.rpcVmReset conn (vmRef vmId)) daemon) :: IO (Either SomeException ())
        pure ()
      go n = do
        res <- try (withDaemonRpc (\conn -> CR.rpcVmShow conn (vmRef vmId)) daemon) :: IO (Either SomeException VmDetails)
        case res of
          Right details
            | vdStatus details == VmStopped -> pure ()
            | otherwise -> threadDelay 1000000 >> go (n - 1)
          _ -> threadDelay 1000000 >> go (n - 1)
  go timeoutSec
  waitForQemuExit (ssQemuConfig (tdState daemon)) vmId

deleteTestVm :: TestDaemon -> Int64 -> IO ()
deleteTestVm daemon vmId = do
  r <- try (withDaemonRpc (\conn -> CR.rpcVmDelete conn (vmRef vmId) False) daemon) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()

getVmVsockCid :: TestDaemon -> Int64 -> IO Int
getVmVsockCid daemon vmId = do
  details <- withDaemonRpc (\conn -> CR.rpcVmShow conn (vmRef vmId)) daemon
  case vdVsockCid details of
    Just cid -> pure cid
    Nothing -> fail $ "VM " <> show vmId <> " has no vsock CID — daemon failed to allocate one"

editTestVm :: TestDaemon -> Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> IO ()
editTestVm daemon vmId mCpus mRam mDesc mHeadless =
  withDaemonRpc (\conn -> CR.rpcVmEdit conn (vmRef vmId) mCpus mRam mDesc mHeadless Nothing Nothing Nothing) daemon

--------------------------------------------------------------------------------
-- VM Configuration
--------------------------------------------------------------------------------

addVmDisk :: TestDaemon -> Int64 -> Int64 -> DriveInterface -> CacheType -> Bool -> Bool -> IO ()
addVmDisk daemon vmId diskImageId iface cache discard ro = do
  _ <-
    withDaemonRpc
      ( \conn ->
          CR.rpcDiskAttach
            conn
            (vmRef vmId)
            (vmRef diskImageId)
            iface
            Nothing
            ro
            discard
            cache
      )
      daemon
  pure ()

addVmNetIf :: TestDaemon -> Int64 -> NetInterfaceType -> Text -> Maybe Text -> IO ()
addVmNetIf daemon vmId ifaceType hostDevice mac = do
  _ <-
    withDaemonRpc
      (\conn -> CR.rpcNetIfAdd conn (vmRef vmId) ifaceType hostDevice mac Nothing)
      daemon
  pure ()

removeVmNetIf :: TestDaemon -> Int64 -> Int64 -> IO ()
removeVmNetIf daemon vmId netIfId =
  withDaemonRpc (\conn -> CR.rpcNetIfRemove conn (vmRef vmId) netIfId) daemon

listVmNetIfs :: TestDaemon -> Int64 -> IO [NetIfInfo]
listVmNetIfs daemon vmId =
  withDaemonRpc (\conn -> CR.rpcNetIfList conn (vmRef vmId)) daemon

addVmSharedDir :: TestDaemon -> Int64 -> Text -> Text -> SharedDirCache -> IO ()
addVmSharedDir daemon vmId path tag cache = do
  _ <-
    withDaemonRpc
      (\conn -> CR.rpcSharedDirAdd conn (vmRef vmId) path tag cache False)
      daemon
  pure ()

--------------------------------------------------------------------------------
-- SSH Key Management
--------------------------------------------------------------------------------

setupVmSshKey :: TestDaemon -> Int64 -> FilePath -> IO (Int64, FilePath, FilePath)
setupVmSshKey daemon vmId tmpDir = do
  keyPairResult <- generateSshKeyPair tmpDir
  (privateKey, publicKey) <- case keyPairResult of
    Left err -> fail $ "Failed to generate SSH key pair: " <> T.unpack err
    Right keyPair -> pure (skpPrivateKey keyPair, skpPublicKey keyPair)
  putStrLn $ "[test] SSH private key: " <> privateKey

  pubKeyContent <- T.pack <$> readFile publicKey

  keyUuid <- nextRandom
  let keyName = "test-key-" <> T.take 8 (toText keyUuid)
  keyId <- createSshKey daemon keyName pubKeyContent

  attachSshKey daemon vmId keyId

  pure (keyId, privateKey, publicKey)

createSshKey :: TestDaemon -> Text -> Text -> IO Int64
createSshKey daemon name publicKey =
  withDaemonRpc (\conn -> CR.rpcSshKeyCreate conn name publicKey) daemon

attachSshKey :: TestDaemon -> Int64 -> Int64 -> IO ()
attachSshKey daemon vmId keyId =
  withDaemonRpc (\conn -> CR.rpcSshKeyAttach conn (vmRef vmId) (vmRef keyId)) daemon

cleanupSshKey :: TestDaemon -> Int64 -> IO ()
cleanupSshKey daemon keyId = do
  r <- try (withDaemonRpc (\conn -> CR.rpcSshKeyDelete conn (vmRef keyId)) daemon) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()

--------------------------------------------------------------------------------
-- Virtual Network Management
--------------------------------------------------------------------------------

createNetwork :: TestDaemon -> Text -> IO Int64
createNetwork daemon name =
  withDaemonRpc (\conn -> CR.rpcNetworkCreate conn name "" False False False) daemon

createNetworkWithSubnet :: TestDaemon -> Text -> Text -> IO Int64
createNetworkWithSubnet daemon name subnet =
  withDaemonRpc (\conn -> CR.rpcNetworkCreate conn name subnet True False False) daemon

createNetworkWithNat :: TestDaemon -> Text -> Text -> IO Int64
createNetworkWithNat daemon name subnet =
  withDaemonRpc (\conn -> CR.rpcNetworkCreate conn name subnet True True False) daemon

deleteNetwork :: TestDaemon -> Int64 -> IO ()
deleteNetwork daemon nwId = do
  r <- try (withDaemonRpc (\conn -> CR.rpcNetworkDelete conn (vmRef nwId)) daemon) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()

startNetwork :: TestDaemon -> Int64 -> IO ()
startNetwork daemon nwId =
  withDaemonRpc (\conn -> CR.rpcNetworkStart conn (vmRef nwId)) daemon

stopNetwork :: TestDaemon -> Int64 -> IO ()
stopNetwork daemon nwId = do
  r <- try (withDaemonRpc (\conn -> CR.rpcNetworkStop conn (vmRef nwId) True) daemon) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()

showNetwork :: TestDaemon -> Int64 -> IO NetworkInfo
showNetwork daemon nwId =
  withDaemonRpc (\conn -> CR.rpcNetworkShow conn (vmRef nwId)) daemon

addVmNetIfWithNetwork :: TestDaemon -> Int64 -> Int64 -> IO ()
addVmNetIfWithNetwork daemon vmId nwId = do
  _ <-
    withDaemonRpc
      (\conn -> CR.rpcNetIfAdd conn (vmRef vmId) NetManaged "" Nothing (Just (vmRef nwId)))
      daemon
  pure ()

waitForQemuExit :: QemuConfig -> Int64 -> IO ()
waitForQemuExit config vmId = do
  qmpSock <- getQmpSocket config vmId
  go qmpSock (20 :: Int) -- up to 2s
  where
    go _ 0 = pure ()
    go sock n = do
      alive <- doesFileExist sock
      when alive $ do
        threadDelay 100000
        go sock (n - 1)

--------------------------------------------------------------------------------
-- Guest Execution
--------------------------------------------------------------------------------

runInVm :: TestVm -> Text -> IO (ExitCode, Text, Text)
runInVm vm = runViaGuestAgent (tvmDaemon vm) (tvmId vm)

runInVm_ :: TestVm -> Text -> IO ()
runInVm_ vm = runViaGuestAgent_ (tvmDaemon vm) (tvmId vm)

runViaGuestAgent :: TestDaemon -> Int64 -> Text -> IO (ExitCode, Text, Text)
runViaGuestAgent daemon vmId command = do
  (exitcode, stdout, stderr) <-
    withDaemonRpc (\conn -> CR.rpcGuestExec conn (vmRef vmId) command) daemon
  pure (if exitcode == 0 then ExitSuccess else ExitFailure exitcode, stdout, stderr)

runViaGuestAgent_ :: TestDaemon -> Int64 -> Text -> IO ()
runViaGuestAgent_ daemon vmId command = do
  (code, _, stderr) <- runViaGuestAgent daemon vmId command
  case code of
    ExitSuccess -> pure ()
    ExitFailure c ->
      fail $ "Guest command failed with exit code " <> show c <> ": " <> T.unpack stderr

--------------------------------------------------------------------------------
-- Cloud-Init Config Management
--------------------------------------------------------------------------------

setCloudInitConfig :: TestDaemon -> Int64 -> Maybe Text -> Maybe Text -> Bool -> IO ()
setCloudInitConfig daemon vmId mUserData mNetworkConfig injectKeys =
  withDaemonRpc
    (\conn -> CR.rpcCloudInitSet conn (vmRef vmId) mUserData mNetworkConfig injectKeys)
    daemon

getCloudInitConfig :: TestDaemon -> Int64 -> IO (Maybe CloudInitInfo)
getCloudInitConfig daemon vmId =
  withDaemonRpc (\conn -> CR.rpcCloudInitGet conn (vmRef vmId)) daemon

deleteCloudInitConfig :: TestDaemon -> Int64 -> IO ()
deleteCloudInitConfig daemon vmId =
  withDaemonRpc (\conn -> CR.rpcCloudInitDelete conn (vmRef vmId)) daemon

--------------------------------------------------------------------------------
-- Task History Queries
--------------------------------------------------------------------------------

-- | List tasks. Subsystem and result filters are currently dropped on
-- the wire — re-add them when the Cap'n Proto schema gains them again.
listTasks :: TestDaemon -> Int -> Maybe TaskSubsystem -> Maybe TaskResult -> Bool -> IO [TaskInfo]
listTasks daemon limit _mSub _mResult _includeSubtasks =
  withDaemonRpc (`CR.rpcTaskList` limit) daemon

showTask :: TestDaemon -> Int64 -> IO (Maybe TaskInfo)
showTask daemon taskId = do
  r <- try (withDaemonRpc (`CR.rpcTaskShow` taskId) daemon) :: IO (Either SomeException TaskInfo)
  case r of
    Right info -> pure (Just info)
    Left _ -> pure Nothing

listSubtasks :: TestDaemon -> Int64 -> IO [TaskInfo]
listSubtasks daemon parentId =
  withDaemonRpc (`CR.rpcTaskListChildren` parentId) daemon
