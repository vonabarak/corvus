{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A strict, scripted RPC peer for CLI tests. No production server or database.
module Test.ClientRpc (Fixture, withMock, withMockTcp, mockSocket, mockPort, expectCalls, assertCalls, requests, setFailure, setReply, mockClient, sampleVmInfo, sampleVmDetails, sampleTaskInfo, sampleNodeDetails, sampleNodeInfo, sampleDiskImageInfo, sampleTemplateDetails, sampleCloudInitInfo, sampleViewGrant, sampleNetIfInfo, sampleSharedDirInfo, sampleAudioDeviceInfo, sampleSnapshotInfo) where

import qualified Capnp as C
import qualified Capnp.Gen.Cloudinit as GCloudinit
import qualified Capnp.Gen.Common as GCommon
import qualified Capnp.Gen.Corvus as GCorvus
import qualified Capnp.Gen.Disk as GDisk
import qualified Capnp.Gen.Enums as GEnums
import qualified Capnp.Gen.Network as GNetwork
import qualified Capnp.Gen.Node as GNode
import qualified Capnp.Gen.Sshkey as GSshkey
import qualified Capnp.Gen.Streams as GStreams
import qualified Capnp.Gen.Task as GTask
import qualified Capnp.Gen.Template as GTemplate
import qualified Capnp.Gen.Vm as GVm
import qualified Capnp.Repr as R
import Capnp.Rpc (ConnConfig (..), fromClient, handleConn, socketTransport, toClient)
import Capnp.Rpc.Errors (throwFailed)
import Capnp.Rpc.Server (MethodHandlerTree (..), ServerOps (..), SomeServer, exportToServerOps, findMethod, handleParsed, methodHandlerTree)
import qualified Capnp.Rpc.Untyped as Untyped
import Control.Concurrent.Async (AsyncCancelled, async, cancel, withAsync)
import Control.Exception (SomeException, bracket, catch, finally, fromException)
import Control.Monad (forever, unless, (>=>))
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Vector as Vector
import GHC.Conc (getUncaughtExceptionHandler, setUncaughtExceptionHandler)
import Network.Socket (Family (AF_INET, AF_UNIX), SockAddr (SockAddrInet, SockAddrUnix), SocketType (Stream), accept, bind, close, getSocketName, listen, socket, tupleToHostAddress)
import Supervisors (withSupervisor)
import System.FilePath ((</>))
import System.IO (stderr)
import System.IO.Silently (hSilence)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (shouldBe)

data Fixture = Fixture
  { mockSocket :: FilePath
  , mockPort :: IORef Int
  , replies :: IORef (Map.Map String Dynamic)
  , pending :: IORef [String]
  , journal :: IORef [(String, Dynamic)]
  , unexpected :: IORef [String]
  , localClient :: IORef (Maybe Untyped.Client)
  , failure :: IORef (Maybe String)
  }

newtype Mock = Mock Fixture
instance SomeServer Mock

expectCalls :: Fixture -> [String] -> IO ()
expectCalls f names = do
  writeIORef (pending f) names
  writeIORef (journal f) []
  writeIORef (unexpected f) []

assertCalls :: Fixture -> IO ()
assertCalls f = do
  readIORef (pending f) >>= (`shouldBe` [])
  readIORef (unexpected f) >>= (`shouldBe` [])

requests :: forall p. (Typeable p) => Fixture -> String -> IO [p]
requests f name = do
  calls <- readIORef (journal f)
  mapM (maybe (fail ("Wrong request type: " ++ name)) pure . fromDynamic . snd) (filter ((== name) . fst) calls)

setFailure :: Fixture -> Maybe String -> IO ()
setFailure f = writeIORef (failure f)

setReply :: (Typeable p, Typeable r) => Fixture -> String -> (p -> IO r) -> IO ()
setReply f name action = atomicModifyIORef' (replies f) (\rs -> (Map.insert name (toDyn action) rs, ()))

respond :: forall p r. (Typeable p, Typeable r) => Fixture -> String -> p -> IO r
respond f name params = do
  atomicModifyIORef' (journal f) (\calls -> (calls ++ [(name, toDyn params)], ()))
  expected <- atomicModifyIORef' (pending f) (\case [] -> ([], Nothing); n : ns -> (ns, Just n))
  unless (expected == Just name) $ do
    let message = "Unexpected RPC " ++ name ++ "; expected " ++ show expected
    atomicModifyIORef' (unexpected f) (\errors -> (message : errors, ()))
    fail message
  failed <- readIORef (failure f)
  if failed == Just name
    then throwFailed "scripted RPC failure"
    else do
      rs <- readIORef (replies f)
      case Map.lookup name rs >>= fromDynamic of
        Nothing -> fail ("Missing typed reply: " ++ name)
        Just action -> action params

mockClient :: (R.IsCap a) => Fixture -> IO (C.Client a)
mockClient f = readIORef (localClient f) >>= maybe (fail "Mock client not initialized") (pure . fromClient)

withMock :: (Fixture -> IO a) -> IO a
withMock = withMockTransport False

withMockTcp :: (Fixture -> IO a) -> IO a
withMockTcp = withMockTransport True

withMockTransport :: Bool -> (Fixture -> IO a) -> IO a
withMockTransport tcp action = quietCancellation $ hSilence [stderr] $ withSystemTempDirectory "corvus-cli" $ \dir -> withSupervisor $ \sup -> do
  port <- newIORef 0
  rs <- newIORef Map.empty
  expected <- newIORef []
  calls <- newIORef []
  errors <- newIORef []
  failed <- newIORef Nothing
  client <- newIORef Nothing
  let path = dir </> "rpc.sock"
      f = Fixture path port rs expected calls errors client failed
  let peer = Mock f
      trees =
        [ methodHandlerTree (Proxy @GCorvus.Daemon) peer
        , methodHandlerTree (Proxy @GVm.VmManager) peer
        , methodHandlerTree (Proxy @GVm.Vm) peer
        , methodHandlerTree (Proxy @GDisk.DiskManager) peer
        , methodHandlerTree (Proxy @GDisk.DiskUpload) peer
        , methodHandlerTree (Proxy @GDisk.Disk) peer
        , methodHandlerTree (Proxy @GDisk.Snapshot) peer
        , methodHandlerTree (Proxy @GNetwork.NetworkManager) peer
        , methodHandlerTree (Proxy @GNetwork.Network) peer
        , methodHandlerTree (Proxy @GNode.NodeManager) peer
        , methodHandlerTree (Proxy @GNode.Node) peer
        , methodHandlerTree (Proxy @GSshkey.SshKeyManager) peer
        , methodHandlerTree (Proxy @GSshkey.SshKey) peer
        , methodHandlerTree (Proxy @GTemplate.TemplateManager) peer
        , methodHandlerTree (Proxy @GTemplate.Template) peer
        , methodHandlerTree (Proxy @GTask.TaskManager) peer
        , methodHandlerTree (Proxy @GTask.Task) peer
        , methodHandlerTree (Proxy @GCloudinit.CloudInitManager) peer
        , methodHandlerTree (Proxy @GStreams.ByteSink) peer
        , methodHandlerTree (Proxy @GStreams.Handle) peer
        ]
      handlers = Map.fromList [(mhtId tree, Vector.fromList (mhtHandlers tree)) | tree <- trees]
      baseOps = exportToServerOps (Proxy @GCorvus.Daemon) peer
      ops =
        baseOps
          { handleCall = \iface method -> case findMethod iface method handlers of
              Just handler -> \params result -> handler params result `catch` \(_ :: SomeException) -> pure ()
              Nothing -> handleCall baseOps iface method
          }
  raw <- Untyped.export sup ops
  writeIORef client (Just raw)
  let capDaemon = fromClient raw :: C.Client GCorvus.Daemon
      capVmManager = fromClient raw :: C.Client GVm.VmManager
      capVm = fromClient raw :: C.Client GVm.Vm
      capDiskManager = fromClient raw :: C.Client GDisk.DiskManager
      capDiskUpload = fromClient raw :: C.Client GDisk.DiskUpload
      capDisk = fromClient raw :: C.Client GDisk.Disk
      capSnapshot = fromClient raw :: C.Client GDisk.Snapshot
      capNetworkManager = fromClient raw :: C.Client GNetwork.NetworkManager
      capNetwork = fromClient raw :: C.Client GNetwork.Network
      capNodeManager = fromClient raw :: C.Client GNode.NodeManager
      capNode = fromClient raw :: C.Client GNode.Node
      capSshKeyManager = fromClient raw :: C.Client GSshkey.SshKeyManager
      capSshKey = fromClient raw :: C.Client GSshkey.SshKey
      capTemplateManager = fromClient raw :: C.Client GTemplate.TemplateManager
      capTemplate = fromClient raw :: C.Client GTemplate.Template
      capTaskManager = fromClient raw :: C.Client GTask.TaskManager
      capTask = fromClient raw :: C.Client GTask.Task
      capCloudInitManager = fromClient raw :: C.Client GCloudinit.CloudInitManager
      capByteSink = fromClient raw :: C.Client GStreams.ByteSink
      capHandle = fromClient raw :: C.Client GStreams.Handle
  setReply f "Daemon.ping" (\(_ :: C.Parsed GCorvus.Daemon'ping'params) -> pure sampleDaemon'ping'results)
  setReply f "Daemon.status" (\(_ :: C.Parsed GCorvus.Daemon'status'params) -> pure sampleDaemon'status'results)
  setReply f "Daemon.shutdown" (\(_ :: C.Parsed GCorvus.Daemon'shutdown'params) -> pure sampleDaemon'shutdown'results)
  setReply f "Daemon.vms" (\(_ :: C.Parsed GCorvus.Daemon'vms'params) -> pure (GCorvus.Daemon'vms'results {GCorvus.mgr = capVmManager}))
  setReply f "Daemon.disks" (\(_ :: C.Parsed GCorvus.Daemon'disks'params) -> pure (GCorvus.Daemon'disks'results {GCorvus.mgr = capDiskManager}))
  setReply f "Daemon.networks" (\(_ :: C.Parsed GCorvus.Daemon'networks'params) -> pure (GCorvus.Daemon'networks'results {GCorvus.mgr = capNetworkManager}))
  setReply f "Daemon.sshKeys" (\(_ :: C.Parsed GCorvus.Daemon'sshKeys'params) -> pure (GCorvus.Daemon'sshKeys'results {GCorvus.mgr = capSshKeyManager}))
  setReply f "Daemon.templates" (\(_ :: C.Parsed GCorvus.Daemon'templates'params) -> pure (GCorvus.Daemon'templates'results {GCorvus.mgr = capTemplateManager}))
  setReply f "Daemon.tasks" (\(_ :: C.Parsed GCorvus.Daemon'tasks'params) -> pure (GCorvus.Daemon'tasks'results {GCorvus.mgr = capTaskManager}))
  setReply f "Daemon.cloudInit" (\(_ :: C.Parsed GCorvus.Daemon'cloudInit'params) -> pure (GCorvus.Daemon'cloudInit'results {GCorvus.mgr = capCloudInitManager}))
  setReply f "Daemon.apply" (\(_ :: C.Parsed GCorvus.Daemon'apply'params) -> pure (GCorvus.Daemon'apply'results {GCorvus.taskId = 42, GCorvus.result = sampleApplyResult}))
  setReply f "Daemon.build" (\(_ :: C.Parsed GCorvus.Daemon'build'params) -> pure (GCorvus.Daemon'build'results {GCorvus.taskId = 42}))
  setReply f "Daemon.nodes" (\(_ :: C.Parsed GCorvus.Daemon'nodes'params) -> pure (GCorvus.Daemon'nodes'results {GCorvus.mgr = capNodeManager}))
  setReply f "VmManager.list" (\(_ :: C.Parsed GVm.VmManager'list'params) -> pure sampleVmManager'list'results)
  setReply f "VmManager.get" (\(_ :: C.Parsed GVm.VmManager'get'params) -> pure (GVm.VmManager'get'results {GVm.vm = capVm}))
  setReply f "VmManager.create" (\(_ :: C.Parsed GVm.VmManager'create'params) -> pure (GVm.VmManager'create'results {GVm.vm = capVm}))
  setReply f "Vm.show" (\(_ :: C.Parsed GVm.Vm'show'params) -> pure sampleVm'show'results)
  setReply f "Vm.start" (\(_ :: C.Parsed GVm.Vm'start'params) -> pure sampleVm'start'results)
  setReply f "Vm.stop" (\(_ :: C.Parsed GVm.Vm'stop'params) -> pure sampleVm'stop'results)
  setReply f "Vm.pause" (\(_ :: C.Parsed GVm.Vm'pause'params) -> pure sampleVm'pause'results)
  setReply f "Vm.reset" (\(_ :: C.Parsed GVm.Vm'reset'params) -> pure sampleVm'reset'results)
  setReply f "Vm.edit" (\(_ :: C.Parsed GVm.Vm'edit'params) -> pure sampleVm'edit'results)
  setReply f "Vm.delete" (\(_ :: C.Parsed GVm.Vm'delete'params) -> pure sampleVm'delete'results)
  setReply f "Vm.cloudInit" (\(_ :: C.Parsed GVm.Vm'cloudInit'params) -> pure sampleVm'cloudInit'results)
  setReply f "Vm.viewGrant" (\(_ :: C.Parsed GVm.Vm'viewGrant'params) -> pure sampleVm'viewGrant'results)
  setReply f "Vm.guestExec" (\(_ :: C.Parsed GVm.Vm'guestExec'params) -> pure sampleVm'guestExec'results)
  setReply f "Vm.sendCtrlAltDel" (\(_ :: C.Parsed GVm.Vm'sendCtrlAltDel'params) -> pure sampleVm'sendCtrlAltDel'results)
  setReply f "Vm.serialConsole" (\(_ :: C.Parsed GVm.Vm'serialConsole'params) -> pure (GVm.Vm'serialConsole'results {GVm.input = capByteSink}))
  setReply f "Vm.serialConsoleFlush" (\(_ :: C.Parsed GVm.Vm'serialConsoleFlush'params) -> pure sampleVm'serialConsoleFlush'results)
  setReply f "Vm.hmpMonitor" (\(_ :: C.Parsed GVm.Vm'hmpMonitor'params) -> pure (GVm.Vm'hmpMonitor'results {GVm.input = capByteSink}))
  setReply f "Vm.hmpMonitorFlush" (\(_ :: C.Parsed GVm.Vm'hmpMonitorFlush'params) -> pure sampleVm'hmpMonitorFlush'results)
  setReply f "Vm.subscribeGuestAgent" (\(_ :: C.Parsed GVm.Vm'subscribeGuestAgent'params) -> pure (GVm.Vm'subscribeGuestAgent'results {GVm.handle = capHandle}))
  setReply f "Vm.attachDisk" (\(_ :: C.Parsed GVm.Vm'attachDisk'params) -> pure sampleVm'attachDisk'results)
  setReply f "Vm.detachDisk" (\(_ :: C.Parsed GVm.Vm'detachDisk'params) -> pure sampleVm'detachDisk'results)
  setReply f "Vm.addNetIf" (\(_ :: C.Parsed GVm.Vm'addNetIf'params) -> pure sampleVm'addNetIf'results)
  setReply f "Vm.removeNetIf" (\(_ :: C.Parsed GVm.Vm'removeNetIf'params) -> pure sampleVm'removeNetIf'results)
  setReply f "Vm.listNetIfs" (\(_ :: C.Parsed GVm.Vm'listNetIfs'params) -> pure sampleVm'listNetIfs'results)
  setReply f "Vm.addSharedDir" (\(_ :: C.Parsed GVm.Vm'addSharedDir'params) -> pure sampleVm'addSharedDir'results)
  setReply f "Vm.removeSharedDir" (\(_ :: C.Parsed GVm.Vm'removeSharedDir'params) -> pure sampleVm'removeSharedDir'results)
  setReply f "Vm.listSharedDirs" (\(_ :: C.Parsed GVm.Vm'listSharedDirs'params) -> pure sampleVm'listSharedDirs'results)
  setReply f "Vm.snapshotCreate" (\(_ :: C.Parsed GVm.Vm'snapshotCreate'params) -> pure sampleVm'snapshotCreate'results)
  setReply f "Vm.snapshotList" (\(_ :: C.Parsed GVm.Vm'snapshotList'params) -> pure sampleVm'snapshotList'results)
  setReply f "Vm.snapshotRollback" (\(_ :: C.Parsed GVm.Vm'snapshotRollback'params) -> pure sampleVm'snapshotRollback'results)
  setReply f "Vm.attachSshKey" (\(_ :: C.Parsed GVm.Vm'attachSshKey'params) -> pure sampleVm'attachSshKey'results)
  setReply f "Vm.detachSshKey" (\(_ :: C.Parsed GVm.Vm'detachSshKey'params) -> pure sampleVm'detachSshKey'results)
  setReply f "Vm.listSshKeys" (\(_ :: C.Parsed GVm.Vm'listSshKeys'params) -> pure sampleVm'listSshKeys'results)
  setReply f "Vm.migrate" (\(_ :: C.Parsed GVm.Vm'migrate'params) -> pure sampleVm'migrate'results)
  setReply f "Vm.save" (\(_ :: C.Parsed GVm.Vm'save'params) -> pure sampleVm'save'results)
  setReply f "Vm.getStatsHistory" (\(_ :: C.Parsed GVm.Vm'getStatsHistory'params) -> pure sampleVm'getStatsHistory'results)
  setReply f "Vm.subscribeStats" (\(_ :: C.Parsed GVm.Vm'subscribeStats'params) -> pure (GVm.Vm'subscribeStats'results {GVm.handle = capHandle}))
  setReply f "Vm.snapshotDelete" (\(_ :: C.Parsed GVm.Vm'snapshotDelete'params) -> pure sampleVm'snapshotDelete'results)
  setReply f "Vm.addAudioDevice" (\(_ :: C.Parsed GVm.Vm'addAudioDevice'params) -> pure sampleVm'addAudioDevice'results)
  setReply f "Vm.editAudioDevice" (\(_ :: C.Parsed GVm.Vm'editAudioDevice'params) -> pure sampleVm'editAudioDevice'results)
  setReply f "Vm.removeAudioDevice" (\(_ :: C.Parsed GVm.Vm'removeAudioDevice'params) -> pure sampleVm'removeAudioDevice'results)
  setReply f "Vm.listAudioDevices" (\(_ :: C.Parsed GVm.Vm'listAudioDevices'params) -> pure sampleVm'listAudioDevices'results)
  setReply f "Vm.editNetIf" (\(_ :: C.Parsed GVm.Vm'editNetIf'params) -> pure sampleVm'editNetIf'results)
  setReply f "Vm.setBalloon" (\(_ :: C.Parsed GVm.Vm'setBalloon'params) -> pure sampleVm'setBalloon'results)
  setReply f "DiskManager.list" (\(_ :: C.Parsed GDisk.DiskManager'list'params) -> pure sampleDiskManager'list'results)
  setReply f "DiskManager.get" (\(_ :: C.Parsed GDisk.DiskManager'get'params) -> pure (GDisk.DiskManager'get'results {GDisk.disk = capDisk}))
  setReply f "DiskManager.create" (\(_ :: C.Parsed GDisk.DiskManager'create'params) -> pure (GDisk.DiskManager'create'results {GDisk.disk = capDisk}))
  setReply f "DiskManager.register" (\(_ :: C.Parsed GDisk.DiskManager'register'params) -> pure (GDisk.DiskManager'register'results {GDisk.disk = capDisk}))
  setReply f "DiskManager.createOverlay" (\(_ :: C.Parsed GDisk.DiskManager'createOverlay'params) -> pure (GDisk.DiskManager'createOverlay'results {GDisk.disk = capDisk}))
  setReply f "DiskManager.clone" (\(_ :: C.Parsed GDisk.DiskManager'clone'params) -> pure (GDisk.DiskManager'clone'results {GDisk.disk = capDisk}))
  setReply f "DiskManager.rebase" (\(_ :: C.Parsed GDisk.DiskManager'rebase'params) -> pure sampleDiskManager'rebase'results)
  setReply f "DiskManager.import" (\(_ :: C.Parsed GDisk.DiskManager'import'params) -> pure sampleDiskManager'import'results)
  setReply f "DiskManager.flatten" (\(_ :: C.Parsed GDisk.DiskManager'flatten'params) -> pure sampleDiskManager'flatten'results)
  setReply f "DiskManager.copy" (\(_ :: C.Parsed GDisk.DiskManager'copy'params) -> pure sampleDiskManager'copy'results)
  setReply f "DiskManager.move" (\(_ :: C.Parsed GDisk.DiskManager'move'params) -> pure sampleDiskManager'move'results)
  setReply f "DiskManager.beginUpload" (\(_ :: C.Parsed GDisk.DiskManager'beginUpload'params) -> pure (GDisk.DiskManager'beginUpload'results {GDisk.result = GDisk.DiskUploadResult (GDisk.DiskUploadResult'upload capDiskUpload)}))
  setReply f "DiskManager.mediaEject" (\(_ :: C.Parsed GDisk.DiskManager'mediaEject'params) -> pure sampleDiskManager'mediaEject'results)
  setReply f "DiskManager.mediaChange" (\(_ :: C.Parsed GDisk.DiskManager'mediaChange'params) -> pure sampleDiskManager'mediaChange'results)
  setReply f "DiskManager.cleanup" (\(_ :: C.Parsed GDisk.DiskManager'cleanup'params) -> pure sampleDiskManager'cleanup'results)
  setReply f "DiskUpload.write" (\(_ :: C.Parsed GDisk.DiskUpload'write'params) -> pure sampleDiskUpload'write'results)
  setReply f "DiskUpload.finish" (\(_ :: C.Parsed GDisk.DiskUpload'finish'params) -> pure (GDisk.DiskUpload'finish'results {GDisk.disk = capDisk}))
  setReply f "DiskUpload.abort" (\(_ :: C.Parsed GDisk.DiskUpload'abort'params) -> pure sampleDiskUpload'abort'results)
  setReply f "Disk.show" (\(_ :: C.Parsed GDisk.Disk'show'params) -> pure sampleDisk'show'results)
  setReply f "Disk.delete" (\(_ :: C.Parsed GDisk.Disk'delete'params) -> pure sampleDisk'delete'results)
  setReply f "Disk.refresh" (\(_ :: C.Parsed GDisk.Disk'refresh'params) -> pure sampleDisk'refresh'results)
  setReply f "Disk.resize" (\(_ :: C.Parsed GDisk.Disk'resize'params) -> pure sampleDisk'resize'results)
  setReply f "Disk.snapshotCreate" (\(_ :: C.Parsed GDisk.Disk'snapshotCreate'params) -> pure (GDisk.Disk'snapshotCreate'results {GDisk.snapshot = capSnapshot, GDisk.snapshotId = 42}))
  setReply f "Disk.snapshotList" (\(_ :: C.Parsed GDisk.Disk'snapshotList'params) -> pure sampleDisk'snapshotList'results)
  setReply f "Disk.snapshotGet" (\(_ :: C.Parsed GDisk.Disk'snapshotGet'params) -> pure (GDisk.Disk'snapshotGet'results {GDisk.snapshot = capSnapshot}))
  setReply f "Disk.tag" (\(_ :: C.Parsed GDisk.Disk'tag'params) -> pure sampleDisk'tag'results)
  setReply f "Disk.untag" (\(_ :: C.Parsed GDisk.Disk'untag'params) -> pure sampleDisk'untag'results)
  setReply f "Disk.registerPlacement" (\(_ :: C.Parsed GDisk.Disk'registerPlacement'params) -> pure sampleDisk'registerPlacement'results)
  setReply f "Snapshot.show" (\(_ :: C.Parsed GDisk.Snapshot'show'params) -> pure sampleSnapshot'show'results)
  setReply f "Snapshot.delete" (\(_ :: C.Parsed GDisk.Snapshot'delete'params) -> pure sampleSnapshot'delete'results)
  setReply f "Snapshot.rollback" (\(_ :: C.Parsed GDisk.Snapshot'rollback'params) -> pure sampleSnapshot'rollback'results)
  setReply f "Snapshot.merge" (\(_ :: C.Parsed GDisk.Snapshot'merge'params) -> pure sampleSnapshot'merge'results)
  setReply f "NetworkManager.list" (\(_ :: C.Parsed GNetwork.NetworkManager'list'params) -> pure sampleNetworkManager'list'results)
  setReply f "NetworkManager.get" (\(_ :: C.Parsed GNetwork.NetworkManager'get'params) -> pure (GNetwork.NetworkManager'get'results {GNetwork.network = capNetwork}))
  setReply f "NetworkManager.create" (\(_ :: C.Parsed GNetwork.NetworkManager'create'params) -> pure (GNetwork.NetworkManager'create'results {GNetwork.network = capNetwork}))
  setReply f "Network.show" (\(_ :: C.Parsed GNetwork.Network'show'params) -> pure sampleNetwork'show'results)
  setReply f "Network.start" (\(_ :: C.Parsed GNetwork.Network'start'params) -> pure sampleNetwork'start'results)
  setReply f "Network.stop" (\(_ :: C.Parsed GNetwork.Network'stop'params) -> pure sampleNetwork'stop'results)
  setReply f "Network.edit" (\(_ :: C.Parsed GNetwork.Network'edit'params) -> pure sampleNetwork'edit'results)
  setReply f "Network.delete" (\(_ :: C.Parsed GNetwork.Network'delete'params) -> pure sampleNetwork'delete'results)
  setReply f "Network.attachNode" (\(_ :: C.Parsed GNetwork.Network'attachNode'params) -> pure sampleNetwork'attachNode'results)
  setReply f "Network.detachNode" (\(_ :: C.Parsed GNetwork.Network'detachNode'params) -> pure sampleNetwork'detachNode'results)
  setReply f "NodeManager.list" (\(_ :: C.Parsed GNode.NodeManager'list'params) -> pure sampleNodeManager'list'results)
  setReply f "NodeManager.get" (\(_ :: C.Parsed GNode.NodeManager'get'params) -> pure (GNode.NodeManager'get'results {GNode.node = capNode}))
  setReply f "NodeManager.create" (\(_ :: C.Parsed GNode.NodeManager'create'params) -> pure (GNode.NodeManager'create'results {GNode.node = capNode}))
  setReply f "Node.show" (\(_ :: C.Parsed GNode.Node'show'params) -> pure sampleNode'show'results)
  setReply f "Node.edit" (\(_ :: C.Parsed GNode.Node'edit'params) -> pure sampleNode'edit'results)
  setReply f "Node.drain" (\(_ :: C.Parsed GNode.Node'drain'params) -> pure sampleNode'drain'results)
  setReply f "Node.delete" (\(_ :: C.Parsed GNode.Node'delete'params) -> pure sampleNode'delete'results)
  setReply f "SshKeyManager.list" (\(_ :: C.Parsed GSshkey.SshKeyManager'list'params) -> pure sampleSshKeyManager'list'results)
  setReply f "SshKeyManager.get" (\(_ :: C.Parsed GSshkey.SshKeyManager'get'params) -> pure (GSshkey.SshKeyManager'get'results {GSshkey.key = capSshKey}))
  setReply f "SshKeyManager.create" (\(_ :: C.Parsed GSshkey.SshKeyManager'create'params) -> pure (GSshkey.SshKeyManager'create'results {GSshkey.key = capSshKey}))
  setReply f "SshKey.show" (\(_ :: C.Parsed GSshkey.SshKey'show'params) -> pure sampleSshKey'show'results)
  setReply f "SshKey.delete" (\(_ :: C.Parsed GSshkey.SshKey'delete'params) -> pure sampleSshKey'delete'results)
  setReply f "TemplateManager.list" (\(_ :: C.Parsed GTemplate.TemplateManager'list'params) -> pure sampleTemplateManager'list'results)
  setReply f "TemplateManager.get" (\(_ :: C.Parsed GTemplate.TemplateManager'get'params) -> pure (GTemplate.TemplateManager'get'results {GTemplate.template = capTemplate}))
  setReply f "TemplateManager.create" (\(_ :: C.Parsed GTemplate.TemplateManager'create'params) -> pure (GTemplate.TemplateManager'create'results {GTemplate.template = capTemplate}))
  setReply f "Template.show" (\(_ :: C.Parsed GTemplate.Template'show'params) -> pure sampleTemplate'show'results)
  setReply f "Template.delete" (\(_ :: C.Parsed GTemplate.Template'delete'params) -> pure sampleTemplate'delete'results)
  setReply f "Template.instantiate" (\(_ :: C.Parsed GTemplate.Template'instantiate'params) -> pure (GTemplate.Template'instantiate'results {GTemplate.vm = capVm}))
  setReply f "Template.update" (\(_ :: C.Parsed GTemplate.Template'update'params) -> pure sampleTemplate'update'results)
  setReply f "TaskManager.list" (\(_ :: C.Parsed GTask.TaskManager'list'params) -> pure sampleTaskManager'list'results)
  setReply f "TaskManager.get" (\(_ :: C.Parsed GTask.TaskManager'get'params) -> pure (GTask.TaskManager'get'results {GTask.task = capTask}))
  setReply f "TaskManager.listChildren" (\(_ :: C.Parsed GTask.TaskManager'listChildren'params) -> pure sampleTaskManager'listChildren'results)
  setReply f "TaskManager.subscribe" (\(_ :: C.Parsed GTask.TaskManager'subscribe'params) -> pure (GTask.TaskManager'subscribe'results {GTask.handle = capHandle}))
  setReply f "TaskManager.cancel" (\(_ :: C.Parsed GTask.TaskManager'cancel'params) -> pure sampleTaskManager'cancel'results)
  setReply f "Task.show" (\(_ :: C.Parsed GTask.Task'show'params) -> pure sampleTask'show'results)
  setReply f "CloudInitManager.set" (\(_ :: C.Parsed GCloudinit.CloudInitManager'set'params) -> pure sampleCloudInitManager'set'results)
  setReply f "CloudInitManager.get" (\(_ :: C.Parsed GCloudinit.CloudInitManager'get'params) -> pure sampleCloudInitManager'get'results)
  setReply f "CloudInitManager.delete" (\(_ :: C.Parsed GCloudinit.CloudInitManager'delete'params) -> pure sampleCloudInitManager'delete'results)
  setReply f "ByteSink.write" (\(_ :: C.Parsed GStreams.ByteSink'write'params) -> pure sampleByteSink'write'results)
  setReply f "ByteSink.end" (\(_ :: C.Parsed GStreams.ByteSink'end'params) -> pure sampleByteSink'end'results)
  setReply f "ByteSink.abort" (\(_ :: C.Parsed GStreams.ByteSink'abort'params) -> pure sampleByteSink'abort'results)
  bracket (socket (if tcp then AF_INET else AF_UNIX) Stream 0) close $ \listener -> do
    bind listener (if tcp then SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) else SockAddrUnix path)
    address <- getSocketName listener
    case address of
      SockAddrInet p _ -> writeIORef port (fromIntegral p)
      _ -> pure ()
    listen listener 8
    bracket (newIORef []) (readIORef >=> mapM_ cancel) $ \workers -> do
      let serve = forever $ do
            sock <- fst <$> accept listener
            worker <- async $ handleConn (socketTransport sock C.defaultLimit) C.def {debugMode = False, bootstrap = Just (toClient capDaemon)} `finally` close sock
            atomicModifyIORef' workers (\xs -> (worker : xs, ()))
      withAsync serve $ \_ -> action f

instance GCorvus.Daemon'server_ Mock where
  daemon'ping (Mock f) = handleParsed (respond f "Daemon.ping")
  daemon'status (Mock f) = handleParsed (respond f "Daemon.status")
  daemon'shutdown (Mock f) = handleParsed (respond f "Daemon.shutdown")
  daemon'vms (Mock f) = handleParsed (respond f "Daemon.vms")
  daemon'disks (Mock f) = handleParsed (respond f "Daemon.disks")
  daemon'networks (Mock f) = handleParsed (respond f "Daemon.networks")
  daemon'sshKeys (Mock f) = handleParsed (respond f "Daemon.sshKeys")
  daemon'templates (Mock f) = handleParsed (respond f "Daemon.templates")
  daemon'tasks (Mock f) = handleParsed (respond f "Daemon.tasks")
  daemon'cloudInit (Mock f) = handleParsed (respond f "Daemon.cloudInit")
  daemon'apply (Mock f) = handleParsed (respond f "Daemon.apply")
  daemon'build (Mock f) = handleParsed (respond f "Daemon.build")
  daemon'nodes (Mock f) = handleParsed (respond f "Daemon.nodes")

instance GVm.VmManager'server_ Mock where
  vmManager'list (Mock f) = handleParsed (respond f "VmManager.list")
  vmManager'get (Mock f) = handleParsed (respond f "VmManager.get")
  vmManager'create (Mock f) = handleParsed (respond f "VmManager.create")

instance GVm.Vm'server_ Mock where
  vm'show (Mock f) = handleParsed (respond f "Vm.show")
  vm'start (Mock f) = handleParsed (respond f "Vm.start")
  vm'stop (Mock f) = handleParsed (respond f "Vm.stop")
  vm'pause (Mock f) = handleParsed (respond f "Vm.pause")
  vm'reset (Mock f) = handleParsed (respond f "Vm.reset")
  vm'edit (Mock f) = handleParsed (respond f "Vm.edit")
  vm'delete (Mock f) = handleParsed (respond f "Vm.delete")
  vm'cloudInit (Mock f) = handleParsed (respond f "Vm.cloudInit")
  vm'viewGrant (Mock f) = handleParsed (respond f "Vm.viewGrant")
  vm'guestExec (Mock f) = handleParsed (respond f "Vm.guestExec")
  vm'sendCtrlAltDel (Mock f) = handleParsed (respond f "Vm.sendCtrlAltDel")
  vm'serialConsole (Mock f) = handleParsed (respond f "Vm.serialConsole")
  vm'serialConsoleFlush (Mock f) = handleParsed (respond f "Vm.serialConsoleFlush")
  vm'hmpMonitor (Mock f) = handleParsed (respond f "Vm.hmpMonitor")
  vm'hmpMonitorFlush (Mock f) = handleParsed (respond f "Vm.hmpMonitorFlush")
  vm'subscribeGuestAgent (Mock f) = handleParsed (respond f "Vm.subscribeGuestAgent")
  vm'attachDisk (Mock f) = handleParsed (respond f "Vm.attachDisk")
  vm'detachDisk (Mock f) = handleParsed (respond f "Vm.detachDisk")
  vm'addNetIf (Mock f) = handleParsed (respond f "Vm.addNetIf")
  vm'removeNetIf (Mock f) = handleParsed (respond f "Vm.removeNetIf")
  vm'listNetIfs (Mock f) = handleParsed (respond f "Vm.listNetIfs")
  vm'addSharedDir (Mock f) = handleParsed (respond f "Vm.addSharedDir")
  vm'removeSharedDir (Mock f) = handleParsed (respond f "Vm.removeSharedDir")
  vm'listSharedDirs (Mock f) = handleParsed (respond f "Vm.listSharedDirs")
  vm'snapshotCreate (Mock f) = handleParsed (respond f "Vm.snapshotCreate")
  vm'snapshotList (Mock f) = handleParsed (respond f "Vm.snapshotList")
  vm'snapshotRollback (Mock f) = handleParsed (respond f "Vm.snapshotRollback")
  vm'attachSshKey (Mock f) = handleParsed (respond f "Vm.attachSshKey")
  vm'detachSshKey (Mock f) = handleParsed (respond f "Vm.detachSshKey")
  vm'listSshKeys (Mock f) = handleParsed (respond f "Vm.listSshKeys")
  vm'migrate (Mock f) = handleParsed (respond f "Vm.migrate")
  vm'save (Mock f) = handleParsed (respond f "Vm.save")
  vm'getStatsHistory (Mock f) = handleParsed (respond f "Vm.getStatsHistory")
  vm'subscribeStats (Mock f) = handleParsed (respond f "Vm.subscribeStats")
  vm'snapshotDelete (Mock f) = handleParsed (respond f "Vm.snapshotDelete")
  vm'addAudioDevice (Mock f) = handleParsed (respond f "Vm.addAudioDevice")
  vm'editAudioDevice (Mock f) = handleParsed (respond f "Vm.editAudioDevice")
  vm'removeAudioDevice (Mock f) = handleParsed (respond f "Vm.removeAudioDevice")
  vm'listAudioDevices (Mock f) = handleParsed (respond f "Vm.listAudioDevices")
  vm'editNetIf (Mock f) = handleParsed (respond f "Vm.editNetIf")
  vm'setBalloon (Mock f) = handleParsed (respond f "Vm.setBalloon")

instance GDisk.DiskManager'server_ Mock where
  diskManager'list (Mock f) = handleParsed (respond f "DiskManager.list")
  diskManager'get (Mock f) = handleParsed (respond f "DiskManager.get")
  diskManager'create (Mock f) = handleParsed (respond f "DiskManager.create")
  diskManager'register (Mock f) = handleParsed (respond f "DiskManager.register")
  diskManager'createOverlay (Mock f) = handleParsed (respond f "DiskManager.createOverlay")
  diskManager'clone (Mock f) = handleParsed (respond f "DiskManager.clone")
  diskManager'rebase (Mock f) = handleParsed (respond f "DiskManager.rebase")
  diskManager'import_ (Mock f) = handleParsed (respond f "DiskManager.import")
  diskManager'flatten (Mock f) = handleParsed (respond f "DiskManager.flatten")
  diskManager'copy (Mock f) = handleParsed (respond f "DiskManager.copy")
  diskManager'move (Mock f) = handleParsed (respond f "DiskManager.move")
  diskManager'beginUpload (Mock f) = handleParsed (respond f "DiskManager.beginUpload")
  diskManager'mediaEject (Mock f) = handleParsed (respond f "DiskManager.mediaEject")
  diskManager'mediaChange (Mock f) = handleParsed (respond f "DiskManager.mediaChange")
  diskManager'cleanup (Mock f) = handleParsed (respond f "DiskManager.cleanup")

instance GDisk.DiskUpload'server_ Mock where
  diskUpload'write (Mock f) = handleParsed (respond f "DiskUpload.write")
  diskUpload'finish (Mock f) = handleParsed (respond f "DiskUpload.finish")
  diskUpload'abort (Mock f) = handleParsed (respond f "DiskUpload.abort")

instance GDisk.Disk'server_ Mock where
  disk'show (Mock f) = handleParsed (respond f "Disk.show")
  disk'delete (Mock f) = handleParsed (respond f "Disk.delete")
  disk'refresh (Mock f) = handleParsed (respond f "Disk.refresh")
  disk'resize (Mock f) = handleParsed (respond f "Disk.resize")
  disk'snapshotCreate (Mock f) = handleParsed (respond f "Disk.snapshotCreate")
  disk'snapshotList (Mock f) = handleParsed (respond f "Disk.snapshotList")
  disk'snapshotGet (Mock f) = handleParsed (respond f "Disk.snapshotGet")
  disk'tag (Mock f) = handleParsed (respond f "Disk.tag")
  disk'untag (Mock f) = handleParsed (respond f "Disk.untag")
  disk'registerPlacement (Mock f) = handleParsed (respond f "Disk.registerPlacement")

instance GDisk.Snapshot'server_ Mock where
  snapshot'show (Mock f) = handleParsed (respond f "Snapshot.show")
  snapshot'delete (Mock f) = handleParsed (respond f "Snapshot.delete")
  snapshot'rollback (Mock f) = handleParsed (respond f "Snapshot.rollback")
  snapshot'merge (Mock f) = handleParsed (respond f "Snapshot.merge")

instance GNetwork.NetworkManager'server_ Mock where
  networkManager'list (Mock f) = handleParsed (respond f "NetworkManager.list")
  networkManager'get (Mock f) = handleParsed (respond f "NetworkManager.get")
  networkManager'create (Mock f) = handleParsed (respond f "NetworkManager.create")

instance GNetwork.Network'server_ Mock where
  network'show (Mock f) = handleParsed (respond f "Network.show")
  network'start (Mock f) = handleParsed (respond f "Network.start")
  network'stop (Mock f) = handleParsed (respond f "Network.stop")
  network'edit (Mock f) = handleParsed (respond f "Network.edit")
  network'delete (Mock f) = handleParsed (respond f "Network.delete")
  network'attachNode (Mock f) = handleParsed (respond f "Network.attachNode")
  network'detachNode (Mock f) = handleParsed (respond f "Network.detachNode")

instance GNode.NodeManager'server_ Mock where
  nodeManager'list (Mock f) = handleParsed (respond f "NodeManager.list")
  nodeManager'get (Mock f) = handleParsed (respond f "NodeManager.get")
  nodeManager'create (Mock f) = handleParsed (respond f "NodeManager.create")

instance GNode.Node'server_ Mock where
  node'show (Mock f) = handleParsed (respond f "Node.show")
  node'edit (Mock f) = handleParsed (respond f "Node.edit")
  node'drain (Mock f) = handleParsed (respond f "Node.drain")
  node'delete (Mock f) = handleParsed (respond f "Node.delete")

instance GSshkey.SshKeyManager'server_ Mock where
  sshKeyManager'list (Mock f) = handleParsed (respond f "SshKeyManager.list")
  sshKeyManager'get (Mock f) = handleParsed (respond f "SshKeyManager.get")
  sshKeyManager'create (Mock f) = handleParsed (respond f "SshKeyManager.create")

instance GSshkey.SshKey'server_ Mock where
  sshKey'show (Mock f) = handleParsed (respond f "SshKey.show")
  sshKey'delete (Mock f) = handleParsed (respond f "SshKey.delete")

instance GTemplate.TemplateManager'server_ Mock where
  templateManager'list (Mock f) = handleParsed (respond f "TemplateManager.list")
  templateManager'get (Mock f) = handleParsed (respond f "TemplateManager.get")
  templateManager'create (Mock f) = handleParsed (respond f "TemplateManager.create")

instance GTemplate.Template'server_ Mock where
  template'show (Mock f) = handleParsed (respond f "Template.show")
  template'delete (Mock f) = handleParsed (respond f "Template.delete")
  template'instantiate (Mock f) = handleParsed (respond f "Template.instantiate")
  template'update (Mock f) = handleParsed (respond f "Template.update")

instance GTask.TaskManager'server_ Mock where
  taskManager'list (Mock f) = handleParsed (respond f "TaskManager.list")
  taskManager'get (Mock f) = handleParsed (respond f "TaskManager.get")
  taskManager'listChildren (Mock f) = handleParsed (respond f "TaskManager.listChildren")
  taskManager'subscribe (Mock f) = handleParsed (respond f "TaskManager.subscribe")
  taskManager'cancel (Mock f) = handleParsed (respond f "TaskManager.cancel")

instance GTask.Task'server_ Mock where
  task'show (Mock f) = handleParsed (respond f "Task.show")

instance GCloudinit.CloudInitManager'server_ Mock where
  cloudInitManager'set (Mock f) = handleParsed (respond f "CloudInitManager.set")
  cloudInitManager'get (Mock f) = handleParsed (respond f "CloudInitManager.get")
  cloudInitManager'delete (Mock f) = handleParsed (respond f "CloudInitManager.delete")

instance GStreams.ByteSink'server_ Mock where
  byteSink'write (Mock f) = handleParsed (respond f "ByteSink.write")
  byteSink'end (Mock f) = handleParsed (respond f "ByteSink.end")
  byteSink'abort (Mock f) = handleParsed (respond f "ByteSink.abort")

instance GStreams.Handle'server_ Mock

sampleApplyCreated :: C.Parsed GCorvus.ApplyCreated
sampleApplyCreated = GCorvus.ApplyCreated {GCorvus.name = "fixture", GCorvus.id = 42}

sampleApplyResult :: C.Parsed GCorvus.ApplyResult
sampleApplyResult = GCorvus.ApplyResult {GCorvus.sshKeys = [sampleApplyCreated], GCorvus.disks = [sampleApplyCreated], GCorvus.networks = [sampleApplyCreated], GCorvus.vms = [sampleApplyCreated], GCorvus.templates = [sampleApplyCreated]}

sampleAudioDeviceInfo :: C.Parsed GVm.AudioDeviceInfo
sampleAudioDeviceInfo = GVm.AudioDeviceInfo {GVm.id = 42, GVm.backend = GEnums.AudioBackend'pulse, GVm.options = "fixture", GVm.model = GEnums.AudioDeviceModel'virtioSound}

sampleByteSink'abort'results :: C.Parsed GStreams.ByteSink'abort'results
sampleByteSink'abort'results = GStreams.ByteSink'abort'results

sampleByteSink'end'results :: C.Parsed GStreams.ByteSink'end'results
sampleByteSink'end'results = GStreams.ByteSink'end'results

sampleByteSink'write'results :: C.Parsed GStreams.ByteSink'write'results
sampleByteSink'write'results = GStreams.ByteSink'write'results

sampleCloudInitInfo :: C.Parsed GCloudinit.CloudInitInfo
sampleCloudInitInfo = GCloudinit.CloudInitInfo {GCloudinit.hasUserData = True, GCloudinit.userData = "fixture", GCloudinit.hasNetworkConfig = True, GCloudinit.networkConfig = "fixture", GCloudinit.injectSshKeys = True}

sampleCloudInitManager'delete'results :: C.Parsed GCloudinit.CloudInitManager'delete'results
sampleCloudInitManager'delete'results = GCloudinit.CloudInitManager'delete'results

sampleCloudInitManager'get'results :: C.Parsed GCloudinit.CloudInitManager'get'results
sampleCloudInitManager'get'results = GCloudinit.CloudInitManager'get'results {GCloudinit.config = sampleCloudInitInfo}

sampleCloudInitManager'set'results :: C.Parsed GCloudinit.CloudInitManager'set'results
sampleCloudInitManager'set'results = GCloudinit.CloudInitManager'set'results

sampleDaemon'ping'results :: C.Parsed GCorvus.Daemon'ping'results
sampleDaemon'ping'results = GCorvus.Daemon'ping'results

sampleDaemon'shutdown'results :: C.Parsed GCorvus.Daemon'shutdown'results
sampleDaemon'shutdown'results = GCorvus.Daemon'shutdown'results

sampleDaemon'status'results :: C.Parsed GCorvus.Daemon'status'results
sampleDaemon'status'results = GCorvus.Daemon'status'results {GCorvus.info = sampleStatusInfo}

sampleDisk'delete'results :: C.Parsed GDisk.Disk'delete'results
sampleDisk'delete'results = GDisk.Disk'delete'results

sampleDisk'refresh'results :: C.Parsed GDisk.Disk'refresh'results
sampleDisk'refresh'results = GDisk.Disk'refresh'results {GDisk.info = sampleDiskImageInfo}

sampleDisk'registerPlacement'results :: C.Parsed GDisk.Disk'registerPlacement'results
sampleDisk'registerPlacement'results = GDisk.Disk'registerPlacement'results

sampleDisk'resize'results :: C.Parsed GDisk.Disk'resize'results
sampleDisk'resize'results = GDisk.Disk'resize'results

sampleDisk'show'results :: C.Parsed GDisk.Disk'show'results
sampleDisk'show'results = GDisk.Disk'show'results {GDisk.info = sampleDiskImageInfo}

sampleDisk'snapshotList'results :: C.Parsed GDisk.Disk'snapshotList'results
sampleDisk'snapshotList'results = GDisk.Disk'snapshotList'results {GDisk.snapshots = [sampleSnapshotInfo]}

sampleDisk'tag'results :: C.Parsed GDisk.Disk'tag'results
sampleDisk'tag'results = GDisk.Disk'tag'results

sampleDisk'untag'results :: C.Parsed GDisk.Disk'untag'results
sampleDisk'untag'results = GDisk.Disk'untag'results

sampleDiskAttachment :: C.Parsed GDisk.DiskAttachment
sampleDiskAttachment = GDisk.DiskAttachment {GDisk.vm = sampleNamedRef}

sampleDiskCleanupPlacement :: C.Parsed GDisk.DiskCleanupPlacement
sampleDiskCleanupPlacement = GDisk.DiskCleanupPlacement {GDisk.node = sampleNamedRef, GDisk.filePath = "fixture", GDisk.status = "fixture", GDisk.reason = "fixture"}

sampleDiskCleanupReport :: C.Parsed GDisk.DiskCleanupReport
sampleDiskCleanupReport = GDisk.DiskCleanupReport {GDisk.dryRun = True, GDisk.versions = [sampleDiskCleanupVersion], GDisk.removedVersions = 42, GDisk.removedPlacements = 42, GDisk.failures = 0}

sampleDiskCleanupVersion :: C.Parsed GDisk.DiskCleanupVersion
sampleDiskCleanupVersion = GDisk.DiskCleanupVersion {GDisk.diskImage = sampleNamedRef, GDisk.tags = ["fixture"], GDisk.status = "fixture", GDisk.reason = "fixture", GDisk.versionDeleted = True, GDisk.placements = [sampleDiskCleanupPlacement]}

sampleDiskImageInfo :: C.Parsed GDisk.DiskImageInfo
sampleDiskImageInfo = GDisk.DiskImageInfo {GDisk.id = 42, GDisk.name = "fixture", GDisk.placements = [sampleDiskImagePlacement], GDisk.format = GEnums.DriveFormat'qcow2, GDisk.size = 42, GDisk.createdAt = 1000000000, GDisk.attachedTo = [sampleDiskAttachment], GDisk.backingImage = sampleNamedRef, GDisk.ephemeral = True, GDisk.tags = ["fixture"]}

sampleDiskImagePlacement :: C.Parsed GDisk.DiskImagePlacement
sampleDiskImagePlacement = GDisk.DiskImagePlacement {GDisk.node = sampleNamedRef, GDisk.filePath = "fixture"}

sampleDiskManager'cleanup'results :: C.Parsed GDisk.DiskManager'cleanup'results
sampleDiskManager'cleanup'results = GDisk.DiskManager'cleanup'results {GDisk.report = sampleDiskCleanupReport}

sampleDiskManager'copy'results :: C.Parsed GDisk.DiskManager'copy'results
sampleDiskManager'copy'results = GDisk.DiskManager'copy'results {GDisk.taskId = 42}

sampleDiskManager'flatten'results :: C.Parsed GDisk.DiskManager'flatten'results
sampleDiskManager'flatten'results = GDisk.DiskManager'flatten'results

sampleDiskManager'import'results :: C.Parsed GDisk.DiskManager'import'results
sampleDiskManager'import'results = GDisk.DiskManager'import'results {GDisk.taskId = 42}

sampleDiskManager'list'results :: C.Parsed GDisk.DiskManager'list'results
sampleDiskManager'list'results = GDisk.DiskManager'list'results {GDisk.disks = [sampleDiskImageInfo]}

sampleDiskManager'mediaChange'results :: C.Parsed GDisk.DiskManager'mediaChange'results
sampleDiskManager'mediaChange'results = GDisk.DiskManager'mediaChange'results

sampleDiskManager'mediaEject'results :: C.Parsed GDisk.DiskManager'mediaEject'results
sampleDiskManager'mediaEject'results = GDisk.DiskManager'mediaEject'results

sampleDiskManager'move'results :: C.Parsed GDisk.DiskManager'move'results
sampleDiskManager'move'results = GDisk.DiskManager'move'results {GDisk.taskId = 42}

sampleDiskManager'rebase'results :: C.Parsed GDisk.DiskManager'rebase'results
sampleDiskManager'rebase'results = GDisk.DiskManager'rebase'results

sampleDiskUpload'abort'results :: C.Parsed GDisk.DiskUpload'abort'results
sampleDiskUpload'abort'results = GDisk.DiskUpload'abort'results

sampleDiskUpload'write'results :: C.Parsed GDisk.DiskUpload'write'results
sampleDiskUpload'write'results = GDisk.DiskUpload'write'results

sampleDriveInfo :: C.Parsed GVm.DriveInfo
sampleDriveInfo = GVm.DriveInfo {GVm.id = 42, GVm.diskImage = sampleNamedRef, GVm.interface = GEnums.DriveInterface'virtio, GVm.filePath = "fixture", GVm.format = GEnums.DriveFormat'qcow2, GVm.media = GEnums.DriveMedia'disk, GVm.readOnly = True, GVm.cacheType = GEnums.CacheType'none, GVm.discard = True}

sampleDriveIo :: C.Parsed GVm.DriveIo
sampleDriveIo = GVm.DriveIo {GVm.name = "fixture", GVm.readBytesTotal = 42, GVm.writeBytesTotal = 42, GVm.readOpsTotal = 42, GVm.writeOpsTotal = 42}

sampleGuestExecResult :: C.Parsed GVm.GuestExecResult
sampleGuestExecResult = GVm.GuestExecResult {GVm.exitCode = 42, GVm.stdout = "fixture", GVm.stderr = "fixture"}

sampleNamedRef :: C.Parsed GCommon.NamedRef
sampleNamedRef = GCommon.NamedRef {GCommon.id = 42, GCommon.name = "fixture"}

sampleNetIfInfo :: C.Parsed GVm.NetIfInfo
sampleNetIfInfo = GVm.NetIfInfo {GVm.id = 42, GVm.type_ = GEnums.NetInterfaceType'user, GVm.hostDevice = "fixture", GVm.macAddress = "fixture", GVm.network = sampleNamedRef, GVm.guestIpAddresses = "fixture", GVm.ipAddress = "fixture", GVm.model = GEnums.NetworkDeviceModel'virtioNetPci}

sampleNetIo :: C.Parsed GVm.NetIo
sampleNetIo = GVm.NetIo {GVm.tapName = "fixture", GVm.rxBytesTotal = 42, GVm.txBytesTotal = 42}

sampleNetwork'attachNode'results :: C.Parsed GNetwork.Network'attachNode'results
sampleNetwork'attachNode'results = GNetwork.Network'attachNode'results

sampleNetwork'delete'results :: C.Parsed GNetwork.Network'delete'results
sampleNetwork'delete'results = GNetwork.Network'delete'results

sampleNetwork'detachNode'results :: C.Parsed GNetwork.Network'detachNode'results
sampleNetwork'detachNode'results = GNetwork.Network'detachNode'results

sampleNetwork'edit'results :: C.Parsed GNetwork.Network'edit'results
sampleNetwork'edit'results = GNetwork.Network'edit'results

sampleNetwork'show'results :: C.Parsed GNetwork.Network'show'results
sampleNetwork'show'results = GNetwork.Network'show'results {GNetwork.info = sampleNetworkInfo}

sampleNetwork'start'results :: C.Parsed GNetwork.Network'start'results
sampleNetwork'start'results = GNetwork.Network'start'results

sampleNetwork'stop'results :: C.Parsed GNetwork.Network'stop'results
sampleNetwork'stop'results = GNetwork.Network'stop'results

sampleNetworkInfo :: C.Parsed GNetwork.NetworkInfo
sampleNetworkInfo = GNetwork.NetworkInfo {GNetwork.id = 42, GNetwork.name = "fixture", GNetwork.subnet = "fixture", GNetwork.dhcp = True, GNetwork.nat = True, GNetwork.running = True, GNetwork.dnsmasqPid = 42, GNetwork.createdAt = 1000000000, GNetwork.autostart = True, GNetwork.vni = 42, GNetwork.peerNodeIds = [42], GNetwork.dnsServers = ["fixture"], GNetwork.domain = "fixture", GNetwork.hostDns = True}

sampleNetworkManager'list'results :: C.Parsed GNetwork.NetworkManager'list'results
sampleNetworkManager'list'results = GNetwork.NetworkManager'list'results {GNetwork.networks = [sampleNetworkInfo]}

sampleNode'delete'results :: C.Parsed GNode.Node'delete'results
sampleNode'delete'results = GNode.Node'delete'results

sampleNode'drain'results :: C.Parsed GNode.Node'drain'results
sampleNode'drain'results = GNode.Node'drain'results

sampleNode'edit'results :: C.Parsed GNode.Node'edit'results
sampleNode'edit'results = GNode.Node'edit'results

sampleNode'show'results :: C.Parsed GNode.Node'show'results
sampleNode'show'results = GNode.Node'show'results {GNode.details = sampleNodeDetails}

sampleNodeDetails :: C.Parsed GNode.NodeDetails
sampleNodeDetails = GNode.NodeDetails {GNode.id = 42, GNode.name = "fixture", GNode.host = "fixture", GNode.nodeAgentPort = 42, GNode.netAgentPort = 42, GNode.basePath = "fixture", GNode.description = "fixture", GNode.adminState = GEnums.NodeAdminState'online, GNode.createdAt = 1000000000, GNode.cpuCount = 2, GNode.ramTotal = 42, GNode.ramFree = 42, GNode.storageBytesTotal = 42, GNode.storageBytesFree = 42, GNode.loadAvg1 = 42, GNode.loadAvg5 = 42, GNode.loadAvg15 = 42, GNode.kernelRelease = "fixture", GNode.agentVersion = "fixture", GNode.lastNodeAgentPushAt = 42, GNode.lastNetAgentPushAt = 42, GNode.netdDisabled = True, GNode.netdConnected = True}

sampleNodeInfo :: C.Parsed GNode.NodeInfo
sampleNodeInfo = GNode.NodeInfo {GNode.id = 42, GNode.name = "fixture", GNode.host = "fixture", GNode.nodeAgentPort = 42, GNode.netAgentPort = 42, GNode.adminState = GEnums.NodeAdminState'online, GNode.createdAt = 1000000000, GNode.cpuCount = 2, GNode.ramTotal = 42, GNode.ramFree = 42, GNode.storageBytesTotal = 42, GNode.storageBytesFree = 42, GNode.loadAvg1 = 42, GNode.lastNodeAgentPushAt = 42, GNode.lastNetAgentPushAt = 42, GNode.netdDisabled = True, GNode.netdConnected = True}

sampleNodeManager'list'results :: C.Parsed GNode.NodeManager'list'results
sampleNodeManager'list'results = GNode.NodeManager'list'results {GNode.nodes = [sampleNodeInfo]}

sampleSharedDirInfo :: C.Parsed GVm.SharedDirInfo
sampleSharedDirInfo = GVm.SharedDirInfo {GVm.id = 42, GVm.path = "fixture", GVm.tag = "fixture", GVm.cache = GEnums.SharedDirCache'always, GVm.readOnly = True, GVm.pid = 42}

sampleSnapshot'delete'results :: C.Parsed GDisk.Snapshot'delete'results
sampleSnapshot'delete'results = GDisk.Snapshot'delete'results

sampleSnapshot'merge'results :: C.Parsed GDisk.Snapshot'merge'results
sampleSnapshot'merge'results = GDisk.Snapshot'merge'results

sampleSnapshot'rollback'results :: C.Parsed GDisk.Snapshot'rollback'results
sampleSnapshot'rollback'results = GDisk.Snapshot'rollback'results

sampleSnapshot'show'results :: C.Parsed GDisk.Snapshot'show'results
sampleSnapshot'show'results = GDisk.Snapshot'show'results {GDisk.info = sampleSnapshotInfo}

sampleSnapshotInfo :: C.Parsed GDisk.SnapshotInfo
sampleSnapshotInfo = GDisk.SnapshotInfo {GDisk.id = 42, GDisk.name = "fixture", GDisk.createdAt = 1000000000, GDisk.size = 42, GDisk.live = True, GDisk.quiesced = True, GDisk.hasVmstate = True}

sampleSshKey'delete'results :: C.Parsed GSshkey.SshKey'delete'results
sampleSshKey'delete'results = GSshkey.SshKey'delete'results

sampleSshKey'show'results :: C.Parsed GSshkey.SshKey'show'results
sampleSshKey'show'results = GSshkey.SshKey'show'results {GSshkey.info = sampleSshKeyInfo}

sampleSshKeyInfo :: C.Parsed GSshkey.SshKeyInfo
sampleSshKeyInfo = GSshkey.SshKeyInfo {GSshkey.id = 42, GSshkey.name = "fixture", GSshkey.publicKey = "fixture", GSshkey.createdAt = 1000000000, GSshkey.attachedVms = [sampleVmAttachment]}

sampleSshKeyManager'list'results :: C.Parsed GSshkey.SshKeyManager'list'results
sampleSshKeyManager'list'results = GSshkey.SshKeyManager'list'results {GSshkey.keys = [sampleSshKeyInfo]}

sampleStatusInfo :: C.Parsed GCommon.StatusInfo
sampleStatusInfo = GCommon.StatusInfo {GCommon.uptimeSeconds = 42, GCommon.connections = 42, GCommon.version = "fixture", GCommon.protocolVersion = 42, GCommon.databaseBackend = "fixture", GCommon.databaseVersion = "fixture"}

sampleTask'show'results :: C.Parsed GTask.Task'show'results
sampleTask'show'results = GTask.Task'show'results {GTask.info = sampleTaskInfo}

sampleTaskInfo :: C.Parsed GTask.TaskInfo
sampleTaskInfo = GTask.TaskInfo {GTask.id = 42, GTask.parentId = 7, GTask.startedAt = 1000000000, GTask.finishedAt = 61000000000, GTask.subsystem = GEnums.TaskSubsystem'vm, GTask.entity = sampleNamedRef, GTask.command = "fixture", GTask.result = GEnums.TaskResult'success, GTask.message = "fixture", GTask.clientName = "fixture"}

sampleTaskManager'cancel'results :: C.Parsed GTask.TaskManager'cancel'results
sampleTaskManager'cancel'results = GTask.TaskManager'cancel'results

sampleTaskManager'list'results :: C.Parsed GTask.TaskManager'list'results
sampleTaskManager'list'results = GTask.TaskManager'list'results {GTask.tasks = [sampleTaskInfo]}

sampleTaskManager'listChildren'results :: C.Parsed GTask.TaskManager'listChildren'results
sampleTaskManager'listChildren'results = GTask.TaskManager'listChildren'results {GTask.tasks = [sampleTaskInfo]}

sampleTemplate'delete'results :: C.Parsed GTemplate.Template'delete'results
sampleTemplate'delete'results = GTemplate.Template'delete'results

sampleTemplate'show'results :: C.Parsed GTemplate.Template'show'results
sampleTemplate'show'results = GTemplate.Template'show'results {GTemplate.details = sampleTemplateDetails}

sampleTemplate'update'results :: C.Parsed GTemplate.Template'update'results
sampleTemplate'update'results = GTemplate.Template'update'results

sampleTemplateAudioDeviceInfo :: C.Parsed GTemplate.TemplateAudioDeviceInfo
sampleTemplateAudioDeviceInfo = GTemplate.TemplateAudioDeviceInfo {GTemplate.id = 42, GTemplate.backend = GEnums.AudioBackend'pulse, GTemplate.options = "fixture", GTemplate.model = GEnums.AudioDeviceModel'virtioSound}

sampleTemplateDetails :: C.Parsed GTemplate.TemplateDetails
sampleTemplateDetails = GTemplate.TemplateDetails {GTemplate.id = 42, GTemplate.name = "fixture", GTemplate.cpuCount = 2, GTemplate.ram = 1073741824, GTemplate.description = "fixture", GTemplate.headless = True, GTemplate.cloudInit = True, GTemplate.guestAgent = True, GTemplate.autostart = True, GTemplate.cloudInitConfig = sampleCloudInitInfo, GTemplate.createdAt = 1000000000, GTemplate.drives = [sampleTemplateDriveInfo], GTemplate.netIfs = [sampleTemplateNetIfInfo], GTemplate.sshKeys = [sampleTemplateSshKeyInfo], GTemplate.rebootQuirk = True, GTemplate.sharedDirs = [sampleTemplateSharedDirInfo], GTemplate.tpm = True, GTemplate.audioDevices = [sampleTemplateAudioDeviceInfo], GTemplate.graphicsAdapter = GEnums.GraphicsAdapter'virtioVga, GTemplate.vsock = True, GTemplate.balloon = True, GTemplate.rng = True}

sampleTemplateDriveInfo :: C.Parsed GTemplate.TemplateDriveInfo
sampleTemplateDriveInfo = GTemplate.TemplateDriveInfo {GTemplate.diskImage = sampleNamedRef, GTemplate.interface = GEnums.DriveInterface'virtio, GTemplate.hasMedia = True, GTemplate.media = GEnums.DriveMedia'disk, GTemplate.readOnly = True, GTemplate.cacheType = GEnums.CacheType'none, GTemplate.discard = True, GTemplate.cloneStrategy = GEnums.TemplateCloneStrategy'clone, GTemplate.size = 42, GTemplate.hasFormat = True, GTemplate.format = GEnums.DriveFormat'qcow2, GTemplate.hasEphemeral = True, GTemplate.ephemeral = True, GTemplate.diskSelector = "fixture", GTemplate.diskName = "fixture"}

sampleTemplateManager'list'results :: C.Parsed GTemplate.TemplateManager'list'results
sampleTemplateManager'list'results = GTemplate.TemplateManager'list'results {GTemplate.templates = [sampleTemplateVmInfo]}

sampleTemplateNetIfInfo :: C.Parsed GTemplate.TemplateNetIfInfo
sampleTemplateNetIfInfo = GTemplate.TemplateNetIfInfo {GTemplate.type_ = GEnums.NetInterfaceType'user, GTemplate.hostDevice = "fixture", GTemplate.network = "fixture", GTemplate.model = GEnums.NetworkDeviceModel'virtioNetPci}

sampleTemplateSharedDirInfo :: C.Parsed GTemplate.TemplateSharedDirInfo
sampleTemplateSharedDirInfo = GTemplate.TemplateSharedDirInfo {GTemplate.id = 42, GTemplate.path = "fixture", GTemplate.tag = "fixture", GTemplate.cache = GEnums.SharedDirCache'always, GTemplate.readOnly = True}

sampleTemplateSshKeyInfo :: C.Parsed GTemplate.TemplateSshKeyInfo
sampleTemplateSshKeyInfo = GTemplate.TemplateSshKeyInfo {GTemplate.id = 42, GTemplate.name = "fixture"}

sampleTemplateVmInfo :: C.Parsed GTemplate.TemplateVmInfo
sampleTemplateVmInfo = GTemplate.TemplateVmInfo {GTemplate.id = 42, GTemplate.name = "fixture", GTemplate.cpuCount = 2, GTemplate.ram = 1073741824, GTemplate.description = "fixture", GTemplate.headless = True, GTemplate.guestAgent = True, GTemplate.autostart = True, GTemplate.rebootQuirk = True, GTemplate.tpm = True, GTemplate.graphicsAdapter = GEnums.GraphicsAdapter'virtioVga, GTemplate.vsock = True, GTemplate.balloon = True, GTemplate.rng = True}

sampleViewGrant :: C.Parsed GCommon.ViewGrant
sampleViewGrant = GCommon.ViewGrant {GCommon.host = "fixture", GCommon.port = 42, GCommon.password = "fixture", GCommon.ttlSeconds = 42}

sampleVm'addAudioDevice'results :: C.Parsed GVm.Vm'addAudioDevice'results
sampleVm'addAudioDevice'results = GVm.Vm'addAudioDevice'results {GVm.audioDeviceId = 42}

sampleVm'addNetIf'results :: C.Parsed GVm.Vm'addNetIf'results
sampleVm'addNetIf'results = GVm.Vm'addNetIf'results {GVm.netIfId = 42}

sampleVm'addSharedDir'results :: C.Parsed GVm.Vm'addSharedDir'results
sampleVm'addSharedDir'results = GVm.Vm'addSharedDir'results {GVm.sharedDirId = 42}

sampleVm'attachDisk'results :: C.Parsed GVm.Vm'attachDisk'results
sampleVm'attachDisk'results = GVm.Vm'attachDisk'results {GVm.driveId = 42}

sampleVm'attachSshKey'results :: C.Parsed GVm.Vm'attachSshKey'results
sampleVm'attachSshKey'results = GVm.Vm'attachSshKey'results

sampleVm'cloudInit'results :: C.Parsed GVm.Vm'cloudInit'results
sampleVm'cloudInit'results = GVm.Vm'cloudInit'results {GVm.config = sampleCloudInitInfo}

sampleVm'delete'results :: C.Parsed GVm.Vm'delete'results
sampleVm'delete'results = GVm.Vm'delete'results

sampleVm'detachDisk'results :: C.Parsed GVm.Vm'detachDisk'results
sampleVm'detachDisk'results = GVm.Vm'detachDisk'results

sampleVm'detachSshKey'results :: C.Parsed GVm.Vm'detachSshKey'results
sampleVm'detachSshKey'results = GVm.Vm'detachSshKey'results

sampleVm'edit'results :: C.Parsed GVm.Vm'edit'results
sampleVm'edit'results = GVm.Vm'edit'results

sampleVm'editAudioDevice'results :: C.Parsed GVm.Vm'editAudioDevice'results
sampleVm'editAudioDevice'results = GVm.Vm'editAudioDevice'results

sampleVm'editNetIf'results :: C.Parsed GVm.Vm'editNetIf'results
sampleVm'editNetIf'results = GVm.Vm'editNetIf'results

sampleVm'getStatsHistory'results :: C.Parsed GVm.Vm'getStatsHistory'results
sampleVm'getStatsHistory'results = GVm.Vm'getStatsHistory'results {GVm.samples = [sampleVmStats]}

sampleVm'guestExec'results :: C.Parsed GVm.Vm'guestExec'results
sampleVm'guestExec'results = GVm.Vm'guestExec'results {GVm.result = sampleGuestExecResult}

sampleVm'hmpMonitorFlush'results :: C.Parsed GVm.Vm'hmpMonitorFlush'results
sampleVm'hmpMonitorFlush'results = GVm.Vm'hmpMonitorFlush'results

sampleVm'listAudioDevices'results :: C.Parsed GVm.Vm'listAudioDevices'results
sampleVm'listAudioDevices'results = GVm.Vm'listAudioDevices'results {GVm.audioDevices = [sampleAudioDeviceInfo]}

sampleVm'listNetIfs'results :: C.Parsed GVm.Vm'listNetIfs'results
sampleVm'listNetIfs'results = GVm.Vm'listNetIfs'results {GVm.netIfs = [sampleNetIfInfo]}

sampleVm'listSharedDirs'results :: C.Parsed GVm.Vm'listSharedDirs'results
sampleVm'listSharedDirs'results = GVm.Vm'listSharedDirs'results {GVm.sharedDirs = [sampleSharedDirInfo]}

sampleVm'listSshKeys'results :: C.Parsed GVm.Vm'listSshKeys'results
sampleVm'listSshKeys'results = GVm.Vm'listSshKeys'results {GVm.keys = [sampleSshKeyInfo]}

sampleVm'migrate'results :: C.Parsed GVm.Vm'migrate'results
sampleVm'migrate'results = GVm.Vm'migrate'results {GVm.taskId = 42}

sampleVm'pause'results :: C.Parsed GVm.Vm'pause'results
sampleVm'pause'results = GVm.Vm'pause'results {GVm.status = GEnums.VmStatus'running}

sampleVm'removeAudioDevice'results :: C.Parsed GVm.Vm'removeAudioDevice'results
sampleVm'removeAudioDevice'results = GVm.Vm'removeAudioDevice'results

sampleVm'removeNetIf'results :: C.Parsed GVm.Vm'removeNetIf'results
sampleVm'removeNetIf'results = GVm.Vm'removeNetIf'results

sampleVm'removeSharedDir'results :: C.Parsed GVm.Vm'removeSharedDir'results
sampleVm'removeSharedDir'results = GVm.Vm'removeSharedDir'results

sampleVm'reset'results :: C.Parsed GVm.Vm'reset'results
sampleVm'reset'results = GVm.Vm'reset'results {GVm.status = GEnums.VmStatus'running}

sampleVm'save'results :: C.Parsed GVm.Vm'save'results
sampleVm'save'results = GVm.Vm'save'results {GVm.status = GEnums.VmStatus'running}

sampleVm'sendCtrlAltDel'results :: C.Parsed GVm.Vm'sendCtrlAltDel'results
sampleVm'sendCtrlAltDel'results = GVm.Vm'sendCtrlAltDel'results

sampleVm'serialConsoleFlush'results :: C.Parsed GVm.Vm'serialConsoleFlush'results
sampleVm'serialConsoleFlush'results = GVm.Vm'serialConsoleFlush'results

sampleVm'setBalloon'results :: C.Parsed GVm.Vm'setBalloon'results
sampleVm'setBalloon'results = GVm.Vm'setBalloon'results

sampleVm'show'results :: C.Parsed GVm.Vm'show'results
sampleVm'show'results = GVm.Vm'show'results {GVm.details = sampleVmDetails}

sampleVm'snapshotCreate'results :: C.Parsed GVm.Vm'snapshotCreate'results
sampleVm'snapshotCreate'results = GVm.Vm'snapshotCreate'results {GVm.info = sampleVmSnapshotInfo}

sampleVm'snapshotDelete'results :: C.Parsed GVm.Vm'snapshotDelete'results
sampleVm'snapshotDelete'results = GVm.Vm'snapshotDelete'results

sampleVm'snapshotList'results :: C.Parsed GVm.Vm'snapshotList'results
sampleVm'snapshotList'results = GVm.Vm'snapshotList'results {GVm.snapshots = [sampleVmSnapshotInfo]}

sampleVm'snapshotRollback'results :: C.Parsed GVm.Vm'snapshotRollback'results
sampleVm'snapshotRollback'results = GVm.Vm'snapshotRollback'results

sampleVm'start'results :: C.Parsed GVm.Vm'start'results
sampleVm'start'results = GVm.Vm'start'results {GVm.status = GEnums.VmStatus'running}

sampleVm'stop'results :: C.Parsed GVm.Vm'stop'results
sampleVm'stop'results = GVm.Vm'stop'results {GVm.status = GEnums.VmStatus'running}

sampleVm'viewGrant'results :: C.Parsed GVm.Vm'viewGrant'results
sampleVm'viewGrant'results = GVm.Vm'viewGrant'results {GVm.grant = sampleViewGrant}

sampleVmAttachment :: C.Parsed GSshkey.VmAttachment
sampleVmAttachment = GSshkey.VmAttachment {GSshkey.vm = sampleNamedRef}

sampleVmDetails :: C.Parsed GVm.VmDetails
sampleVmDetails = GVm.VmDetails {GVm.id = 42, GVm.name = "fixture", GVm.createdAt = 1000000000, GVm.status = GEnums.VmStatus'running, GVm.cpuCount = 2, GVm.ram = 1073741824, GVm.description = "fixture", GVm.drives = [sampleDriveInfo], GVm.netIfs = [sampleNetIfInfo], GVm.sharedDirs = [sampleSharedDirInfo], GVm.headless = True, GVm.monitorSocket = "fixture", GVm.spicePort = 42, GVm.vsockCid = 42, GVm.serialSocket = "fixture", GVm.guestAgentSocket = "fixture", GVm.guestAgent = True, GVm.cloudInit = True, GVm.cloudInitConfig = sampleCloudInitInfo, GVm.lastHealthcheck = 1000000000, GVm.autostart = True, GVm.errorMessage = "fixture", GVm.lastErrorAt = 42, GVm.rebootQuirk = True, GVm.node = sampleNamedRef, GVm.cpuModel = "fixture", GVm.stats = sampleVmStats, GVm.tpm = True, GVm.audioDevices = [sampleAudioDeviceInfo], GVm.graphicsAdapter = GEnums.GraphicsAdapter'virtioVga, GVm.vsock = True, GVm.balloon = True, GVm.rng = True}

sampleVmInfo :: C.Parsed GVm.VmInfo
sampleVmInfo = GVm.VmInfo {GVm.id = 42, GVm.name = "fixture", GVm.status = GEnums.VmStatus'running, GVm.cpuCount = 2, GVm.ram = 1073741824, GVm.headless = True, GVm.guestAgent = True, GVm.cloudInit = True, GVm.lastHealthcheck = 1000000000, GVm.autostart = True, GVm.rebootQuirk = True, GVm.node = sampleNamedRef, GVm.cpuModel = "fixture", GVm.tpm = True, GVm.graphicsAdapter = GEnums.GraphicsAdapter'virtioVga, GVm.vsock = True, GVm.balloon = True, GVm.rng = True}

sampleVmManager'list'results :: C.Parsed GVm.VmManager'list'results
sampleVmManager'list'results = GVm.VmManager'list'results {GVm.vms = [sampleVmInfo]}

sampleVmSnapshotInfo :: C.Parsed GVm.VmSnapshotInfo
sampleVmSnapshotInfo = GVm.VmSnapshotInfo {GVm.name = "fixture", GVm.createdAt = 1000000000, GVm.vm = sampleNamedRef, GVm.carrierDisk = sampleNamedRef, GVm.diskCount = 42, GVm.totalSize = 42}

sampleVmStats :: C.Parsed GVm.VmStats
sampleVmStats = GVm.VmStats {GVm.sampledAtNanos = 42, GVm.intervalMillis = 42, GVm.cpuJiffiesTotal = 42, GVm.clkTck = 42, GVm.hostRssBytes = 42, GVm.balloonActualBytes = 42, GVm.balloonMaxBytes = 42, GVm.drives = [sampleDriveIo], GVm.nets = [sampleNetIo]}

-- Capnp's supervised callback workers report cancellation through GHC's
-- uncaught-exception hook during normal teardown. Keep other errors visible.
quietCancellation :: IO a -> IO a
quietCancellation action = bracket getUncaughtExceptionHandler setUncaughtExceptionHandler $ \original -> do
  setUncaughtExceptionHandler $ \exception -> case fromException exception :: Maybe AsyncCancelled of
    Just _ -> pure ()
    Nothing -> original exception
  action
