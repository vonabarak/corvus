{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Daemon-side Cap'n Proto client for `corvus-nodeagent`.
--
-- Phase 1: bootstrap only. The session cap is opened at connect
-- time so handlers can route session-level RPC through it once
-- later phases add operations. The Session cap is thread-safe
-- ('callP' goes through STM), so concurrent daemon handlers can
-- issue calls without extra locking.
--
-- The underlying TCP connection is held by a single 'withConn'
-- bracket on the daemon's main thread — see
-- 'withNodeAgentClient' and the 'runNodeAgentConnection' async
-- in @app/daemon/Main.hs@.
module Corvus.NodeAgentClient
  ( -- * Client handle
    NodeAgentClient (..)
  , NodeAgentError (..)

    -- * Lifecycle
  , withNodeAgentClient
  , defaultNodeAgentAddress

    -- * Liveness / negotiation
  , ping
  , sessionPing
  , agentVersion

    -- * Result types (mirrors of agent-side ADTs)
  , DiskOpResult (..)
  , DiskOpKind (..)
  , DiskInspectInfo (..)
  , DiskSnapshotInfo (..)

    -- * Disk image operations
  , diskCreate
  , diskCreateOverlay
  , diskDelete
  , diskResize
  , diskRebase
  , diskClone
  , diskInspect

    -- * Snapshot operations
  , snapshotCreate
  , snapshotDelete
  , snapshotRollback

    -- * Download / hash / decompress
  , diskDownload
  , diskDecompressXz
  , diskMd5

    -- * Cloud-init
  , cloudInitGenerateIso

    -- * Process supervision
  , ProcessStopResult (..)
  , VirtiofsdSpawnResult (..)
  , processSpawnQemu
  , processSpawnVirtiofsd
  , processStop
  , processIsAlive

    -- * VM abstraction (replaces process-supervision RPCs)
  , VmSpec (..)
  , VmDriveSpec (..)
  , VmNetIfSpec (..)
  , VmSharedDirSpec (..)
  , VmRuntimeInfo (..)
  , VmStopResult (..)
  , VmStopKind (..)
  , VmAgentStatus (..)
  , VmAgentState (..)
  , VmGuestExecReq (..)
  , VmGuestExecInfo (..)
  , vmStart
  , vmStopGraceful
  , vmStopHard
  , vmPause
  , vmResume
  , vmGuestExec
  , vmStatus
  , vmSetSpiceTicket
  , subscribeVmStatus
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc
  ( ConnConfig (..)
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import qualified Control.Exception as E
import qualified Data.Default as Def
import Data.Function ((&))
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Data.Word (Word32)
import qualified Network.Socket as NS
import Supervisors (Supervisor, withSupervisor)

import Corvus.Node.VmSpec
  ( VmAgentState (..)
  , VmAgentStatus (..)
  , VmDriveSpec (..)
  , VmGuestExecInfo (..)
  , VmGuestExecReq (..)
  , VmNetIfSpec (..)
  , VmRuntimeInfo (..)
  , VmSharedDirSpec (..)
  , VmSpec (..)
  , VmStopKind (..)
  , VmStopResult (..)
  )

-- ---------------------------------------------------------------------------
-- Client handle + lifecycle

-- | The daemon-side handle for the node agent. Holds the
-- bootstrap @NodeAgent@ cap (for ping/version/session) and the
-- already-opened @Session@ cap (used by every operation once
-- session-level methods land in later phases).
data NodeAgentClient = NodeAgentClient
  { nacAgent :: !(C.Client CGNA.NodeAgent)
  , nacSession :: !(C.Client CGNA.Session)
  , nacSupervisor :: !Supervisor
  , nacOwner :: !T.Text
  }

data NodeAgentError
  = NodeAgentConnectFailed !T.Text
  | NodeAgentRemoteError !T.Text
  deriving (Show)

instance E.Exception NodeAgentError

defaultNodeAgentAddress :: (String, Int)
defaultNodeAgentAddress = ("127.0.0.1", 9878)

withNodeAgentClient
  :: String
  -> Int
  -> T.Text
  -- ^ owner tag (typically the daemon uid as text)
  -> (Either NodeAgentError NodeAgentClient -> IO a)
  -> IO a
withNodeAgentClient host port owner body = do
  sockResult <- E.try @E.SomeException (openTcp host port)
  case sockResult of
    Left e ->
      body (Left (NodeAgentConnectFailed (T.pack (show e))))
    Right sock ->
      E.bracket (pure sock) NS.close $ \_ -> runOnSocket sock
  where
    runOnSocket sock = do
      let transport = socketTransport sock C.defaultLimit
          cfg = Def.def {debugMode = False}
      r <-
        E.try @E.SomeException $
          withSupervisor $ \sup ->
            withConn transport cfg $ \conn -> do
              rawAgent <- requestBootstrap conn
              let agent :: C.Client CGNA.NodeAgent
                  agent = fromClient rawAgent
              CGNA.NodeAgent'session'results {CGNA.session = sess} <-
                callOn
                  #session
                  CGNA.NodeAgent'session'params {CGNA.owner = owner}
                  agent
              body $
                Right
                  NodeAgentClient
                    { nacAgent = agent
                    , nacSession = sess
                    , nacSupervisor = sup
                    , nacOwner = owner
                    }
      case r of
        Left (e :: E.SomeException) ->
          body (Left (NodeAgentConnectFailed (T.pack (show e))))
        Right out -> pure out

-- ---------------------------------------------------------------------------
-- Liveness

ping :: NodeAgentClient -> IO (Either NodeAgentError ())
ping nac = remote $ do
  _ :: C.Parsed CGNA.NodeAgent'ping'results <-
    callOn #ping CGNA.NodeAgent'ping'params (nacAgent nac)
  pure ()

sessionPing :: NodeAgentClient -> IO (Either NodeAgentError ())
sessionPing nac = remote $ do
  _ :: C.Parsed CGNA.Session'ping'results <-
    callOn #ping CGNA.Session'ping'params (nacSession nac)
  pure ()

agentVersion :: NodeAgentClient -> IO (Either NodeAgentError (T.Text, [T.Text]))
agentVersion nac = remote $ do
  CGNA.NodeAgent'version'results {CGNA.info = info_} <-
    callOn #version CGNA.NodeAgent'version'params (nacAgent nac)
  let CGNA.AgentInfo {CGNA.semver = sv, CGNA.capabilities = caps} = info_
  pure (sv, caps)

-- ---------------------------------------------------------------------------
-- Internals

openTcp :: String -> Int -> IO NS.Socket
openTcp host port = do
  ais <- NS.getAddrInfo Nothing (Just host) (Just (show port))
  case ais of
    (ai : _) -> do
      sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress ai)
      pure sock
    [] -> E.throwIO (userError ("no addrinfo for " <> host))

remote :: IO a -> IO (Either NodeAgentError a)
remote action = do
  r <- E.try @E.SomeException action
  case r of
    Right a -> pure (Right a)
    Left e -> pure (Left (NodeAgentRemoteError (T.pack (show e))))

callOn
  :: forall iface params results
   . ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO (C.Parsed results)
callOn method p client = do
  raw <- (client & C.callP method p) >>= C.waitPipeline
  C.evalLimitT C.defaultLimit (C.parse raw)

-- ---------------------------------------------------------------------------
-- Result types — mirror DiskOpResult / DiskInspectInfo on the wire so the
-- daemon can pattern-match without parsing error strings.

-- | Result kind matching the agent-side @ImageResult@ ADT.
data DiskOpKind
  = DiskOpSuccess
  | DiskOpError !T.Text
  | DiskOpNotFound
  | DiskOpFormatUnsupported !T.Text
  deriving (Eq, Show)

newtype DiskOpResult = DiskOpResult
  { dorKind :: DiskOpKind
  }
  deriving (Eq, Show)

data DiskSnapshotInfo = DiskSnapshotInfo
  { dsiId :: !T.Text
  , dsiName :: !T.Text
  , dsiSizeMb :: !(Maybe Int64)
  }
  deriving (Eq, Show)

data DiskInspectInfo = DiskInspectInfo
  { diiFormat :: !T.Text
  , diiVirtualSizeMb :: !Int64
  , diiActualSizeMb :: !(Maybe Int64)
  , diiSnapshots :: ![DiskSnapshotInfo]
  }
  deriving (Eq, Show)

decodeDiskOpResult :: C.Parsed CGNA.DiskOpResult -> DiskOpResult
decodeDiskOpResult CGNA.DiskOpResult {CGNA.kind = k, CGNA.message = m} =
  DiskOpResult $ case k of
    CGNA.DiskOpKind'success -> DiskOpSuccess
    CGNA.DiskOpKind'errorGeneric -> DiskOpError m
    CGNA.DiskOpKind'errorNotFound -> DiskOpNotFound
    CGNA.DiskOpKind'errorFormatUnsupported -> DiskOpFormatUnsupported m
    CGNA.DiskOpKind'unknown' _ ->
      DiskOpError ("unknown DiskOpKind: " <> m)

decodeDiskSnapshotInfo :: C.Parsed CGNA.DiskSnapshotInfo -> DiskSnapshotInfo
decodeDiskSnapshotInfo
  CGNA.DiskSnapshotInfo
    { CGNA.id = i
    , CGNA.name = n
    , CGNA.sizeMb = sz
    , CGNA.hasSize = hs
    } =
    DiskSnapshotInfo
      { dsiId = i
      , dsiName = n
      , dsiSizeMb = if hs then Just sz else Nothing
      }

decodeDiskInspectInfo :: C.Parsed CGNA.DiskInspectInfo -> DiskInspectInfo
decodeDiskInspectInfo
  CGNA.DiskInspectInfo
    { CGNA.format = fmt
    , CGNA.virtualSizeMb = vs
    , CGNA.actualSizeMb = ac
    , CGNA.hasActualSize = ha
    , CGNA.snapshots = ss
    } =
    DiskInspectInfo
      { diiFormat = fmt
      , diiVirtualSizeMb = vs
      , diiActualSizeMb = if ha then Just ac else Nothing
      , diiSnapshots = map decodeDiskSnapshotInfo ss
      }

-- ---------------------------------------------------------------------------
-- Disk image operations

diskCreate
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> Int64
  -> IO (Either NodeAgentError DiskOpResult)
diskCreate nac path format sizeMb = remote $ do
  CGNA.Session'diskCreate'results {CGNA.result = r} <-
    callOn
      #diskCreate
      CGNA.Session'diskCreate'params
        { CGNA.path = path
        , CGNA.format = format
        , CGNA.sizeMb = sizeMb
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskCreateOverlay
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
diskCreateOverlay nac overlayPath backingPath backingFormat = remote $ do
  CGNA.Session'diskCreateOverlay'results {CGNA.result = r} <-
    callOn
      #diskCreateOverlay
      CGNA.Session'diskCreateOverlay'params
        { CGNA.overlayPath = overlayPath
        , CGNA.backingPath = backingPath
        , CGNA.backingFormat = backingFormat
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskDelete :: NodeAgentClient -> T.Text -> IO (Either NodeAgentError DiskOpResult)
diskDelete nac path = remote $ do
  CGNA.Session'diskDelete'results {CGNA.result = r} <-
    callOn
      #diskDelete
      CGNA.Session'diskDelete'params {CGNA.path = path}
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskResize
  :: NodeAgentClient
  -> T.Text
  -> Int64
  -> IO (Either NodeAgentError DiskOpResult)
diskResize nac path newSizeMb = remote $ do
  CGNA.Session'diskResize'results {CGNA.result = r} <-
    callOn
      #diskResize
      CGNA.Session'diskResize'params
        { CGNA.path = path
        , CGNA.newSizeMb = newSizeMb
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

-- | Rebase an overlay onto a new backing image (or flatten if
-- 'Nothing'). Mirrors 'Corvus.Node.Image.rebaseImage'.
diskRebase
  :: NodeAgentClient
  -> T.Text
  -> Maybe (T.Text, T.Text)
  -- ^ @Just (newBackingPath, newBackingFormat)@ to rebase;
  -- @Nothing@ to flatten.
  -> Bool
  -- ^ Unsafe (@-u@) mode.
  -> IO (Either NodeAgentError DiskOpResult)
diskRebase nac overlayPath mNewBacking unsafeUpdate = remote $ do
  let (newBacking, newBackingFormat, hasNewBacking) = case mNewBacking of
        Just (p, f) -> (p, f, True)
        Nothing -> ("", "", False)
  CGNA.Session'diskRebase'results {CGNA.result = r} <-
    callOn
      #diskRebase
      CGNA.Session'diskRebase'params
        { CGNA.overlayPath = overlayPath
        , CGNA.newBacking = newBacking
        , CGNA.newBackingFormat = newBackingFormat
        , CGNA.hasNewBacking = hasNewBacking
        , CGNA.unsafeUpdate = unsafeUpdate
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskClone
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
diskClone nac src dest = remote $ do
  CGNA.Session'diskClone'results {CGNA.result = r} <-
    callOn
      #diskClone
      CGNA.Session'diskClone'params
        { CGNA.sourcePath = src
        , CGNA.destPath = dest
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskInspect
  :: NodeAgentClient
  -> T.Text
  -> IO (Either NodeAgentError DiskInspectInfo)
diskInspect nac path = remote $ do
  CGNA.Session'diskInspect'results {CGNA.info = i} <-
    callOn
      #diskInspect
      CGNA.Session'diskInspect'params {CGNA.path = path}
      (nacSession nac)
  pure (decodeDiskInspectInfo i)

-- ---------------------------------------------------------------------------
-- Snapshot operations

snapshotCreate
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
snapshotCreate nac path name = remote $ do
  CGNA.Session'snapshotCreate'results {CGNA.result = r} <-
    callOn
      #snapshotCreate
      CGNA.Session'snapshotCreate'params {CGNA.path = path, CGNA.name = name}
      (nacSession nac)
  pure (decodeDiskOpResult r)

snapshotDelete
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
snapshotDelete nac path name = remote $ do
  CGNA.Session'snapshotDelete'results {CGNA.result = r} <-
    callOn
      #snapshotDelete
      CGNA.Session'snapshotDelete'params {CGNA.path = path, CGNA.name = name}
      (nacSession nac)
  pure (decodeDiskOpResult r)

snapshotRollback
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
snapshotRollback nac path name = remote $ do
  CGNA.Session'snapshotRollback'results {CGNA.result = r} <-
    callOn
      #snapshotRollback
      CGNA.Session'snapshotRollback'params {CGNA.path = path, CGNA.name = name}
      (nacSession nac)
  pure (decodeDiskOpResult r)

-- ---------------------------------------------------------------------------
-- Download / decompress / hash

diskDownload
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
diskDownload nac destPath url = remote $ do
  CGNA.Session'diskDownload'results {CGNA.result = r} <-
    callOn
      #diskDownload
      CGNA.Session'diskDownload'params
        { CGNA.destPath = destPath
        , CGNA.url = url
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskDecompressXz
  :: NodeAgentClient
  -> T.Text
  -> IO (Either NodeAgentError T.Text)
diskDecompressXz nac xzPath = remote $ do
  CGNA.Session'diskDecompressXz'results {CGNA.finalPath = f} <-
    callOn
      #diskDecompressXz
      CGNA.Session'diskDecompressXz'params {CGNA.xzPath = xzPath}
      (nacSession nac)
  pure f

diskMd5 :: NodeAgentClient -> T.Text -> IO (Either NodeAgentError T.Text)
diskMd5 nac path = remote $ do
  CGNA.Session'diskMd5'results {CGNA.hex = h} <-
    callOn
      #diskMd5
      CGNA.Session'diskMd5'params {CGNA.path = path}
      (nacSession nac)
  pure h

-- ---------------------------------------------------------------------------
-- Cloud-init

cloudInitGenerateIso
  :: NodeAgentClient
  -> T.Text
  -- ^ Target directory; agent @mkdir -p@'s it.
  -> T.Text
  -- ^ Composed user-data text.
  -> T.Text
  -- ^ Composed meta-data text.
  -> Maybe T.Text
  -- ^ Optional network-config text.
  -> IO (Either NodeAgentError T.Text)
cloudInitGenerateIso nac targetDir userData metaData mNetworkConfig = remote $ do
  let (networkConfig, hasNetworkConfig) = case mNetworkConfig of
        Just nc -> (nc, True)
        Nothing -> ("", False)
  CGNA.Session'cloudInitGenerateIso'results {CGNA.isoPath = p} <-
    callOn
      #cloudInitGenerateIso
      CGNA.Session'cloudInitGenerateIso'params
        { CGNA.targetDir = targetDir
        , CGNA.userData = userData
        , CGNA.metaData = metaData
        , CGNA.networkConfig = networkConfig
        , CGNA.hasNetworkConfig = hasNetworkConfig
        }
      (nacSession nac)
  pure p

-- ---------------------------------------------------------------------------
-- Process supervision

-- | Daemon-side mirror of the agent's @ProcessStopResult@.
data ProcessStopResult
  = ProcessStoppedGracefully
  | ProcessStoppedByTerm
  | ProcessStoppedByKill
  | ProcessNotRunning
  | ProcessStopFailed !T.Text
  deriving (Eq, Show)

-- | Daemon-side mirror of the agent's @VirtiofsdSpawnResult@.
data VirtiofsdSpawnResult
  = VirtiofsdSpawned !Word32
  | VirtiofsdSpawnFailed !T.Text
  | VirtiofsdSocketTimeout !T.Text
  deriving (Eq, Show)

decodeProcessStopResult :: CGNA.Parsed CGNA.ProcessStopResult -> ProcessStopResult
decodeProcessStopResult CGNA.ProcessStopResult {CGNA.kind = k, CGNA.message = m} =
  case k of
    CGNA.ProcessStopKind'stoppedGracefully -> ProcessStoppedGracefully
    CGNA.ProcessStopKind'stoppedByTerm -> ProcessStoppedByTerm
    CGNA.ProcessStopKind'stoppedByKill -> ProcessStoppedByKill
    CGNA.ProcessStopKind'notRunning -> ProcessNotRunning
    CGNA.ProcessStopKind'stopFailed -> ProcessStopFailed m
    CGNA.ProcessStopKind'unknown' _ ->
      ProcessStopFailed ("unknown ProcessStopKind: " <> m)

decodeVirtiofsdSpawnResult
  :: CGNA.Parsed CGNA.VirtiofsdSpawnResult -> VirtiofsdSpawnResult
decodeVirtiofsdSpawnResult CGNA.VirtiofsdSpawnResult {CGNA.kind = k, CGNA.pid = p, CGNA.message = m} =
  case k of
    CGNA.VirtiofsdSpawnKind'success -> VirtiofsdSpawned (fromIntegral p)
    CGNA.VirtiofsdSpawnKind'spawnFailed -> VirtiofsdSpawnFailed m
    CGNA.VirtiofsdSpawnKind'socketNeverAppeared -> VirtiofsdSocketTimeout m
    CGNA.VirtiofsdSpawnKind'unknown' _ ->
      VirtiofsdSpawnFailed ("unknown VirtiofsdSpawnKind: " <> m)

processSpawnQemu
  :: NodeAgentClient
  -> Int64
  -- ^ VM id (used to compute the runtime directory).
  -> T.Text
  -- ^ QEMU binary path.
  -> [T.Text]
  -- ^ QEMU argv.
  -> IO (Either NodeAgentError Word32)
processSpawnQemu nac vmId binary args = remote $ do
  CGNA.Session'processSpawnQemu'results {CGNA.pid = pidI32} <-
    callOn
      #processSpawnQemu
      CGNA.Session'processSpawnQemu'params
        { CGNA.vmId = vmId
        , CGNA.binary = binary
        , CGNA.args = args
        }
      (nacSession nac)
  pure (fromIntegral pidI32)

processSpawnVirtiofsd
  :: NodeAgentClient
  -> T.Text
  -- ^ virtiofsd binary path.
  -> [T.Text]
  -- ^ virtiofsd argv.
  -> T.Text
  -- ^ Socket path the agent should poll for as a readiness signal.
  -> Word32
  -- ^ Timeout in milliseconds.
  -> IO (Either NodeAgentError VirtiofsdSpawnResult)
processSpawnVirtiofsd nac binary args socketPath waitMs = remote $ do
  CGNA.Session'processSpawnVirtiofsd'results {CGNA.result = r} <-
    callOn
      #processSpawnVirtiofsd
      CGNA.Session'processSpawnVirtiofsd'params
        { CGNA.binary = binary
        , CGNA.args = args
        , CGNA.socketPath = socketPath
        , CGNA.waitForSocketTimeoutMs = waitMs
        }
      (nacSession nac)
  pure (decodeVirtiofsdSpawnResult r)

processStop
  :: NodeAgentClient
  -> Word32
  -- ^ PID returned earlier from a spawn call.
  -> Word32
  -- ^ Seconds to wait after SIGTERM before escalating to SIGKILL.
  -> IO (Either NodeAgentError ProcessStopResult)
processStop nac pid graceSec = remote $ do
  CGNA.Session'processStop'results {CGNA.result = r} <-
    callOn
      #processStop
      CGNA.Session'processStop'params
        { CGNA.pid = fromIntegral pid :: Int32
        , CGNA.gracefulTimeoutSec = graceSec
        }
      (nacSession nac)
  pure (decodeProcessStopResult r)

processIsAlive :: NodeAgentClient -> Word32 -> IO (Either NodeAgentError Bool)
processIsAlive nac pid = remote $ do
  CGNA.Session'processIsAlive'results {CGNA.alive = a} <-
    callOn
      #processIsAlive
      CGNA.Session'processIsAlive'params {CGNA.pid = fromIntegral pid :: Int32}
      (nacSession nac)
  pure a

-- ---------------------------------------------------------------------------
-- VM abstraction — wire types are defined in "Corvus.Node.VmSpec"
-- and re-exported above so the daemon-side client surface is
-- self-contained.

-- ---------------------------------------------------------------------------
-- Encoders / decoders for VM abstraction wire types.

encodeVmSpec :: VmSpec -> CGNA.Parsed CGNA.VmSpec
encodeVmSpec s =
  CGNA.VmSpec
    { CGNA.vmId = vsVmId s
    , CGNA.name = vsName s
    , CGNA.cpuCount = vsCpuCount s
    , CGNA.ramMb = vsRamMb s
    , CGNA.headless = vsHeadless s
    , CGNA.guestAgent = vsGuestAgent s
    , CGNA.vsockCid = fromMaybe 0 (vsVsockCid s)
    , CGNA.hasVsockCid = isJust (vsVsockCid s)
    , CGNA.spicePort = fromMaybe 0 (vsSpicePort s)
    , CGNA.hasSpicePort = isJust (vsSpicePort s)
    , CGNA.drives = map encodeVmDriveSpec (vsDrives s)
    , CGNA.netIfs = map encodeVmNetIfSpec (vsNetIfs s)
    , CGNA.sharedDirs = map encodeVmSharedDirSpec (vsSharedDirs s)
    , CGNA.waitForGuestAgentMs = vsWaitForGuestAgentMs s
    }

encodeVmDriveSpec :: VmDriveSpec -> CGNA.Parsed CGNA.VmDriveSpec
encodeVmDriveSpec d =
  CGNA.VmDriveSpec
    { CGNA.diskFilePath = vdsDiskFilePath d
    , CGNA.format = vdsFormat d
    , CGNA.ifKind = vdsIfKind d
    , CGNA.media = vdsMedia d
    , CGNA.readOnly = vdsReadOnly d
    , CGNA.cache = vdsCache d
    , CGNA.discard = vdsDiscard d
    }

encodeVmNetIfSpec :: VmNetIfSpec -> CGNA.Parsed CGNA.VmNetIfSpec
encodeVmNetIfSpec n =
  CGNA.VmNetIfSpec
    { CGNA.ifType = vnsIfType n
    , CGNA.hostDevice = vnsHostDevice n
    , CGNA.macAddress = vnsMacAddress n
    }

encodeVmSharedDirSpec :: VmSharedDirSpec -> CGNA.Parsed CGNA.VmSharedDirSpec
encodeVmSharedDirSpec s =
  CGNA.VmSharedDirSpec
    { CGNA.hostPath = vssHostPath s
    , CGNA.tag = vssTag s
    , CGNA.cache = vssCache s
    , CGNA.readOnly = vssReadOnly s
    }

decodeVmRuntimeInfo :: CGNA.Parsed CGNA.VmRuntimeInfo -> VmRuntimeInfo
decodeVmRuntimeInfo
  CGNA.VmRuntimeInfo
    { CGNA.qemuPid = q
    , CGNA.virtiofsdPids = vs
    , CGNA.spicePort = sp
    } =
    VmRuntimeInfo
      { vriQemuPid = q
      , vriVirtiofsdPids = vs
      , vriSpicePort = sp
      }

decodeVmStopResult :: CGNA.Parsed CGNA.VmStopResult -> VmStopResult
decodeVmStopResult CGNA.VmStopResult {CGNA.kind = k, CGNA.message = m} =
  VmStopResult
    { vsrKind = case k of
        CGNA.VmStopKind'stopped -> VmStopStopped
        CGNA.VmStopKind'alreadyStopped -> VmStopAlreadyStopped
        CGNA.VmStopKind'timeout -> VmStopTimeout
        CGNA.VmStopKind'failed -> VmStopFailed
        CGNA.VmStopKind'unknown' _ -> VmStopFailed
    , vsrMessage = m
    }

decodeVmAgentStatus :: CGNA.Parsed CGNA.VmAgentStatus -> VmAgentStatus
decodeVmAgentStatus
  CGNA.VmAgentStatus
    { CGNA.state = s
    , CGNA.qemuPid = q
    , CGNA.lastExitCode = e
    } =
    VmAgentStatus
      { vasState = case s of
          CGNA.VmAgentState'running -> VmAgentRunning
          CGNA.VmAgentState'stopped -> VmAgentStopped
          CGNA.VmAgentState'errored -> VmAgentErrored
          CGNA.VmAgentState'unknown -> VmAgentUnknown
          CGNA.VmAgentState'unknown' _ -> VmAgentUnknown
      , vasQemuPid = q
      , vasLastExitCode = e
      }

encodeVmGuestExecReq :: VmGuestExecReq -> CGNA.Parsed CGNA.VmGuestExecReq
encodeVmGuestExecReq r =
  CGNA.VmGuestExecReq
    { CGNA.vmId = vgeVmId r
    , CGNA.path = vgePath r
    , CGNA.args = vgeArgs r
    , CGNA.captureOutput = vgeCaptureOutput r
    , CGNA.inputData = vgeInputData r
    , CGNA.timeoutSec = vgeTimeoutSec r
    }

decodeVmGuestExecInfo :: CGNA.Parsed CGNA.VmGuestExecInfo -> VmGuestExecInfo
decodeVmGuestExecInfo
  CGNA.VmGuestExecInfo
    { CGNA.exitCode = c
    , CGNA.hasExit = h
    , CGNA.signal = sg
    , CGNA.stdout = so
    , CGNA.stderr = se
    } =
    VmGuestExecInfo
      { vgiExitCode = c
      , vgiHasExit = h
      , vgiSignal = sg
      , vgiStdout = so
      , vgiStderr = se
      }

-- ---------------------------------------------------------------------------
-- VM abstraction — client wrappers.

vmStart :: NodeAgentClient -> VmSpec -> IO (Either NodeAgentError VmRuntimeInfo)
vmStart nac spec = remote $ do
  CGNA.Session'vmStart'results {CGNA.info = i} <-
    callOn
      #vmStart
      CGNA.Session'vmStart'params {CGNA.spec = encodeVmSpec spec}
      (nacSession nac)
  pure (decodeVmRuntimeInfo i)

vmStopGraceful
  :: NodeAgentClient
  -> Int64
  -> Word32
  -> IO (Either NodeAgentError VmStopResult)
vmStopGraceful nac vmId timeoutSec = remote $ do
  CGNA.Session'vmStopGraceful'results {CGNA.result = r} <-
    callOn
      #vmStopGraceful
      CGNA.Session'vmStopGraceful'params
        { CGNA.vmId = vmId
        , CGNA.timeoutSec = timeoutSec
        }
      (nacSession nac)
  pure (decodeVmStopResult r)

vmStopHard :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError VmStopResult)
vmStopHard nac vmId = remote $ do
  CGNA.Session'vmStopHard'results {CGNA.result = r} <-
    callOn
      #vmStopHard
      CGNA.Session'vmStopHard'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure (decodeVmStopResult r)

vmPause :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
vmPause nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'vmPause'results <-
    callOn
      #vmPause
      CGNA.Session'vmPause'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

vmResume :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
vmResume nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'vmResume'results <-
    callOn
      #vmResume
      CGNA.Session'vmResume'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

vmGuestExec
  :: NodeAgentClient -> VmGuestExecReq -> IO (Either NodeAgentError VmGuestExecInfo)
vmGuestExec nac r = remote $ do
  CGNA.Session'vmGuestExec'results {CGNA.info = i} <-
    callOn
      #vmGuestExec
      CGNA.Session'vmGuestExec'params {CGNA.req = encodeVmGuestExecReq r}
      (nacSession nac)
  pure (decodeVmGuestExecInfo i)

vmStatus :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError VmAgentStatus)
vmStatus nac vmId = remote $ do
  CGNA.Session'vmStatus'results {CGNA.status = s} <-
    callOn
      #vmStatus
      CGNA.Session'vmStatus'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure (decodeVmAgentStatus s)

vmSetSpiceTicket
  :: NodeAgentClient -> Int64 -> T.Text -> Word32 -> IO (Either NodeAgentError ())
vmSetSpiceTicket nac vmId password ttlSeconds = remote $ do
  _ :: C.Parsed CGNA.Session'vmSetSpiceTicket'results <-
    callOn
      #vmSetSpiceTicket
      CGNA.Session'vmSetSpiceTicket'params
        { CGNA.vmId = vmId
        , CGNA.password = password
        , CGNA.ttlSeconds = ttlSeconds
        }
      (nacSession nac)
  pure ()

-- | Register a 'VmStatusSink' with the agent. The agent retains
-- a reference and pushes a 'VmStatusSnapshot' to it every ~10 s
-- until the sink throws (at which point it is pruned). One
-- registration per agent connection is sufficient — the daemon
-- calls this once from its on-connect callback.
subscribeVmStatus
  :: NodeAgentClient
  -> C.Client CGNA.VmStatusSink
  -> IO (Either NodeAgentError ())
subscribeVmStatus nac sink = remote $ do
  _ :: C.Parsed CGNA.Session'subscribeVmStatus'results <-
    callOn
      #subscribeVmStatus
      CGNA.Session'subscribeVmStatus'params {CGNA.sink = sink}
      (nacSession nac)
  pure ()
