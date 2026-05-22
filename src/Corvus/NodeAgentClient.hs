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

    -- * VM abstraction
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
  , vmGuestExecStream
  , vmStatus
  , vmSetSpiceTicket
  , subscribeVmStatus

    -- * Chardev streaming
  , openSerialConsole
  , openHmpMonitor
  , flushSerialConsole
  , flushHmpMonitor

    -- * QMP-mediated runtime changes
  , vmAttachDrive
  , vmDetachDrive
  , probeVsockCid

    -- * Inter-agent disk transfer
  , DiskOpenReadResult (..)
  , diskOpenRead
  , attachReader
  , diskReaderPipeInto
  , diskImportFromPeer
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc
  ( ConnConfig (..)
  , Transport
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import qualified Control.Exception as E
import qualified Corvus.Tls as Tls
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
  -> Maybe Tls.TlsConfig
  -- ^ When 'Just', wrap the TCP socket with mTLS and validate
  -- the peer's CN before issuing the bootstrap call. The
  -- caller is expected to have already specialised the config
  -- with the peer node's expected CN suffix via
  -- 'Tls.withPeerExpectation'.
  -> (Either NodeAgentError NodeAgentClient -> IO a)
  -> IO a
withNodeAgentClient host port owner mTlsCfg body = do
  sockResult <- E.try @E.SomeException (openTcp host port)
  case sockResult of
    Left e ->
      body (Left (NodeAgentConnectFailed (T.pack (show e))))
    Right sock ->
      E.bracket (pure sock) NS.close $ \_ -> runOnSocket sock
  where
    runOnSocket sock = do
      eTransport <- buildTransport mTlsCfg sock
      case eTransport of
        Left err -> body (Left (NodeAgentConnectFailed err))
        Right (transport, cleanup) ->
          (`E.finally` cleanup) $ do
            -- A streaming build provisioner can hold one
            -- outstanding 'vmGuestExecStream' for tens of minutes
            -- AND the agent is pushing tens of writes/sec on its
            -- LineBufferSink during that window. The default
            -- 128-question / 32 MiB-word budget is comfortable
            -- but small under burst loads (emerge spewing MB/s
            -- of output): exhaustion triggers STM retries inside
            -- haskell-capnp and, in pathological cases,
            -- 'BlockedIndefinitelyOnSTM'. Quadruple the budget
            -- for headroom; the memory cost is negligible vs the
            -- failure mode.
            let cfg =
                  Def.def
                    { debugMode = False
                    , maxQuestions = 4096
                    , maxCallWords = 128 * 1024 * 1024 `div` 8
                    }
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

-- | Build a Cap'n Proto 'Transport' over the connected socket,
-- TLS-wrapped when 'Just' was passed. Returns a teardown action
-- the caller runs when the transport is no longer in use.
buildTransport
  :: Maybe Tls.TlsConfig
  -> NS.Socket
  -> IO (Either T.Text (Transport, IO ()))
buildTransport Nothing sock =
  pure (Right (socketTransport sock C.defaultLimit, pure ()))
buildTransport (Just cfg) sock = do
  r <- E.try @E.SomeException (Tls.wrapClientSocket cfg sock)
  case r of
    Left e -> pure (Left (T.pack ("TLS handshake failed: " <> show e)))
    Right (ctx, ref) -> do
      v <- Tls.validatePeerCN cfg ref
      case v of
        Left msg -> do
          Tls.closeTlsContext ctx
          pure (Left ("TLS peer rejected: " <> msg))
        Right () -> do
          transport <- Tls.tlsTransport ctx C.defaultLimit
          pure (Right (transport, Tls.closeTlsContext ctx))

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
    , CGNA.rebootQuirk = vsRebootQuirk s
    , CGNA.spiceBindAddr = vsSpiceBindAddr s
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

-- | Streaming variant of 'vmGuestExec'. The agent pushes
-- incremental stdout / stderr bytes through the caller-supplied
-- sinks while the guest process runs; the returned
-- 'VmGuestExecInfo' carries only the exit code. The caller MUST
-- export the sinks BEFORE this call (they must be live for the
-- duration of the exec) and end-of-stream signalling arrives via
-- the agent calling @sink.end()@ on completion.
vmGuestExecStream
  :: NodeAgentClient
  -> VmGuestExecReq
  -> C.Client CGS.ByteSink
  -- ^ stdout sink (exported by the caller)
  -> C.Client CGS.ByteSink
  -- ^ stderr sink (exported by the caller)
  -> IO (Either NodeAgentError VmGuestExecInfo)
vmGuestExecStream nac r stdoutSink stderrSink = remote $ do
  CGNA.Session'vmGuestExecStream'results {CGNA.info = i} <-
    callOn
      #vmGuestExecStream
      CGNA.Session'vmGuestExecStream'params
        { CGNA.req = encodeVmGuestExecReq r
        , CGNA.stdoutSink = stdoutSink
        , CGNA.stderrSink = stderrSink
        }
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

-- ---------------------------------------------------------------------------
-- Chardev streaming
--
-- Each call hands the agent a 'ByteSink' cap to write into (the
-- ring-buffer's replay + live output goes here) and receives back
-- another 'ByteSink' the caller can write to in order to forward
-- bytes into QEMU's chardev (keystrokes / HMP commands).
--
-- The daemon proxies these on behalf of CLI clients today; a
-- future direct-to-client variant can be added without changing
-- this wire shape.

openSerialConsole
  :: NodeAgentClient
  -> Int64
  -> C.Client CGS.ByteSink
  -> IO (Either NodeAgentError (C.Client CGS.ByteSink))
openSerialConsole nac vmId sink = remote $ do
  CGNA.Session'openSerialConsole'results {CGNA.input = inp} <-
    callOn
      #openSerialConsole
      CGNA.Session'openSerialConsole'params
        { CGNA.vmId = vmId
        , CGNA.sink = sink
        }
      (nacSession nac)
  pure inp

openHmpMonitor
  :: NodeAgentClient
  -> Int64
  -> C.Client CGS.ByteSink
  -> IO (Either NodeAgentError (C.Client CGS.ByteSink))
openHmpMonitor nac vmId sink = remote $ do
  CGNA.Session'openHmpMonitor'results {CGNA.input = inp} <-
    callOn
      #openHmpMonitor
      CGNA.Session'openHmpMonitor'params
        { CGNA.vmId = vmId
        , CGNA.sink = sink
        }
      (nacSession nac)
  pure inp

flushSerialConsole
  :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
flushSerialConsole nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'flushSerialConsole'results <-
    callOn
      #flushSerialConsole
      CGNA.Session'flushSerialConsole'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

flushHmpMonitor
  :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
flushHmpMonitor nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'flushHmpMonitor'results <-
    callOn
      #flushHmpMonitor
      CGNA.Session'flushHmpMonitor'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

-- ---------------------------------------------------------------------------
-- QMP-mediated runtime changes

-- | Hot-attach a drive via QMP @blockdev-add@ + @device_add@.
-- @driveId@ is the daemon's drive table key; the agent derives
-- QEMU's @node-name@ (@drive-N@) and @id@ (@device-N@) from it.
-- The wire path returns @()@; QMP failures arrive as remote
-- exceptions and surface as 'NodeAgentRemoteError'.
vmAttachDrive
  :: NodeAgentClient
  -> Int64
  -- ^ vmId
  -> Int64
  -- ^ driveId (DB key)
  -> T.Text
  -- ^ resolved disk file path
  -> T.Text
  -- ^ disk format (@"qcow2"@ / @"raw"@ / …)
  -> T.Text
  -- ^ drive interface (@"virtio"@ / @"ide"@ / …)
  -> Bool
  -- ^ read-only
  -> IO (Either NodeAgentError ())
vmAttachDrive nac vmId driveId filePath fmt ifKind ro = remote $ do
  let req =
        CGNA.VmAttachDriveReq
          { CGNA.vmId = vmId
          , CGNA.driveId = driveId
          , CGNA.filePath = filePath
          , CGNA.format = fmt
          , CGNA.ifKind = ifKind
          , CGNA.readOnly = ro
          }
  _ :: C.Parsed CGNA.Session'vmAttachDrive'results <-
    callOn
      #vmAttachDrive
      CGNA.Session'vmAttachDrive'params {CGNA.req = req}
      (nacSession nac)
  pure ()

-- | Hot-detach a drive via QMP @device_del@ + @blockdev-del@
-- (the agent handles the busy-retry).
vmDetachDrive :: NodeAgentClient -> Int64 -> Int64 -> IO (Either NodeAgentError ())
vmDetachDrive nac vmId driveId = remote $ do
  _ :: C.Parsed CGNA.Session'vmDetachDrive'results <-
    callOn
      #vmDetachDrive
      CGNA.Session'vmDetachDrive'params
        { CGNA.vmId = vmId
        , CGNA.driveId = driveId
        }
      (nacSession nac)
  pure ()

-- | Probe whether the given AF_VSOCK CID is currently free on
-- the agent's host kernel. Returns 'True' when the kernel
-- reports the CID as available (or when the agent's host has
-- no vhost-vsock device at all).
probeVsockCid :: NodeAgentClient -> Int -> IO (Either NodeAgentError Bool)
probeVsockCid nac cid = remote $ do
  CGNA.Session'probeVsockCid'results {CGNA.free = free} <-
    callOn
      #probeVsockCid
      CGNA.Session'probeVsockCid'params {CGNA.cid = fromIntegral cid}
      (nacSession nac)
  pure free

-- ---------------------------------------------------------------------------
-- Inter-agent disk transfer

-- | Result of 'diskOpenRead': a 'DiskReader' cap, the token the
-- destination presents to @attachReader@ on its own session, and
-- the source-side size + md5 the destination cross-checks the
-- received bytes against.
data DiskOpenReadResult = DiskOpenReadResult
  { dorReader :: !(C.Client CGNA.DiskReader)
  , dorToken :: !T.Text
  , dorSizeBytes :: !Int64
  , dorMd5 :: !T.Text
  }

-- | Open a file on the source agent for inter-agent transfer.
diskOpenRead
  :: NodeAgentClient
  -> T.Text
  -- ^ source-side absolute path
  -> Word32
  -- ^ token TTL in seconds; the source agent evicts the entry
  -- after this many seconds if @attachReader@ never claims it.
  -> IO (Either NodeAgentError DiskOpenReadResult)
diskOpenRead nac path ttlSec = remote $ do
  CGNA.Session'diskOpenRead'results
    { CGNA.reader = reader
    , CGNA.token = token
    , CGNA.sizeBytes = sz
    , CGNA.md5 = md5
    } <-
    callOn
      #diskOpenRead
      CGNA.Session'diskOpenRead'params
        { CGNA.path = path
        , CGNA.ttlSec = ttlSec
        }
      (nacSession nac)
  pure $
    DiskOpenReadResult
      { dorReader = reader
      , dorToken = token
      , dorSizeBytes = sz
      , dorMd5 = md5
      }

-- | Single-use re-resolve. Typically called by the destination
-- agent on its own session to claim the same 'DiskReader' the
-- daemon obtained from 'diskOpenRead'.
attachReader
  :: NodeAgentClient
  -> T.Text
  -> IO (Either NodeAgentError (C.Client CGNA.DiskReader))
attachReader nac token = remote $ do
  CGNA.Session'attachReader'results {CGNA.reader = reader} <-
    callOn
      #attachReader
      CGNA.Session'attachReader'params {CGNA.token = token}
      (nacSession nac)
  pure reader

-- | Drive a 'DiskReader' to stream into a caller-supplied
-- 'ByteSink'. Blocks until @sink.end@ is delivered (or until the
-- source raises, in which case the RPC returns with that error).
diskReaderPipeInto
  :: C.Client CGNA.DiskReader
  -> C.Client CGS.ByteSink
  -> IO (Either NodeAgentError ())
diskReaderPipeInto reader sink = remote $ do
  _ :: C.Parsed CGNA.DiskReader'pipeInto'results <-
    callOn
      #pipeInto
      CGNA.DiskReader'pipeInto'params {CGNA.sink = sink}
      reader
  pure ()

-- | Ask a destination agent to pull a file from a peer source
-- agent. Resolves @peerHost:peerPort@, opens a fresh session,
-- claims the reader via @attachReader(token)@, then runs
-- @reader.pipeInto(localSink)@ where @localSink@ writes to
-- @destPath.part@ on the destination. On clean completion the
-- destination renames @.part@ to @destPath@ after verifying
-- @expectedBytes@ + @expectedMd5@.
diskImportFromPeer
  :: NodeAgentClient
  -> T.Text
  -- ^ destination path
  -> T.Text
  -- ^ peer host (source's @nodeHost@)
  -> Int32
  -- ^ peer port (source's @nodeAgentPort@)
  -> T.Text
  -- ^ token issued by 'diskOpenRead'
  -> Int64
  -- ^ expected size in bytes
  -> T.Text
  -- ^ expected md5 hex hash
  -> IO (Either NodeAgentError ())
diskImportFromPeer nac destPath peerHost peerPort token expectedBytes expectedMd5 = remote $ do
  _ :: C.Parsed CGNA.Session'diskImportFromPeer'results <-
    callOn
      #diskImportFromPeer
      CGNA.Session'diskImportFromPeer'params
        { CGNA.destPath = destPath
        , CGNA.peerHost = peerHost
        , CGNA.peerPort = peerPort
        , CGNA.token = token
        , CGNA.expectedBytes = expectedBytes
        , CGNA.expectedMd5 = expectedMd5
        }
      (nacSession nac)
  pure ()
