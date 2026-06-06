{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Command execution for the Corvus client.
-- This module dispatches CLI commands to domain-specific handler modules.
module Corvus.Client.Commands
  ( -- * Command execution
    runCommand

    -- * Re-exported formatters
  , formatUptime
  , vmColumns
  , printVmDetails
  , diskColumns
  , snapshotColumns
  , sshKeyColumns
  , templateVmColumns
  , printTemplateDetails

    -- * SPICE view helpers (exposed for tests)
  , resolveGrantHost
  , grantToJson
  )
where

import qualified Capnp.Gen.Enums as CGEnums
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Corvus.Client.Capnp.Connection (CapnpConnection, withCapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Commands.Apply
import Corvus.Client.Commands.Build
import Corvus.Client.Commands.CloudInit
import Corvus.Client.Commands.Disk
import Corvus.Client.Commands.GuestExec
import Corvus.Client.Commands.NetIf
import Corvus.Client.Commands.Network
import qualified Corvus.Client.Commands.Node as CmdNode
import Corvus.Client.Commands.SharedDir
import Corvus.Client.Commands.SshKey
import Corvus.Client.Commands.Task
import Corvus.Client.Commands.Template
import Corvus.Client.Commands.Vm
import Corvus.Client.Config (defaultClientConfig)
import Corvus.Client.Output
import Corvus.Client.Types
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (StatusInfo (..), VmDetails (..))
import qualified Corvus.Tls as Tls
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import Corvus.Wire.Common (ViewGrant (..), entityRefFromText)
import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Options.Applicative.BashCompletion (bashCompletionScript, fishCompletionScript, zshCompletionScript)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- | Get the listen address from options.
--
-- Defaults to TCP at the resolved host/port (see 'Options' /
-- 'Corvus.Client.Parser.readClientDefaults'). Switches to Unix
-- when '--unix' was passed OR when '--socket' / 'CORVUS_SOCKET'
-- gave an explicit socket path.
getListenAddress :: Options -> IO ListenAddress
getListenAddress opts
  | optUnix opts || isJust (optSocket opts) =
      case optSocket opts of
        Just path -> pure (UnixAddress path)
        Nothing -> UnixAddress <$> getDefaultSocketPath
  | otherwise = pure $ TcpAddress (optHost opts) (optPort opts)

-- | Execute the selected command
runCommand :: Options -> IO ()
runCommand opts = do
  let fmt = optOutput opts
      tableOpts = tableOptsFromOptions opts
  case optCommand opts of
    Completion shell -> handleCompletion shell
    _ -> pure ()
  addr <- getListenAddress opts
  tlsCfg <- resolveClientTls opts addr
  connResult <- withCapnpConnection addr tlsCfg $ \conn ->
    case optCommand opts of
      Ping -> do
        r <- try (CR.rpcPing conn) :: IO (Either SomeException ())
        case r of
          Right () -> do
            emitOk fmt $ putStrLn "pong"
            pure True
          Left e -> do
            emitError fmt "rpc_error" (T.pack (show e)) $
              putStrLn ("Error: " ++ show e)
            pure False
      Status -> do
        r <- try (CR.rpcStatus conn) :: IO (Either SomeException StatusInfo)
        case r of
          Right st@StatusInfo {..} -> do
            emitResult fmt st $ do
              putStrLn $ "Uptime:           " ++ formatUptime siUptime
              putStrLn $ "Connections:      " ++ show siConnections
              putStrLn $ "Version:          " ++ T.unpack siVersion
              putStrLn $ "Protocol version: " ++ show siProtocolVersion
            pure True
          Left e -> do
            emitError fmt "rpc_error" (T.pack (show e)) $
              putStrLn ("Error: " ++ show e)
            pure False
      Shutdown -> do
        r <- try (CR.rpcShutdown conn) :: IO (Either SomeException ())
        case r of
          Right () -> do
            emitOk fmt $ putStrLn "Shutdown acknowledged"
            pure True
          Left e -> do
            emitError fmt "rpc_error" (T.pack (show e)) $
              putStrLn ("Error: " ++ show e)
            pure False
      VmList -> do
        r <- try @SomeException (CR.rpcVmList conn)
        case r of
          Right vms -> do
            emitResult fmt vms $
              if null vms
                then putStrLn "No VMs found."
                else do
                  now <- getCurrentTime
                  printTable tableOpts (vmColumns now) vms
            pure True
          Left e -> do
            emitError fmt "rpc_error" (T.pack (show e)) $
              putStrLn ("Error: " ++ show e)
            pure False
      VmShow vmRef -> do
        r <- try (CR.rpcVmShow conn (entityRefFromText vmRef)) :: IO (Either SomeException VmDetails)
        case r of
          Right details -> do
            emitResult fmt details $ printVmDetails details
            pure True
          Left e -> do
            emitError fmt "rpc_error" (T.pack (show e)) $
              putStrLn ("Error: " ++ show e)
            pure False
      VmCreate name nodeRef cpuCount ramMb mDesc headless ga ci as rq cm ->
        handleVmCreate fmt conn name nodeRef cpuCount ramMb mDesc headless ga ci as rq cm
      VmDelete vmRef keepDisks -> handleVmDelete fmt conn vmRef keepDisks
      VmStart vmRef waitOpts -> handleVmStart fmt conn vmRef waitOpts
      VmStop vmRef waitOpts -> handleVmStop fmt conn vmRef waitOpts
      VmPause vmRef ->
        handleVmAction fmt "pause" vmRef (CR.rpcVmPause conn (entityRefFromText vmRef))
      VmReset vmRef ->
        handleVmAction fmt "reset" vmRef (CR.rpcVmReset conn (entityRefFromText vmRef))
      VmSave vmRef waitOpts -> handleVmSave fmt conn vmRef waitOpts
      VmEdit vmRef mCpus mRam mDesc mHeadless mGa mCi mAs mRq mCm ->
        handleVmEdit fmt conn vmRef mCpus mRam mDesc mHeadless mGa mCi mAs mRq mCm
      VmExec vmRef cmd -> handleVmExec fmt conn vmRef cmd
      VmView vmRef -> handleVmView opts fmt conn vmRef
      VmMonitor vmRef -> runHmpMonitorSession fmt conn vmRef
      VmMigrate vmRef toNodeRef -> handleVmMigrate fmt conn vmRef toNodeRef
      -- Disk commands
      DiskCreate name formatStr sizeMb mPath ephemeral nodeRef -> do
        case parseFormat formatStr of
          Left err -> do
            emitError fmt "invalid_format" err $ putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right format -> handleDiskCreate fmt conn name format sizeMb mPath ephemeral nodeRef
      DiskCreateOverlay name baseDiskRef optDirPath ephemeral -> handleDiskCreateOverlay fmt conn name baseDiskRef optDirPath ephemeral
      DiskRegisterCmd name path mFormatStr mBackingRef ephemeral nodeRef -> handleDiskRegister fmt conn name path mFormatStr mBackingRef ephemeral nodeRef
      DiskImport name source mPath mFormatStr ephemeral nodeRef waitOpts -> handleDiskImport fmt conn name source mPath mFormatStr ephemeral nodeRef waitOpts
      DiskRefresh diskRef -> handleDiskRefresh fmt conn diskRef
      DiskDelete diskRef -> handleDiskDelete fmt conn diskRef
      DiskResize diskRef newSizeMb -> handleDiskResize fmt conn diskRef newSizeMb
      DiskList -> handleDiskList fmt tableOpts conn
      DiskShow diskRef -> handleDiskShow fmt conn diskRef
      DiskClone name baseDiskRef optionalPath ephemeral -> handleDiskClone fmt conn name baseDiskRef optionalPath ephemeral
      DiskRebase diskRef mNewBacking unsafe -> handleDiskRebase fmt conn diskRef mNewBacking unsafe
      DiskAttach vmRef diskRef ifaceStr media readOnly discard cacheStr -> do
        case parseInterface ifaceStr of
          Left err -> do
            emitError fmt "invalid_interface" err $ putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right iface -> do
            case parseCacheType cacheStr of
              Left err -> do
                emitError fmt "invalid_cache_type" err $ putStrLn $ "Error: " ++ T.unpack err
                pure False
              Right cache -> do
                case media of
                  Nothing -> handleDiskAttach fmt conn vmRef diskRef iface Nothing readOnly discard cache
                  Just m -> case parseMedia m of
                    Left err -> do
                      emitError fmt "invalid_media" err $ putStrLn $ "Error: " ++ T.unpack err
                      pure False
                    Right parsedMedia -> handleDiskAttach fmt conn vmRef diskRef iface (Just parsedMedia) readOnly discard cache
      DiskDetach vmRef diskRef -> handleDiskDetach fmt conn vmRef diskRef
      DiskCopy diskRef toNodeRef mToPath withChain ->
        handleDiskCopy fmt conn diskRef toNodeRef mToPath withChain
      DiskMove diskRef toNodeRef mToPath withChain ->
        handleDiskMove fmt conn diskRef toNodeRef mToPath withChain
      -- Shared directory commands
      SharedDirAdd vmRef path tag cacheStr readOnly -> do
        case parseSharedDirCache cacheStr of
          Left err -> do
            emitError fmt "invalid_cache" err $ putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right cache -> handleSharedDirAdd fmt conn vmRef path tag cache readOnly
      SharedDirRemove vmRef sharedDirRef -> handleSharedDirRemove fmt conn vmRef sharedDirRef
      SharedDirList vmRef -> handleSharedDirList fmt tableOpts conn vmRef
      -- Network interface commands
      NetIfAdd vmRef ifaceTypeStr hostDevice macAddress mNetworkRef -> do
        case parseNetInterfaceType ifaceTypeStr of
          Left err -> do
            emitError fmt "invalid_interface_type" err $ putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right ifaceType -> handleNetIfAdd fmt conn vmRef ifaceType hostDevice macAddress mNetworkRef
      NetIfRemove vmRef netIfId -> handleNetIfRemove fmt conn vmRef netIfId
      NetIfList vmRef -> handleNetIfList fmt tableOpts conn vmRef
      -- Snapshot commands
      SnapshotCreate diskRef name quiesce ->
        handleSnapshotCreate fmt conn diskRef name (toCapnpQuiesce quiesce)
      SnapshotDelete diskRef snapshotRef -> handleSnapshotDelete fmt conn diskRef snapshotRef
      SnapshotRollback diskRef snapshotRef autoStop ->
        handleSnapshotRollback fmt conn diskRef snapshotRef autoStop
      SnapshotMerge diskRef snapshotRef -> handleSnapshotMerge fmt conn diskRef snapshotRef
      SnapshotList diskRef -> handleSnapshotList fmt tableOpts conn diskRef
      -- SSH key commands
      SshKeyCreate name publicKey -> handleSshKeyCreate fmt conn name publicKey
      SshKeyDelete keyRef -> handleSshKeyDelete fmt conn keyRef
      SshKeyList -> handleSshKeyList fmt tableOpts conn
      SshKeyAttach vmRef keyRef -> handleSshKeyAttach fmt conn vmRef keyRef
      SshKeyDetach vmRef keyRef -> handleSshKeyDetach fmt conn vmRef keyRef
      SshKeyListForVm vmRef -> handleSshKeyListForVm fmt tableOpts conn vmRef
      TemplateCreate mPath -> handleTemplateCreate fmt conn mPath
      TemplateEdit tRef -> handleTemplateEdit fmt conn tRef
      TemplateDelete tRef -> handleTemplateDelete fmt conn tRef
      TemplateList -> handleTemplateList fmt tableOpts conn
      TemplateShow tRef -> handleTemplateShow fmt conn tRef
      TemplateInstantiate tRef name nodeRef -> handleTemplateInstantiate fmt conn tRef name nodeRef
      -- Network commands
      NetworkCreate name nodeRef subnet dhcp nat as -> handleNetworkCreate fmt conn name nodeRef subnet dhcp nat as
      NetworkDelete nwRef -> handleNetworkDelete fmt conn nwRef
      NetworkStart nwRef -> handleNetworkStart fmt conn nwRef
      NetworkStop nwRef force -> handleNetworkStop fmt conn nwRef force
      NetworkList -> handleNetworkList fmt tableOpts conn
      NetworkShow nwRef -> handleNetworkShow fmt conn nwRef
      NetworkEdit nwRef mSubnet mDhcp mNat mAutostart -> handleNetworkEdit fmt conn nwRef mSubnet mDhcp mNat mAutostart
      NetworkAttachNode nwRef nodeRef -> handleNetworkAttachNode fmt conn nwRef nodeRef
      NetworkDetachNode nwRef nodeRef -> handleNetworkDetachNode fmt conn nwRef nodeRef
      -- Node commands
      NodeAdd name host nodeAgentPort netAgentPort mBasePath mDesc adminState netdDisabled ->
        CmdNode.handleNodeAdd fmt conn name host nodeAgentPort netAgentPort mBasePath mDesc adminState netdDisabled
      NodeList -> CmdNode.handleNodeList fmt tableOpts conn
      NodeShow nRef -> CmdNode.handleNodeShow fmt conn nRef
      NodeEdit nRef mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mAdminState mNetdDisabled ->
        CmdNode.handleNodeEdit fmt conn nRef mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mAdminState mNetdDisabled
      NodeDrain nRef -> CmdNode.handleNodeDrain fmt conn nRef
      NodeDelete nRef -> CmdNode.handleNodeDelete fmt conn nRef
      -- Cloud-init config
      CloudInitGenerate vmRef -> handleCloudInitGenerate fmt conn vmRef
      CloudInitSet vmRef mFile -> handleCloudInitSet fmt conn vmRef mFile
      CloudInitEdit vmRef -> handleCloudInitEdit fmt conn vmRef
      CloudInitShow vmRef -> handleCloudInitShow fmt conn vmRef
      CloudInitDelete vmRef -> handleCloudInitDelete fmt conn vmRef
      -- Apply
      Apply path skipExisting waitOpts -> handleApply fmt conn path skipExisting waitOpts
      -- Build
      Build path bcOpts waitOpts -> handleBuild fmt conn path bcOpts waitOpts
      -- Task history
      TaskList limit mSub mResult inclSub -> handleTaskList fmt tableOpts conn limit mSub mResult inclSub
      TaskShow taskId -> handleTaskShow fmt conn taskId
      TaskWait taskId mTimeout -> handleTaskWait fmt conn taskId mTimeout
      TaskCancel taskId -> handleTaskCancel fmt conn taskId
      -- Completion (handled above, but needed for exhaustive pattern match)
      Completion _ -> pure True

  case connResult of
    Left err -> do
      emitError fmt "connection_error" (T.pack $ show err) $
        putStrLn $
          "Connection error: " ++ show err
      exitFailure
    Right True -> exitSuccess
    Right False -> exitFailure

-- | Handle the completion command by generating a shell completion script.
handleCompletion :: Text -> IO ()
handleCompletion shell = do
  let progName = "crv"
  case map toLower (T.unpack shell) of
    "bash" -> putStr (bashCompletionScript progName progName)
    "zsh" -> putStr (zshCompletionScript progName progName)
    "fish" -> putStr (fishCompletionScript progName progName)
    _ -> hPutStrLn stderr ("Unknown shell: " <> T.unpack shell <> " (use bash, zsh, or fish)")
  exitSuccess

-- | Handle the @crv vm view@ command for both serial-console (headless)
-- and SPICE (graphical) VMs.
--
-- For headless VMs, the serial console relay rides
-- 'runSerialConsoleSession'. For graphical VMs,
-- 'handleGraphicalViewGrant' asks the daemon for a short-lived
-- password grant via 'CR.rpcVmViewGrant' and either emits it as
-- structured output or hands it to 'runRemoteViewer', which writes
-- a 0600 @.vv@ file and execs @remote-viewer@.
handleVmView :: Options -> OutputFormat -> CapnpConnection -> Text -> IO Bool
handleVmView opts fmt conn vmRef = do
  r <- try (CR.rpcVmShow conn (entityRefFromText vmRef)) :: IO (Either SomeException VmDetails)
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False
    Right details -> do
      let st = vdStatus details
      if st `notElem` [VmRunning, VmStarting, VmStopping]
        then do
          emitError fmt "vm_not_running" ("VM '" <> vdName details <> "' is not running") $ do
            putStrLn $ "Error: VM '" ++ T.unpack (vdName details) ++ "' is not running."
            putStrLn $ "Current status: " ++ T.unpack (enumToText st)
          pure False
        else
          if vdHeadless details
            then runSerialConsoleSession fmt conn vmRef (vdName details)
            else handleGraphicalViewGrant opts fmt conn vmRef (vdName details)

-- | Open the serial console for a headless VM. Wires the
-- bidirectional 'ByteSink' pair returned by 'CR.rpcVmSerialConsole'
-- to a raw-terminal session: server-pushed chunks go straight to
-- stdout, stdin bytes (less @Ctrl+]@-prefixed escape sequences)
-- flow back to the daemon.
runSerialConsoleSession :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
runSerialConsoleSession fmt conn vmRef vmName = do
  endSignal <- newEmptyMVar
  let ref = entityRefFromText vmRef
      onOutput chunk = BS.putStr chunk >> hFlush stdout
      onEnd = void (tryPutMVar endSignal ())
  attached <-
    try (CR.rpcVmSerialConsole conn ref onOutput onEnd)
      :: IO (Either SomeException (BS.ByteString -> IO (), IO ()))
  case attached of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error attaching serial console: " <> show e)
      pure False
    Right (writeInput, endInput) -> do
      putStrLn . T.unpack $ "Connecting to VM '" <> vmName <> "' serial console..."
      putStrLn "Escape: Ctrl+]  then  q=quit  d=Ctrl+Alt+Del  f=flush  ?=help"
      putStrLn ""
      let ctrlAltDelAction = Just (CR.rpcVmSendCtrlAltDel conn ref)
          flushAction = Just (CR.rpcVmSerialConsoleFlush conn ref)
      runRawTerminalSession
        writeInput
        endInput
        endSignal
        SerialSession
        ctrlAltDelAction
        flushAction

-- | Open the HMP (QEMU monitor) console for a running VM. Mirrors
-- 'runSerialConsoleSession' but rides the per-VM monitor buffer
-- instead, and the @Ctrl+] d@ key is suppressed by passing
-- 'Nothing' for the Ctrl+Alt+Del action (no meaning at the HMP
-- layer).
runHmpMonitorSession :: OutputFormat -> CapnpConnection -> Text -> IO Bool
runHmpMonitorSession fmt conn vmRef = do
  endSignal <- newEmptyMVar
  let ref = entityRefFromText vmRef
      onOutput chunk = BS.putStr chunk >> hFlush stdout
      onEnd = void (tryPutMVar endSignal ())
  attached <-
    try (CR.rpcVmHmpMonitor conn ref onOutput onEnd)
      :: IO (Either SomeException (BS.ByteString -> IO (), IO ()))
  case attached of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error attaching HMP monitor: " <> show e)
      pure False
    Right (writeInput, endInput) -> do
      putStrLn . T.unpack $ "Connecting to VM '" <> vmRef <> "' HMP monitor..."
      putStrLn "Escape: Ctrl+]  then  q=quit  f=flush  ?=help"
      putStrLn ""
      runRawTerminalSession
        writeInput
        endInput
        endSignal
        MonitorSession
        Nothing
        (Just (CR.rpcVmHmpMonitorFlush conn ref))

-- | Request a SPICE grant. For structured output emit the grant
-- (host, port, password, ttl) as JSON/YAML and exit; for text output
-- print a short banner with the endpoint and TTL and hand the grant
-- to 'runRemoteViewer', which materialises a @.vv@ file and execs
-- @remote-viewer@. The password is never printed to the terminal —
-- the user sees only @host:port@ — and never lands in @argv@, so
-- @ps(1)@ can't leak it.
handleGraphicalViewGrant :: Options -> OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleGraphicalViewGrant opts fmt conn vmRef vmName = do
  result <- try (CR.rpcVmViewGrant conn (entityRefFromText vmRef)) :: IO (Either SomeException ViewGrant)
  case result of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to obtain SPICE grant: " ++ show e)
      pure False
    Right grant0 -> do
      let grant = resolveGrantHost opts grant0
      if isStructured fmt
        then do
          outputValue fmt (grantToJson grant)
          pure True
        else do
          putStrLn $
            "VM '"
              ++ T.unpack vmName
              ++ "' is reachable via SPICE at "
              ++ T.unpack (vgHost grant)
              ++ ":"
              ++ show (vgPort grant)
              ++ " (ttl "
              ++ show (vgTtlSeconds grant)
              ++ "s)"
          runRemoteViewer defaultClientConfig grant

-- | Replace wildcard SPICE hosts with the client's @--host@ when
-- the client is connected via TCP. Unix-socket connections
-- always leave the grant host untouched (the daemon and the
-- viewer share a host, so a literal wildcard is harmless).
resolveGrantHost :: Options -> ViewGrant -> ViewGrant
resolveGrantHost opts grant
  | isWildcard (vgHost grant) && not (isUnixConnection opts) =
      grant {vgHost = T.pack (optHost opts)}
  | otherwise = grant
  where
    isWildcard h = h `elem` ["0.0.0.0", "::", ""]
    isUnixConnection o = optUnix o || isJust (optSocket o)

-- | JSON projection of a grant for @--output json|yaml@.
grantToJson :: ViewGrant -> Value
grantToJson g =
  object
    [ "host" .= vgHost g
    , "port" .= vgPort g
    , "password" .= vgPassword g
    , "ttl_seconds" .= vgTtlSeconds g
    ]

-- | Resolve the CLI's TLS config based on @--no-tls@ /
-- @--tls-cert-dir@ and the resolved listen address. Unix-socket
-- connections never need TLS; TCP connections load the client
-- credentials from @$XDG_CONFIG_HOME/corvus/@ (the cert is
-- per-user, never under @/etc/corvus@). On load failure the CLI
-- aborts with a clear pointer at @corvus-admin deploy client@
-- rather than silently falling back to plaintext — operators
-- expect TCP without certs to mean "you forgot to deploy".
resolveClientTls :: Options -> ListenAddress -> IO (Maybe Tls.TlsConfig)
resolveClientTls _ (UnixAddress _) = pure Nothing
resolveClientTls opts (TcpAddress _ _)
  | optNoTls opts = pure Nothing
  | otherwise = do
      sp <- case optTlsCertDir opts of
        Just d -> pure (Tls.CertSearchPath [d])
        Nothing -> Tls.clientCertSearchPath
      r <- Tls.loadTlsConfig sp Tls.RoleClient Tls.RoleDaemon Nothing
      case r of
        Right cfg -> pure (Just cfg)
        Left e -> do
          hPutStrLn stderr $
            "crv: failed to load TLS material for TCP connection: "
              <> show e
              <> "\n  searched: "
              <> show (Tls.certSearchDirs sp)
              <> "\nFix with `corvus-admin deploy client ...` or pass --no-tls."
          exitFailure

-- | Translate the CLI-side 'QuiesceModeFlag' to the wire enum.
toCapnpQuiesce :: QuiesceModeFlag -> CGEnums.QuiesceMode
toCapnpQuiesce QuiesceFlagAuto = CGEnums.QuiesceMode'auto
toCapnpQuiesce QuiesceFlagRequire = CGEnums.QuiesceMode'require
toCapnpQuiesce QuiesceFlagSkip = CGEnums.QuiesceMode'skip
