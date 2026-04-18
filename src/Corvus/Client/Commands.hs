{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  )
where

import Corvus.Client.Commands.Apply
import Corvus.Client.Commands.CloudInit
import Corvus.Client.Commands.Disk
import Corvus.Client.Commands.GuestExec
import Corvus.Client.Commands.NetIf
import Corvus.Client.Commands.Network
import Corvus.Client.Commands.SharedDir
import Corvus.Client.Commands.SshKey
import Corvus.Client.Commands.Task
import Corvus.Client.Commands.Template
import Corvus.Client.Commands.Vm
import Corvus.Client.Config (defaultClientConfig)
import Corvus.Client.Connection
import Corvus.Client.Output
import Corvus.Client.Rpc
import Corvus.Client.Types
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (Ref (..), Request (..), Response (..), StatusInfo (..), VmDetails (..), VmInfo (..))
import Corvus.Qemu.Netns (nsExec)
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import Data.Aeson (object, toJSON, (.=))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Options.Applicative.BashCompletion (bashCompletionScript, fishCompletionScript, zshCompletionScript)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Handler (..), installHandler, sigINT)
import Text.Printf (printf)

-- | Get the listen address from options
getListenAddress :: Options -> IO ListenAddress
getListenAddress opts
  | optTcp opts = pure $ TcpAddress (optHost opts) (optPort opts)
  | otherwise = case optSocket opts of
      Just path -> pure $ UnixAddress path
      Nothing -> UnixAddress <$> getDefaultSocketPath

-- | Execute the selected command
runCommand :: Options -> IO ()
runCommand opts = do
  let fmt = optOutput opts
      tableOpts = tableOptsFromOptions opts
  -- Handle completion command before establishing daemon connection
  case optCommand opts of
    Completion shell -> handleCompletion shell
    _ -> pure ()
  addr <- getListenAddress opts
  connResult <- withConnection addr $ \conn ->
    case optCommand opts of
      Ping -> do
        resp <- sendPing conn
        case resp of
          Left err -> do
            emitRpcError fmt err
            pure False
          Right () -> do
            emitOk fmt $ putStrLn "pong"
            pure True
      Status -> do
        resp <- getStatus conn
        case resp of
          Left err -> do
            emitRpcError fmt err
            pure False
          Right st@StatusInfo {..} -> do
            emitResult fmt st $ do
              putStrLn $ "Uptime:           " ++ formatUptime siUptime
              putStrLn $ "Connections:      " ++ show siConnections
              putStrLn $ "Version:          " ++ T.unpack siVersion
              putStrLn $ "Protocol version: " ++ show siProtocolVersion
              case siNamespacePid of
                Nothing -> putStrLn "Namespace:        not running"
                Just pid -> putStrLn $ "Namespace:        PID " ++ show pid
            pure True
      Shutdown -> do
        resp <- requestShutdown conn
        case resp of
          Left err -> do
            emitRpcError fmt err
            pure False
          Right True -> do
            emitOk fmt $ putStrLn "Shutdown acknowledged"
            pure True
          Right False -> do
            emitError fmt "shutdown_rejected" "Shutdown not acknowledged" $
              putStrLn "Shutdown not acknowledged"
            pure False
      VmList -> do
        resp <- vmList conn
        case resp of
          Left err -> do
            emitRpcError fmt err
            pure False
          Right vms -> do
            emitResult fmt vms $
              if null vms
                then putStrLn "No VMs found."
                else do
                  now <- getCurrentTime
                  printTable tableOpts (vmColumns now) vms
            pure True
      VmShow vmRef -> do
        resp <- vmShow conn vmRef
        case resp of
          Left err -> do
            emitRpcError fmt err
            pure False
          Right Nothing -> do
            emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
              putStrLn $
                "VM '" ++ T.unpack vmRef ++ "' not found."
            pure False
          Right (Just details) -> do
            emitResult fmt details $ printVmDetails details
            pure True
      VmCreate name cpuCount ramMb mDesc headless ga ci as -> handleVmCreate fmt conn name cpuCount ramMb mDesc headless ga ci as
      VmDelete vmRef deleteDisks -> handleVmDelete fmt conn vmRef deleteDisks
      VmStart vmRef waitOpts -> handleVmStart fmt conn vmRef waitOpts
      VmStop vmRef waitOpts -> handleVmStop fmt conn vmRef waitOpts
      VmPause vmRef -> handleVmAction fmt "pause" vmRef (vmPause conn vmRef)
      VmReset vmRef -> handleVmAction fmt "reset" vmRef (vmReset conn vmRef)
      VmEdit vmRef mCpus mRam mDesc mHeadless mGa mCi mAs -> handleVmEdit fmt conn vmRef mCpus mRam mDesc mHeadless mGa mCi mAs
      VmExec vmRef cmd -> handleVmExec fmt conn vmRef cmd
      VmView vmRef -> do
        resp <- vmShow conn vmRef
        case resp of
          Left err -> do
            emitRpcError fmt err
            pure False
          Right Nothing -> do
            emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
              putStrLn $
                "VM '" ++ T.unpack vmRef ++ "' not found."
            pure False
          Right (Just details) -> do
            let st = vdStatus details
            if st `notElem` [VmRunning, VmStarting, VmStopping]
              then do
                emitError fmt "vm_not_running" ("VM '" <> vdName details <> "' is not running") $ do
                  putStrLn $ "Error: VM '" ++ T.unpack (vdName details) ++ "' is not running."
                  putStrLn $ "Current status: " ++ T.unpack (enumToText st)
                pure False
              else do
                if vdHeadless details
                  then do
                    let monitorSock = T.unpack (vdMonitorSocket details)
                    if isStructured fmt
                      then outputValue fmt (object ["serialSocket" .= vdSerialSocket details])
                      else do
                        -- Attach to serial console via daemon RPC (buffered)
                        serialResp <- sendRequest conn (ReqSerialConsole (Ref vmRef))
                        case serialResp of
                          Right RespSerialConsoleOk -> do
                            putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' serial console..."
                            putStrLn "Escape: Ctrl+]  then  q=quit  d=Ctrl+Alt+Del  f=flush  ?=help"
                            putStrLn ""
                            let flushAction = do
                                  _ <- withConnection addr $ \flushConn ->
                                    sendRequest flushConn (ReqSerialConsoleFlush (Ref vmRef))
                                  pure ()
                            _ <- runRawTerminalSession (connSocket conn) (Just monitorSock) (Just flushAction)
                            pure ()
                          Right (RespError err) ->
                            putStrLn $ "Error: " ++ T.unpack err
                          Right other ->
                            putStrLn $ "Unexpected response: " ++ show other
                          Left err ->
                            putStrLn $ "Error: " ++ show err
                  else do
                    let spiceSock = T.unpack (vdSpiceSocket details)
                    if isStructured fmt
                      then outputValue fmt (object ["spiceSocket" .= vdSpiceSocket details])
                      else withIgnoredSigINT $ do
                        putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' via SPICE..."
                        _ <- runRemoteViewer defaultClientConfig spiceSock
                        pure ()
                pure True
      VmMonitor vmRef -> do
        resp <- vmShow conn vmRef
        case resp of
          Left err -> do
            emitRpcError fmt err
            pure False
          Right Nothing -> do
            emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
              putStrLn $
                "VM '" ++ T.unpack vmRef ++ "' not found."
            pure False
          Right (Just details) -> do
            if vdStatus details /= VmRunning
              then do
                emitError fmt "vm_not_running" ("VM '" <> vdName details <> "' is not running") $ do
                  putStrLn $ "Error: VM '" ++ T.unpack (vdName details) ++ "' is not running."
                  putStrLn $ "Current status: " ++ T.unpack (enumToText $ vdStatus details)
                pure False
              else do
                let monitorSock = T.unpack (vdMonitorSocket details)
                if isStructured fmt
                  then outputValue fmt (object ["monitorSocket" .= vdMonitorSocket details])
                  else do
                    putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' HMP monitor..."
                    putStrLn "Escape: Ctrl+]  then  q=quit  ?=help"
                    putStrLn ""
                    _ <- runMonitorSession monitorSock Nothing
                    pure ()
                pure True
      -- Disk commands
      DiskCreate name formatStr sizeMb mPath -> do
        case parseFormat formatStr of
          Left err -> do
            emitError fmt "invalid_format" err $ putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right format -> handleDiskCreate fmt conn name format sizeMb mPath
      DiskCreateOverlay name baseDiskRef optDirPath -> handleDiskCreateOverlay fmt conn name baseDiskRef optDirPath
      DiskRegisterCmd name path mFormatStr mBackingRef -> handleDiskRegister fmt conn name path mFormatStr mBackingRef
      DiskImport name source mPath mFormatStr waitOpts -> handleDiskImport fmt conn name source mPath mFormatStr waitOpts
      DiskRefresh diskRef -> handleDiskRefresh fmt conn diskRef
      DiskDelete diskRef -> handleDiskDelete fmt conn diskRef
      DiskResize diskRef newSizeMb -> handleDiskResize fmt conn diskRef newSizeMb
      DiskList -> handleDiskList fmt tableOpts conn
      DiskShow diskRef -> handleDiskShow fmt conn diskRef
      DiskClone name baseDiskRef optionalPath -> handleDiskClone fmt conn name baseDiskRef optionalPath
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
      SnapshotCreate diskRef name -> handleSnapshotCreate fmt conn diskRef name
      SnapshotDelete diskRef snapshotRef -> handleSnapshotDelete fmt conn diskRef snapshotRef
      SnapshotRollback diskRef snapshotRef -> handleSnapshotRollback fmt conn diskRef snapshotRef
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
      TemplateInstantiate tRef name -> handleTemplateInstantiate fmt conn tRef name
      -- Network commands
      NetworkCreate name subnet dhcp nat as -> handleNetworkCreate fmt conn name subnet dhcp nat as
      NetworkDelete nwRef -> handleNetworkDelete fmt conn nwRef
      NetworkStart nwRef -> handleNetworkStart fmt conn nwRef
      NetworkStop nwRef force -> handleNetworkStop fmt conn nwRef force
      NetworkList -> handleNetworkList fmt tableOpts conn
      NetworkShow nwRef -> handleNetworkShow fmt conn nwRef
      NetworkEdit nwRef mSubnet mDhcp mNat mAutostart -> handleNetworkEdit fmt conn nwRef mSubnet mDhcp mNat mAutostart
      -- Cloud-init config
      CloudInitGenerate vmRef -> handleCloudInitGenerate fmt conn vmRef
      CloudInitSet vmRef mFile -> handleCloudInitSet fmt conn vmRef mFile
      CloudInitEdit vmRef -> handleCloudInitEdit fmt conn vmRef
      CloudInitShow vmRef -> handleCloudInitShow fmt conn vmRef
      CloudInitDelete vmRef -> handleCloudInitDelete fmt conn vmRef
      -- Apply
      Apply path skipExisting waitOpts -> handleApply fmt conn path skipExisting waitOpts
      -- Task history
      TaskList limit mSub mResult inclSub -> handleTaskList fmt tableOpts conn limit mSub mResult inclSub
      TaskShow taskId -> handleTaskShow fmt conn taskId
      TaskWait taskId mTimeout -> handleTaskWait fmt conn taskId mTimeout
      -- Namespace exec
      NamespaceExec cmdArgs -> handleNamespaceExec fmt conn cmdArgs
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

-- | Handle namespace exec: fetch namespace PID from daemon, then run command locally via FFI.
handleNamespaceExec :: OutputFormat -> Connection -> [String] -> IO Bool
handleNamespaceExec fmt conn cmdArgs = do
  resp <- getStatus conn
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right StatusInfo {siNamespacePid = mNsPid} -> case mNsPid of
      Nothing -> do
        emitError fmt "no_namespace" "Network namespace is not running" $
          putStrLn "Error: network namespace is not running"
        pure False
      Just nsPid -> do
        args <- case cmdArgs of
          [] -> do
            mShell <- lookupEnv "SHELL"
            pure [fromMaybe "/bin/sh" mShell]
          as -> pure as
        result <- withIgnoredSigINT $ nsExec nsPid args
        case result of
          Right () -> pure True
          Left err -> do
            emitError fmt "ns_exec_error" err $ putStrLn $ "Error: " ++ T.unpack err
            pure False

-- | Handle the completion command by generating a shell completion script.
-- This exits immediately without connecting to the daemon.
handleCompletion :: Text -> IO ()
handleCompletion shell = do
  let progName = "crv"
  case map toLower (T.unpack shell) of
    "bash" -> putStr (bashCompletionScript progName progName)
    "zsh" -> putStr (zshCompletionScript progName progName)
    "fish" -> putStr (fishCompletionScript progName progName)
    _ -> hPutStrLn stderr ("Unknown shell: " <> T.unpack shell <> " (use bash, zsh, or fish)")
  exitSuccess

-- | Run an action with SIGINT ignored, restoring the previous handler afterwards.
-- This lets Ctrl+C pass through to the child process (VM) instead of killing the client.
withIgnoredSigINT :: IO a -> IO a
withIgnoredSigINT action = do
  oldHandler <- installHandler sigINT Ignore Nothing
  result <- action
  _ <- installHandler sigINT oldHandler Nothing
  pure result
