{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Corvus.Handlers.Apply.Execute (ApplyAction (..), executeApply) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Apply.Disk (ApplyDiskCreate (..))
import Corvus.Handlers.Apply.Overwrite
import Corvus.Handlers.Apply.Resolve
import Corvus.Handlers.Apply.Vm (ApplyVmCreate (..))
import Corvus.Handlers.Disk.Maintenance (DiskDelete (..))
import Corvus.Handlers.Network (NetworkCreate (..), NetworkDelete (..))
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.SshKey (SshKeyCreate (..), SshKeyDelete (..))
import Corvus.Handlers.Template (TemplateDelete (..), insertTemplateYaml)
import Corvus.Handlers.Vm (VmDelete (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Apply
import Corvus.Schema.Template (TemplateYaml (..))
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist ((==.))
import Database.Persist.Sql (fromSqlKey, runSqlPool)

data ApplyAction = ApplyAction {aaConfig :: ApplyConfig, aaSkipExisting :: Bool}
instance Action ApplyAction where
  actionSubsystem _ = SubApply
  actionCommand _ = "apply"
  actionExecute ctx a = handleApplyExecute ctx (aaConfig a) (aaSkipExisting a)
handleApplyExecute ctx config cliSkipExisting = runServerLogging (acState ctx) $ do
  logInfoN "Applying environment configuration..."
  let effective = if cliSkipExisting && acIfExists config == IfExistsError then IfExistsSkip else acIfExists config
  result <- liftIO $ executeApply ctx config effective
  case result of
    Left err -> logWarnN ("Apply failed: " <> err) >> pure (RespError err)
    Right applyResult -> logInfoN "Apply completed successfully" >> pure (RespApplyResult applyResult)

executeApply ctx config ifExists = do
  keyResult <- phase "sshKeys" (acSshKeys config) Map.empty $ \k _ -> entity "sshKeys" "ssh-key-create" (askName k) (resolveByName state UniqueSshKeyName Map.empty $ askName k) (runActionAsSubtask ctx $ SshKeyCreate (askName k) (askPublicKey k)) $ \eid -> Overwrite (preflightSshKeyOverwrite state eid) (runActionAsSubtask ctx $ SshKeyDelete eid)
  case keyResult of
    Left e -> pure $ Left e
    Right (keyMap, keys) -> do
      diskResult <- phase "disks" (acDisks config) Map.empty $ \d m -> entity "disks" (diskKind d) (adName d) (resolveByName state UniqueDiskImageName m $ adName d) (runActionAsSubtask ctx $ ApplyDiskCreate d m) $ \eid -> Overwrite (preflightDiskOverwrite state (adName d) eid) (runActionAsSubtask ctx $ DiskDelete eid)
      case diskResult of
        Left e -> pure $ Left e
        Right (diskMap, disks) -> do
          networkResult <- phase "networks" (acNetworks config) Map.empty $ \n _ -> entity "networks" "network-create" (anName n) (resolveByNameFilter state (\nm -> [NetworkName ==. nm]) (\nid -> [NetworkNodeId ==. nid]) Map.empty (anName n) (anNode n)) (runActionAsSubtask ctx $ NetworkCreate (anName n) (anNode n) (anSubnet n) (anDhcp n) (anNat n) (anAutostart n) (anDnsServers n) (anDomain n) (anHostDns n)) $ \eid -> Overwrite (preflightNetworkOverwrite state (anName n) eid) (runActionAsSubtask ctx $ NetworkDelete eid)
          case networkResult of
            Left e -> pure $ Left e
            Right (networkMap, networks) -> do
              vmResult <- vms keyMap diskMap networkMap
              case vmResult of
                Left e -> pure $ Left e
                Right vs -> do
                  templateResult <- phase "templates" (acTemplates config) Map.empty $ \t _ -> entity "templates" "template-create" (tyName t) (resolveByName state UniqueTemplateVmName Map.empty $ tyName t) (runActionAsSubtask ctx $ ApplyTemplateCreate t) $ \eid -> Overwrite (pure $ Right ()) (runActionAsSubtask ctx $ TemplateDelete eid)
                  pure $ fmap (\(_, ts) -> ApplyResult keys disks networks vs ts) templateResult
  where
    state = acState ctx
    sink = acApplySink ctx
    existing q = case ifExists of IfExistsError -> pure Nothing; _ -> q
    diskKind d = case (adImport d, adOverlay d, adClone d, adRegister d) of (Just _, _, _, _) -> "disk-import"; (_, Just _, _, _) -> "disk-overlay"; (_, _, Just _, _) -> "disk-clone"; (_, _, _, Just _) -> "disk-register"; _ -> "disk-create"
    entity phaseName kind name find create makeOverwrite = do
      mExisting <- existing find
      case (mExisting, ifExists) of
        (Just eid, IfExistsSkip) -> sink (EntityStart phaseName name "skip") >> sink (EntityEnd phaseName name TaskSuccess "" eid) >> pure (Right (name, eid))
        (Just eid, IfExistsOverwrite) -> overwrite phaseName name eid (makeOverwrite eid) create
        _ -> subtask phaseName kind name create
    overwrite phaseName name eid ow create = do
      sink (EntityStart phaseName name "overwrite")
      pre <- oPreflight ow
      case pre of
        Left err -> sink (EntityEnd phaseName name TaskError err eid) >> pure (Left $ phaseName <> " '" <> name <> "': " <> err)
        Right () -> do
          deleted <- oDelete ow
          case classifyResponse deleted of
            (TaskError, msg) -> let m = fromMaybe "" msg in sink (EntityEnd phaseName name TaskError m eid) >> pure (Left $ phaseName <> " '" <> name <> "': delete: " <> m)
            _ -> subtaskEnd phaseName name create
    subtask phaseName kind name create = sink (EntityStart phaseName name kind) >> subtaskEnd phaseName name create
    subtaskEnd phaseName name create = do
      response <- create
      let (result, message) = classifyResponse response; (mId, _) = extractEntityFromResponse response; eid = maybe 0 fromIntegral mId; msg = fromMaybe "" message
      sink (EntityEnd phaseName name result msg eid)
      case result of
        TaskSuccess -> maybe (pure $ Left $ name <> ": succeeded but no entity ID") (\entityId -> pure $ Right (name, fromIntegral entityId)) mId
        _ -> pure $ Left $ name <> ": " <> fromMaybe "unknown error" message
    phase
      :: Text
      -> [a]
      -> Map.Map Text Int64
      -> (a -> Map.Map Text Int64 -> IO (Either Text (Text, Int64)))
      -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
    phase phaseName xs initial run = do
      if null xs then pure () else sink (PhaseStart phaseName $ fromIntegral $ length xs)
      go xs initial []
      where
        go [] m acc = pure $ Right (m, reverse acc)
        go (x : rest) m acc = do
          throwIfCancelled ctx
          r <- run x m
          case r of Left e -> pure $ Left e; Right (name, eid) -> go rest (Map.insert name eid m) (ApplyCreated name eid : acc)
    vms keyMap diskMap networkMap = do
      let xs = acVms config
      if null xs then pure () else sink (PhaseStart "vms" $ fromIntegral $ length xs)
      go xs []
      where
        go [] acc = pure $ Right $ reverse acc
        go (v : rest) acc = do
          throwIfCancelled ctx
          let name = avName v
              find = resolveByNameFilter state (\nm -> [VmName ==. nm]) (\nid -> [VmNodeId ==. nid]) Map.empty name (avNode v)
              create = runActionAsSubtask ctx $ ApplyVmCreate keyMap diskMap networkMap v
              ow eid = Overwrite (preflightVmOverwrite state name eid) (runActionAsSubtask ctx $ VmDelete eid False False)
          mExisting <- existing find
          r <- entity "vms" "vm-create" name (pure mExisting) create ow
          case r of
            Left e -> pure $ Left e
            Right (_, eid) -> do
              go rest $ case (mExisting, ifExists) of (Just _, IfExistsSkip) -> acc; _ -> ApplyCreated name eid : acc

newtype ApplyTemplateCreate = ApplyTemplateCreate {atcYaml :: TemplateYaml}
instance Action ApplyTemplateCreate where
  actionSubsystem _ = SubTemplate
  actionCommand _ = "create"
  actionEntityName = Just . tyName . atcYaml
  actionExecute ctx a = do
    let ty = atcYaml a; state = acState ctx
    case validateName "Template" (tyName ty) of
      Left err -> pure $ RespError err
      Right () -> do
        now <- liftIO getCurrentTime
        result <- liftIO $ runSqlPool (insertTemplateYaml ty now) (ssDbPool state)
        pure $ either RespError (RespTemplateCreated . fromSqlKey) result
