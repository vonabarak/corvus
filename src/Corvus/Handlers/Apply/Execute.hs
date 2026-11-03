{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Corvus.Handlers.Apply.Execute (ApplyAction (..), executeApply) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Apply.Disk (ApplyDiskCreate (..), matchesDiskImport)
import Corvus.Handlers.Apply.Overwrite
import Corvus.Handlers.Apply.Resolve
import Corvus.Handlers.Apply.Validation (validateConfig)
import Corvus.Handlers.Apply.Vm (ApplyVmCreate (..))
import Corvus.Handlers.Network (NetworkCreate (..), NetworkDelete (..))
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Scheduler (pickNodeForNetwork, pickNodeForVm)
import Corvus.Handlers.SshKey (SshKeyCreate (..), SshKeyDelete (..))
import Corvus.Handlers.Template (TemplateDelete (..), insertTemplateYaml, templateOperationResources)
import Corvus.Handlers.Vm (VmDelete (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Apply
import Corvus.Schema.Template (TemplateYaml (..))
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist ((==.))
import Database.Persist.Sql (runSqlPool)

data ApplyAction = ApplyAction {aaConfig :: ApplyConfig, aaSkipExisting :: Bool}
instance Action ApplyAction where
  actionSubsystem _ = SubApply
  actionCommand _ = "apply"
  actionExecute ctx a = case validateConfig (aaConfig a) of
    Left err -> pure $ RespError err
    Right () -> handleApplyExecute ctx (aaConfig a) (aaSkipExisting a)
handleApplyExecute :: ActionContext -> ApplyConfig -> Bool -> IO Response
handleApplyExecute ctx config cliSkipExisting = runServerLogging (acState ctx) $ do
  logInfoN "Applying environment configuration..."
  let effective = if cliSkipExisting && acIfExists config == IfExistsError then IfExistsSkip else acIfExists config
  result <- liftIO $ executeApply ctx config effective
  case result of
    Left err -> logWarnN ("Apply failed: " <> err) >> pure (RespError err)
    Right applyResult -> logInfoN "Apply completed successfully" >> pure (RespApplyResult applyResult)

executeApply :: ActionContext -> ApplyConfig -> IfExists -> IO (Either Text ApplyResult)
executeApply ctx config ifExists = do
  keyResult <- phase "sshKeys" (acSshKeys config) Map.empty $ \k _ -> entity SshKeyObject Nothing (actionResources state (SshKeyCreate (askName k) (askPublicKey k))) "sshKeys" "ssh-key-create" (askName k) (resolveByName state UniqueSshKeyName Map.empty $ askName k) (\leased -> runActionAsSubtask leased $ SshKeyCreate (askName k) (askPublicKey k)) $ \leased eid -> Overwrite (preflightSshKeyOverwrite state eid) (runActionAsSubtask leased $ SshKeyDelete eid)
  case keyResult of
    Left e -> pure $ Left e
    Right (keyMap, keys) -> do
      diskResult <- phase "disks" (acDisks config) Map.empty $ \d m -> diskEntity d m
      case diskResult of
        Left e -> pure $ Left e
        Right (diskMap, disks) -> do
          networkResult <- phase "networks" (acNetworks config) Map.empty $ \raw _ -> withPlacement (anNode raw) (pickNodeForNetwork state) $ \node -> let n = raw {anNode = node} in entity NetworkObject (readNode node) (nodeNameResources state NetworkObject node (anName n)) "networks" "network-create" (anName n) (resolveByNameFilter state (\nm -> [NetworkName ==. nm]) (\nid -> [NetworkNodeId ==. nid]) Map.empty (anName n) (anNode n)) (\leased -> runActionAsSubtask leased $ NetworkCreate (anName n) (anNode n) (anSubnet n) (anDhcp n) (anNat n) (anAutostart n) (anDnsServers n) (anDomain n) (anHostDns n)) $ \leased eid -> Overwrite (preflightNetworkOverwrite state (anName n) eid) (runActionAsSubtask leased $ NetworkDelete eid)
          case networkResult of
            Left e -> pure $ Left e
            Right (networkMap, networks) -> do
              vmResult <- vms keyMap diskMap networkMap
              case vmResult of
                Left e -> pure $ Left e
                Right vs -> do
                  templateResult <- phase "templates" (acTemplates config) Map.empty $ \t _ -> entity TemplateObject Nothing (templateOperationResources state t) "templates" "template-create" (tyName t) (resolveByName state UniqueTemplateVmName Map.empty $ tyName t) (\leased -> runActionAsSubtask leased $ ApplyTemplateCreate t) $ \leased eid -> Overwrite (pure $ Right ()) (runActionAsSubtask leased $ TemplateDelete eid)
                  pure $ fmap (\(_, ts) -> ApplyResult keys disks networks vs ts) templateResult
  where
    state = acState ctx
    sink = acApplySink ctx
    diskKind d = case (adImport d, adOverlay d, adClone d, adRegister d) of (Just _, _, _, _) -> "disk-import"; (_, Just _, _, _) -> "disk-overlay"; (_, _, Just _, _) -> "disk-clone"; (_, _, _, Just _) -> "disk-register"; _ -> "disk-create"
    readNode ref = case reads (T.unpack ref) of [(node, "")] -> Just node; _ -> Nothing
    withPlacement :: Text -> IO (Either Text NodeId) -> (Text -> IO (Either Text a)) -> IO (Either Text a)
    withPlacement ref automatic operation = do
      pinned <- pinNode state ref automatic
      case pinned of
        Left response -> pure $ Left $ fromMaybe "Node placement failed" (snd (classifyResponse response))
        Right node -> operation node
    entity objectKind scope dependencies phaseName kind name find create makeOverwrite =
      fmap fst <$> entityOutcome objectKind scope dependencies phaseName kind name find create makeOverwrite
    entityOutcome objectKind scope dependencies phaseName kind name find create makeOverwrite =
      withPreparedOperationLeases
        ctx
        ( do
            found <- find
            old <- maybe (pure []) (objectResources state ExclusiveAccess objectKind) found
            images <- if objectKind == VmObject then maybe (pure []) (\eid -> actionResources state (VmDelete eid False False)) found else pure []
            refs <- dependencies
            pure (found, (ObjectName objectKind scope name, ExclusiveAccess) : old ++ images ++ refs)
        )
        $ \leased found -> do
          let mExisting = if ifExists == IfExistsError then Nothing else found
          case (mExisting, ifExists) of
            (Just eid, IfExistsSkip) -> sink (EntityStart phaseName name "skip") >> sink (EntityEnd phaseName name TaskSuccess "" eid) >> pure (Right ((name, eid), True))
            (Just eid, IfExistsOverwrite) -> fmap (,False) <$> overwrite phaseName name eid (makeOverwrite leased eid) (create leased)
            _ -> fmap (,False) <$> subtask phaseName kind name (create leased)
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
    diskEntity d m =
      withPreparedOperationLeases
        ctx
        ( do
            pins <- mapM (\name -> fmap (fmap (name,)) (resolveDiskName state m name)) (catMaybes [adOverlay d, adClone d, adBacking d])
            let pinned = Map.union (Map.fromList (catMaybes pins)) m
            resources <- actionResources state (ApplyDiskCreate d pinned)
            pure (pinned, resources)
        )
        (`diskEntityLocked` d)
    diskEntityLocked leased d m = do
      found <- resolveDiskName state m $ adName d
      let policy = fromMaybe (DiskIfExistsPolicy ifExists) (adIfExists d)
          name = adName d
          create = runActionAsSubtask leased $ ApplyDiskCreate d m
          skip eid = sink (EntityStart "disks" name "skip") >> sink (EntityEnd "disks" name TaskSuccess "" eid) >> pure (Right (name, eid))
      case (policy, found) of
        (DiskIfExistsPolicy IfExistsError, Just _) -> pure $ Left $ "Disk target already exists: " <> name
        (DiskIfExistsPolicy IfExistsSkip, Just eid) -> skip eid
        (DiskIfExistsUpdate, Just eid) -> do
          matches <- matchesDiskImport state d eid
          if matches then skip eid else subtask "disks" (diskKind d) name create
        (DiskIfExistsPolicy IfExistsOverwrite, Just eid) -> overwrite "disks" name eid (Overwrite (pure $ Right ()) (pure RespDiskOk)) create
        _ -> subtask "disks" (diskKind d) name create
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
        go (raw : rest) acc = withPlacement (avNode raw) (pickNodeForVm state (avRam raw)) $ \node -> do
          let v = raw {avNode = node}
          throwIfCancelled ctx
          let name = avName v
              find = resolveByNameFilter state (\nm -> [VmName ==. nm]) (\nid -> [VmNodeId ==. nid]) Map.empty name (avNode v)
              create leased = runActionAsSubtask leased $ ApplyVmCreate keyMap diskMap networkMap v
              ow leased eid = Overwrite (preflightVmOverwrite state name eid) (runActionAsSubtask leased $ VmDelete eid False False)
          r <- entityOutcome VmObject (readNode node) (actionResources state (ApplyVmCreate keyMap diskMap networkMap v)) "vms" "vm-create" name find create ow
          case r of
            Left e -> pure $ Left e
            Right ((_, eid), skipped) ->
              go rest $ if skipped then acc else ApplyCreated name eid : acc

newtype ApplyTemplateCreate = ApplyTemplateCreate {atcYaml :: TemplateYaml}
instance Action ApplyTemplateCreate where
  actionResources state a = templateOperationResources state (atcYaml a)
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
