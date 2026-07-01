{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handler for applying declarative environment configurations from YAML.
-- Creates SSH keys, disks, networks, and VMs in dependency order.
module Corvus.Handlers.Apply
  ( -- * Action types
    ApplyAction (..)
  , ApplyDiskCreate (..)
  , ApplyVmCreate (..)

    -- * Handlers
  , handleApplyValidate
  , executeApply
  )
where

import Corvus.Action

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.CloudInit (RegenerateCloudInit (..))
import Corvus.Handlers.Disk (DiskClone (..), DiskCreate (..), DiskCreateOverlay (..), DiskDelete (..), DiskImportAction (..), DiskRegister (..))
import qualified Corvus.Handlers.NetIf as NetIfH
import Corvus.Handlers.Network (NetworkCreate (..), NetworkDelete (..))
import Corvus.Handlers.Resolve (resolveNode, validateName)
import Corvus.Handlers.SshKey (SshKeyCreate (..), SshKeyDelete (..))
import Corvus.Handlers.Template (TemplateDelete (..), insertTemplateYaml)
import Corvus.Handlers.Vm (VmCreate (..), VmDelete (..))
import Corvus.Model
import Corvus.Node.Image (detectFormatFromPath, isHttpUrl)
import Corvus.Protocol
import Corvus.Schema.Apply
  ( ApplyConfig (..)
  , ApplyDisk (..)
  , ApplyDrive (..)
  , ApplyNetIf (..)
  , ApplyNetwork (..)
  , ApplySharedDir (..)
  , ApplySshKey (..)
  , ApplyVm (..)
  , ChecksumAlgorithm (..)
  , ChecksumSpec (..)
  , ChecksumTarget (..)
  , IfExists (..)
  )
import Corvus.Schema.CloudInit (CloudInitConfigYaml (..))
import Corvus.Schema.Template (TemplateYaml (..))
import Corvus.Types
import Corvus.Utils.Network (generateMacAddress)
import Data.Char (isDigit)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Yaml (decodeEither')
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

-- | Validate YAML and config (synchronous, fast).
-- Returns Left Response on error, Right ApplyConfig on success.
handleApplyValidate :: ServerState -> Text -> IO (Either Response ApplyConfig)
handleApplyValidate state yamlContent = runServerLogging state $ do
  case decodeEither' (TE.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack $ show err
      logWarnN $ "Failed to parse apply config YAML: " <> msg
      pure $ Left $ RespError msg
    Right config -> do
      case validateConfig config of
        Left err -> do
          logWarnN $ "Config validation failed: " <> err
          pure $ Left $ RespError err
        Right () -> pure $ Right config

-- | Execute apply with subtask tracking.
-- Called by dispatchApply with the parent task ID.
-- Each resource creation is dispatched as an Action subtask via runActionAsSubtask.
--
-- The effective skip-existing policy is the OR of the CLI flag (which
-- forces skip when present) and the YAML's @ifExists: skip@. Absent
-- on both means @ifExists: error@ — the default — and any duplicate
-- resource fails the apply.
handleApplyExecute :: ActionContext -> ApplyConfig -> Bool -> IO Response
handleApplyExecute ctx config cliSkipExisting = runServerLogging (acState ctx) $ do
  logInfoN "Applying environment configuration..."
  -- The CLI's @--skip-existing@ flag only forces skip behaviour;
  -- the YAML's @ifExists@ field carries the full policy. The
  -- effective policy is the LOOSEST of the two: a CLI flag set
  -- always overrides @ifExists: error@ down to @skip@, but YAML's
  -- @ifExists: overwrite@ stays overwrite even if the operator
  -- forgot @--skip-existing@.
  let effective =
        if cliSkipExisting && acIfExists config == IfExistsError
          then IfExistsSkip
          else acIfExists config
  result <- liftIO $ executeApply ctx config effective
  case result of
    Left err -> do
      logWarnN $ "Apply failed: " <> err
      pure $ RespError err
    Right applyResult -> do
      logInfoN "Apply completed successfully"
      pure $ RespApplyResult applyResult

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateConfig :: ApplyConfig -> Either Text ()
validateConfig config = do
  checkDuplicates "SSH key" $ map askName (acSshKeys config)
  checkDuplicates "disk" $ map adName (acDisks config)
  -- VMs and networks are unique per-node (UniqueVmNamePerNode /
  -- UniqueNetworkNamePerNode), so two same-named entries on
  -- different nodes are legal. Dedupe by the (name, node) pair.
  checkDuplicatesPerNode "VM" [(avName v, avNode v) | v <- acVms config]
  checkDuplicatesPerNode "network" [(anName n, anNode n) | n <- acNetworks config]
  checkDuplicates "template" $ map tyName (acTemplates config)
  forM_ (acSshKeys config) $ \k -> validateName "SSH key" (askName k)
  forM_ (acDisks config) $ \d -> validateName "Disk" (adName d)
  forM_ (acNetworks config) $ \n -> validateName "Network" (anName n)
  forM_ (acVms config) $ \v -> validateName "VM" (avName v)
  forM_ (acTemplates config) $ \t -> validateName "Template" (tyName t)
  forM_ (acDisks config) validateDisk
  forM_ (acVms config) validateVmCloudInit
  where
    checkDuplicates :: Text -> [Text] -> Either Text ()
    checkDuplicates kind names =
      case findDuplicate names of
        Nothing -> Right ()
        Just d -> Left $ "Duplicate " <> kind <> " name: " <> d

    -- \| Dedup key is the (name, nodeRef) pair so same-named
    -- VMs / networks on different nodes are allowed. An empty
    -- node-ref (scheduler defers) is treated as its own bucket
    -- — if two entries both omit @node:@ the scheduler can't
    -- guarantee they land on different hosts, so we still
    -- refuse those as ambiguous duplicates.
    checkDuplicatesPerNode :: Text -> [(Text, Text)] -> Either Text ()
    checkDuplicatesPerNode kind pairs =
      case findDuplicate pairs of
        Nothing -> Right ()
        Just (nm, nd) ->
          Left $
            "Duplicate "
              <> kind
              <> " name '"
              <> nm
              <> "' on node '"
              <> (if T.null nd then "(scheduler)" else nd)
              <> "'"

    findDuplicate :: (Eq a) => [a] -> Maybe a
    findDuplicate [] = Nothing
    findDuplicate (x : xs)
      | x `elem` xs = Just x
      | otherwise = findDuplicate xs

    validateVmCloudInit :: ApplyVm -> Either Text ()
    validateVmCloudInit v =
      let ci = effectiveCloudInit v
       in if not (null (avSshKeys v)) && not ci
            then Left $ "VM '" <> avName v <> "': has SSH keys but cloud-init is not enabled"
            else Right ()

    validateDisk :: ApplyDisk -> Either Text ()
    validateDisk d =
      let hasImport = isJust (adImport d)
          hasOverlay = isJust (adOverlay d)
          hasClone = isJust (adClone d)
          hasRegister = isJust (adRegister d)
          hasBacking = isJust (adBacking d)
          hasCreate = isJust (adFormat d) && isJust (adSizeMb d) && not hasImport && not hasOverlay && not hasClone && not hasRegister
          strategies = length $ filter id [hasImport, hasOverlay, hasClone, hasRegister]
       in if strategies > 1
            then Left $ "Disk '" <> adName d <> "': cannot specify more than one of 'import', 'overlay', 'clone', 'register'"
            else
              if not hasImport && not hasOverlay && not hasClone && not hasRegister && not hasCreate
                then Left $ "Disk '" <> adName d <> "': must specify 'import', 'overlay', 'clone', 'register', or both 'format' and 'sizeMb'"
                else
                  if isJust (adPath d) && not hasOverlay && not hasClone && not hasCreate && not hasImport
                    then Left $ "Disk '" <> adName d <> "': 'path' can only be used with 'import', 'overlay', 'clone', or 'create'"
                    else
                      if hasBacking && not hasRegister
                        then Left $ "Disk '" <> adName d <> "': 'backing' can only be used with 'register'"
                        else case adChecksum d of
                          Nothing -> Right ()
                          Just cs
                            | not hasImport ->
                                Left $ "Disk '" <> adName d <> "': 'checksum' can only be used with 'import'"
                            | maybe False (not . isHttpUrl) (adImport d) ->
                                Left $ "Disk '" <> adName d <> "': 'checksum' can only be used with HTTP/HTTPS imports"
                            | not (isValidChecksum cs) ->
                                Left $
                                  "Disk '"
                                    <> adName d
                                    <> "': checksum value for "
                                    <> checksumAlgorithmText (csAlgorithm cs)
                                    <> " must be "
                                    <> T.pack (show (checksumHexLength (csAlgorithm cs)))
                                    <> " hex characters"
                            | otherwise -> Right ()

    isValidChecksum :: ChecksumSpec -> Bool
    isValidChecksum cs =
      T.length (csValue cs) == checksumHexLength (csAlgorithm cs)
        && T.all isHexDigit (csValue cs)

    isHexDigit c =
      isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Resolve effective cloudInit value for a VM.
-- If explicitly set, use that. Otherwise, auto-enable when sshKeys are present.
effectiveCloudInit :: ApplyVm -> Bool
effectiveCloudInit v = case avCloudInit v of
  Just ci -> ci
  Nothing -> not (null (avSshKeys v))

checksumAlgorithmText :: ChecksumAlgorithm -> Text
checksumAlgorithmText ChecksumMd5 = "md5"
checksumAlgorithmText ChecksumSha1 = "sha1"
checksumAlgorithmText ChecksumSha256 = "sha256"
checksumAlgorithmText ChecksumSha512 = "sha512"
checksumAlgorithmText ChecksumBlake2b = "blake2b"

checksumTargetText :: ChecksumTarget -> Text
checksumTargetText ChecksumDownload = "download"
checksumTargetText ChecksumFinal = "final"

checksumHexLength :: ChecksumAlgorithm -> Int
checksumHexLength ChecksumMd5 = 32
checksumHexLength ChecksumSha1 = 40
checksumHexLength ChecksumSha256 = 64
checksumHexLength ChecksumSha512 = 128
checksumHexLength ChecksumBlake2b = 128

checksumSpecToImport :: ChecksumSpec -> (Text, Text, Text)
checksumSpecToImport cs =
  (checksumAlgorithmText (csAlgorithm cs), csValue cs, checksumTargetText (csTarget cs))

--------------------------------------------------------------------------------
-- ifExists: overwrite plumbing
--------------------------------------------------------------------------------

-- | What an entity branch needs to mount the overwrite arm — a
-- pre-flight check that returns 'Left' with an actionable message
-- when the existing entity is in use (so the apply refuses
-- cleanly rather than surfacing the raw FK RESTRICT from the
-- DELETE), and the delete-subtask invocation itself. The matching
-- create call is shared with the no-existing branch, so it's
-- passed separately at the call site.
data Overwrite = Overwrite
  { oPreflight :: IO (Either Text ())
  , oDelete :: IO Response
  }

-- | Refuse a disk overwrite when the disk is attached to any VM.
-- Mirrors 'Corvus.Handlers.Build.checkIfExistsPreBake'.
preflightDiskOverwrite :: ServerState -> Text -> Int64 -> IO (Either Text ())
preflightDiskOverwrite state name eid = do
  attached <-
    runSqlPool (vmsAttachedToDisk (toSqlKey eid :: DiskImageId)) (ssDbPool state)
  pure $
    if null attached
      then Right ()
      else
        Left $
          "cannot overwrite disk '"
            <> name
            <> "': attached to VM(s) "
            <> T.intercalate ", " attached
            <> "; detach or delete those VMs first"

-- | Refuse a network overwrite when any VM has a NIC on it. The
-- referenced relation is @network_interface.network_id@ with
-- @ON DELETE RESTRICT@.
preflightNetworkOverwrite :: ServerState -> Text -> Int64 -> IO (Either Text ())
preflightNetworkOverwrite state name eid = do
  attached <- runSqlPool (vmsAttachedToNetwork (toSqlKey eid :: NetworkId)) (ssDbPool state)
  pure $
    if null attached
      then Right ()
      else
        Left $
          "cannot overwrite network '"
            <> name
            <> "': in use by VM(s) "
            <> T.intercalate ", " attached
            <> "; remove their network interfaces first"

-- | Refuse a VM overwrite when the VM is currently running. The
-- 'VmDelete' action already refuses non-stopped VMs at the
-- handler layer, but surfacing the reason here gives the
-- operator a clearer message and keeps the
-- 'EntityEnd'/error pair tagged @"overwrite"@.
preflightVmOverwrite :: ServerState -> Text -> Int64 -> IO (Either Text ())
preflightVmOverwrite state name eid = do
  mVm <- runSqlPool (get (toSqlKey eid :: VmId)) (ssDbPool state)
  pure $ case mVm of
    Nothing -> Right () -- raced with another delete; let the create surface it
    Just vm
      | vmStatus vm == VmStopped || vmStatus vm == VmError ->
          Right ()
      | otherwise ->
          Left $
            "cannot overwrite VM '"
              <> name
              <> "': currently in status "
              <> enumToText (vmStatus vm)
              <> "; stop it first"

-- | Refuse an SSH-key overwrite when the key is attached to any
-- VM or template. Both 'vm_ssh_key' and 'template_ssh_key' carry
-- @ON DELETE RESTRICT@ from 'ssh_key_id'.
preflightSshKeyOverwrite :: ServerState -> Int64 -> IO (Either Text ())
preflightSshKeyOverwrite state eid = do
  let keyId = toSqlKey eid :: SshKeyId
      pool = ssDbPool state
  vmRefs <- runSqlPool (selectList [VmSshKeySshKeyId ==. keyId] []) pool
  tmplRefs <- runSqlPool (selectList [TemplateSshKeySshKeyId ==. keyId] []) pool
  let vmIds = map (vmSshKeyVmId . entityVal) vmRefs
      tmplIds = map (templateSshKeyTemplateId . entityVal) tmplRefs
  vmNames <- runSqlPool (mapMaybeNamesM vmName vmIds) pool
  tmplNames <- runSqlPool (mapMaybeNamesM templateVmName tmplIds) pool
  pure $ case (vmNames, tmplNames) of
    ([], []) -> Right ()
    (vs, ts) ->
      Left $
        "cannot overwrite SSH key: attached to "
          <> describe "VM" vs
          <> (if null ts || null vs then "" else " and ")
          <> describe "template" ts
          <> "; detach first"
  where
    describe _ [] = ""
    describe kind xs = kind <> "(s) " <> T.intercalate ", " xs
    mapMaybeNamesM
      :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend)
      => (e -> Text)
      -> [Key e]
      -> SqlPersistT IO [Text]
    mapMaybeNamesM nameOf ks = do
      es <- mapM get ks
      pure [nameOf e | Just e <- es]

-- | Names of every VM that has the given disk attached. Mirrors
-- 'Corvus.Handlers.Build.vmsAttachedToDisk'; duplicated here to
-- avoid an import cycle (Build.hs imports Apply.hs already).
vmsAttachedToDisk :: DiskImageId -> SqlPersistT IO [Text]
vmsAttachedToDisk diskId = do
  drives <- selectList [DriveDiskImageId ==. diskId] []
  let vmIds = map (driveVmId . entityVal) drives
  vms <- mapM get vmIds
  pure [vmName v | Just v <- vms]

-- | Names of every VM that holds a network-interface on the given
-- network. Mirrors 'vmsAttachedToDisk' for symmetry.
vmsAttachedToNetwork :: NetworkId -> SqlPersistT IO [Text]
vmsAttachedToNetwork nid = do
  nis <- selectList [NetworkInterfaceNetworkId ==. Just nid] []
  let vmIds = map (networkInterfaceVmId . entityVal) nis
  vms <- mapM get vmIds
  pure [vmName v | Just v <- vms]

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

-- | Execute apply: create resources in dependency order using Action subtasks.
--
-- When @acApplySink ctx@ is non-silent, emits 'PhaseStart' before
-- each non-empty phase and 'EntityStart' / 'EntityEnd' bracketing
-- every entity creation (including the skip-existing branch, with
-- @kind = \"skip\"@, and the overwrite branch, with
-- @kind = \"overwrite\"@).
executeApply :: ActionContext -> ApplyConfig -> IfExists -> IO (Either Text ApplyResult)
executeApply ctx config ifExists = do
  -- Phase 1: SSH keys
  keyResult <- runPhase "sshKeys" (acSshKeys config) Map.empty $ \k _keyMap -> do
    let name = askName k
        create = runActionAsSubtask ctx (SshKeyCreate name (askPublicKey k))
    mExisting <- resolveExisting (resolveByName state UniqueSshKeyName Map.empty name)
    dispatchEntity "sshKeys" "ssh-key-create" name mExisting create $ \eid ->
      Overwrite
        (preflightSshKeyOverwrite state eid)
        (runActionAsSubtask ctx (SshKeyDelete eid))
  case keyResult of
    Left err -> pure $ Left err
    Right (keyMap, keyCreated) -> do
      -- Phase 2: Disks (need accumulating map for overlay/clone references)
      diskResult <- runPhase "disks" (acDisks config) Map.empty $ \d diskMap -> do
        let name = adName d
            create = runActionAsSubtask ctx (ApplyDiskCreate d diskMap)
        mExisting <- resolveExisting (resolveByName state UniqueDiskImageName diskMap name)
        dispatchEntity "disks" (diskKindTag d) name mExisting create $ \eid ->
          Overwrite
            (preflightDiskOverwrite state name eid)
            (runActionAsSubtask ctx (DiskDelete eid))
      case diskResult of
        Left err -> pure $ Left err
        Right (diskMap, diskCreated) -> do
          -- Phase 3: Networks
          nwResult <- runPhase "networks" (acNetworks config) Map.empty $ \n _nwMap -> do
            let name = anName n
                create =
                  runActionAsSubtask
                    ctx
                    ( NetworkCreate
                        name
                        (anNode n)
                        (anSubnet n)
                        (anDhcp n)
                        (anNat n)
                        (anAutostart n)
                        (anDnsServers n)
                        (anDomain n)
                        (anHostDns n)
                    )
            mExisting <-
              resolveExisting $
                resolveByNameFilter
                  state
                  (\nm -> [NetworkName ==. nm])
                  (\nid -> [NetworkNodeId ==. nid])
                  Map.empty
                  name
                  (anNode n)
            dispatchEntity "networks" "network-create" name mExisting create $ \eid ->
              Overwrite
                (preflightNetworkOverwrite state name eid)
                (runActionAsSubtask ctx (NetworkDelete eid))
          case nwResult of
            Left err -> pure $ Left err
            Right (nwMap, nwCreated) -> do
              -- Phase 4: VMs
              vmResult <- runSequentialVms (acVms config) keyMap diskMap nwMap
              case vmResult of
                Left err -> pure $ Left err
                Right vmCreated -> do
                  -- Phase 5: Templates
                  tmplResult <- runPhase "templates" (acTemplates config) Map.empty $ \ty _ -> do
                    let name = tyName ty
                        create = runActionAsSubtask ctx (ApplyTemplateCreate ty)
                    mExisting <- resolveExisting (resolveByName state UniqueTemplateVmName Map.empty name)
                    dispatchEntity "templates" "template-create" name mExisting create $ \eid ->
                      Overwrite
                        (pure (Right ()))
                        -- Templates have no incoming FK from other
                        -- entities (template_drive is owned by the
                        -- template itself and cascades). No
                        -- pre-flight refusal needed.
                        (runActionAsSubtask ctx (TemplateDelete eid))
                  case tmplResult of
                    Left err -> pure $ Left err
                    Right (_tmplMap, tmplCreated) ->
                      pure $
                        Right
                          ApplyResult
                            { arSshKeys = keyCreated
                            , arDisks = diskCreated
                            , arNetworks = nwCreated
                            , arVms = vmCreated
                            , arTemplates = tmplCreated
                            }
  where
    state = acState ctx
    sink = acApplySink ctx

    -- Resolve "does this name already exist" iff the policy
    -- needs to know — error policy passes through and lets the
    -- per-entity create surface the duplicate-name SQL error.
    resolveExisting :: IO (Maybe Int64) -> IO (Maybe Int64)
    resolveExisting q = case ifExists of
      IfExistsError -> pure Nothing
      IfExistsSkip -> q
      IfExistsOverwrite -> q

    -- Tag the disk's creation kind for the EntityStart event.
    diskKindTag :: ApplyDisk -> Text
    diskKindTag d = case (adImport d, adOverlay d, adClone d, adRegister d) of
      (Just _, _, _, _) -> "disk-import"
      (_, Just _, _, _) -> "disk-overlay"
      (_, _, Just _, _) -> "disk-clone"
      (_, _, _, Just _) -> "disk-register"
      _ -> "disk-create"

    -- The pieces 'dispatchEntity' needs to mount the overwrite
    -- branch. Computing the pre-flight inside the closure keeps
    -- the FK query off the happy paths (error / skip / no
    -- existing).
    -- (Lives in this 'where' so it captures the 'state' / 'ctx'
    -- via the enclosing closures rather than threading them.)
    --
    -- 'oPreflight': returns 'Left err' if the existing entity is
    -- in use and we should refuse the overwrite with an
    -- actionable message; 'Right ()' to proceed with the delete.
    -- 'oDelete': the @runActionAsSubtask ctx (FooDelete eid)@
    -- already wired up — the caller closes over the existing id.
    -- See 'withOverwrite' for the runtime shape.

    -- Single dispatcher per entity, picking among 'IfExistsError'
    -- (a name collision is a fatal apply failure),
    -- 'IfExistsSkip' (emit a skip event), 'IfExistsOverwrite'
    -- (pre-flight, delete, create — bracketed in one
    -- 'EntityStart'/'EntityEnd' pair tagged @"overwrite"@). The
    -- @mkOverwrite@ callback gets the existing id and returns the
    -- pre-flight + delete pair; the 'IO Response' for the create
    -- is the same value the no-existing branch would have used.
    dispatchEntity
      :: Text
      -> Text
      -> Text
      -> Maybe Int64
      -> IO Response
      -> (Int64 -> Overwrite)
      -> IO (Either Text (Text, Int64))
    dispatchEntity phase createKind name mExisting create mkOverwrite =
      case (mExisting, ifExists) of
        (Just eid, IfExistsSkip) -> withSkip phase name eid
        (Just eid, IfExistsOverwrite) -> do
          let ow = mkOverwrite eid
          withOverwrite phase name eid (oPreflight ow) (oDelete ow) create
        _ ->
          -- 'IfExistsError' + 'Just' falls into here too; the
          -- create action will surface the unique-constraint
          -- error verbatim, matching the pre-overwrite behaviour.
          withSubtask phase createKind name create

    -- Emit EntityStart / EntityEnd around an existing-entity skip.
    withSkip
      :: Text -> Text -> Int64 -> IO (Either Text (Text, Int64))
    withSkip phase name eid = do
      sink (EntityStart phase name "skip")
      sink (EntityEnd phase name TaskSuccess "" eid)
      pure $ Right (name, eid)

    -- Apply-side delete-then-create when the operator asked for
    -- 'ifExists: overwrite'. Mirrors 'checkIfExistsPreBake' in
    -- 'Corvus.Handlers.Build': the @preflight@ refuses with a
    -- clear message when the existing entity is in use (FK
    -- RESTRICT would otherwise surface as an opaque Postgres
    -- error). On a clean pre-flight, run the delete subtask; on
    -- delete success, run the create subtask. The whole
    -- delete-then-create is bracketed in one
    -- 'EntityStart' / 'EntityEnd' pair tagged @"overwrite"@.
    withOverwrite
      :: Text
      -> Text
      -> Int64
      -> IO (Either Text ())
      -> IO Response
      -> IO Response
      -> IO (Either Text (Text, Int64))
    withOverwrite phase name eid preflight runDelete runCreate = do
      sink (EntityStart phase name "overwrite")
      preRes <- preflight
      case preRes of
        Left err -> do
          sink (EntityEnd phase name TaskError err eid)
          pure $ Left $ phase <> " '" <> name <> "': " <> err
        Right () -> do
          delResp <- runDelete
          let (delTaskRes, delMsgM) = classifyResponse delResp
              delMsg = Data.Maybe.fromMaybe "" delMsgM
          case delTaskRes of
            TaskError -> do
              sink (EntityEnd phase name TaskError delMsg eid)
              pure $ Left $ phase <> " '" <> name <> "': delete: " <> delMsg
            _ -> do
              createResp <- runCreate
              out <- extractCreatedResult name createResp
              let (taskRes, msgM) = classifyResponse createResp
                  (mId, _) = extractEntityFromResponse createResp
                  msg = Data.Maybe.fromMaybe "" msgM
                  newEid = maybe 0 fromIntegral mId
              sink (EntityEnd phase name taskRes msg newEid)
              pure out

    -- Emit EntityStart / EntityEnd around a real subtask invocation,
    -- and translate the resulting 'Response' into the
    -- (name, entityId) pair the phase folds carry.
    withSubtask
      :: Text
      -> Text
      -> Text
      -> IO Response
      -> IO (Either Text (Text, Int64))
    withSubtask phase kind name run = do
      sink (EntityStart phase name kind)
      resp <- run
      out <- extractCreatedResult name resp
      let (taskRes, msgM) = classifyResponse resp
          (mId, _) = extractEntityFromResponse resp
          msg = Data.Maybe.fromMaybe "" msgM
          eid = maybe 0 fromIntegral mId
      sink (EntityEnd phase name taskRes msg eid)
      pure out

    -- Phase wrapper: emit PhaseStart before the fold (if non-empty),
    -- then delegate to runSequentialCreate.
    runPhase
      :: Text
      -> [a]
      -> Map.Map Text Int64
      -> (a -> Map.Map Text Int64 -> IO (Either Text (Text, Int64)))
      -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
    runPhase phase items initMap action = do
      case items of
        [] -> pure ()
        _ -> sink (PhaseStart phase (fromIntegral (length items)))
      runSequentialCreate items initMap action

    -- Run items sequentially, building a name→ID map.
    runSequentialCreate
      :: [a]
      -> Map.Map Text Int64
      -> (a -> Map.Map Text Int64 -> IO (Either Text (Text, Int64)))
      -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
    runSequentialCreate items initMap action = go items initMap []
      where
        go [] m acc = pure $ Right (m, reverse acc)
        go (item : rest) m acc = do
          -- Cooperative cancellation checkpoint: stop before creating
          -- the next entity if `crv task cancel` was issued.
          throwIfCancelled ctx
          result <- action item m
          case result of
            Left err -> pure $ Left err
            Right (name, rid) ->
              go rest (Map.insert name rid m) (ApplyCreated name rid : acc)

    -- Run VM creation sequentially (VMs don't need a name map)
    runSequentialVms
      :: [ApplyVm]
      -> Map.Map Text Int64
      -> Map.Map Text Int64
      -> Map.Map Text Int64
      -> IO (Either Text [ApplyCreated])
    runSequentialVms vms keyMap diskMap nwMap = do
      case vms of
        [] -> pure ()
        _ -> sink (PhaseStart "vms" (fromIntegral (length vms)))
      go vms []
      where
        go [] acc = pure $ Right $ reverse acc
        go (v : vs) acc = do
          let name = avName v
              create = runActionAsSubtask ctx (ApplyVmCreate keyMap diskMap nwMap v)
          mExisting <-
            resolveExisting $
              resolveByNameFilter
                state
                (\nm -> [VmName ==. nm])
                (\nid -> [VmNodeId ==. nid])
                Map.empty
                name
                (avNode v)
          let mkOverwrite eid =
                Overwrite
                  (preflightVmOverwrite state name eid)
                  -- keepDisks = False: drop ephemeral attached disks
                  -- (cloud-init ISO, template clones) along with the
                  -- VM, since the apply will recreate them.
                  (runActionAsSubtask ctx (VmDelete eid False))
          result <- dispatchEntity "vms" "vm-create" name mExisting create mkOverwrite
          case result of
            Left err -> pure $ Left err
            Right (_, vmId) ->
              -- Skipped VMs are intentionally dropped from
              -- 'ApplyResult.vms' — the original 'runSequentialVms'
              -- contract that 'TestMultiNodeDiskPlacement.
              -- test_apply_skip_existing_disambiguates_…' pins.
              -- Overwrite + fresh-create are echoed (the operator
              -- wants to see what actually ran).
              case (mExisting, ifExists) of
                (Just _, IfExistsSkip) -> go vs acc
                _ -> go vs (ApplyCreated name vmId : acc)

    -- Extract entity ID from a creation response.
    extractCreatedResult :: Text -> Response -> IO (Either Text (Text, Int64))
    extractCreatedResult name resp =
      let (result, msg) = classifyResponse resp
          (mId, _) = extractEntityFromResponse resp
       in case result of
            TaskSuccess -> case mId of
              Just eid -> pure $ Right (name, fromIntegral eid)
              Nothing -> pure $ Left $ name <> ": succeeded but no entity ID"
            _ -> pure $ Left $ name <> ": " <> fromMaybe "unknown error" msg

--------------------------------------------------------------------------------
-- Resource creation primitives (shared by tracked path)
--------------------------------------------------------------------------------

createOneVm
  :: ActionContext
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> ApplyVm
  -> IO (Either Text Int64)
createOneVm ctx keyMap diskMap nwMap v = do
  -- Use a dummy TaskId since VM creation doesn't create subtasks
  let dummyTaskId = toSqlKey 0
      state = acState ctx
  vmResult <-
    executeCreate
      ctx
      ( VmCreate
          (avName v)
          (avNode v)
          (avCpuCount v)
          (avRamMb v)
          (avDescription v)
          (avHeadless v)
          (avGuestAgent v)
          (effectiveCloudInit v)
          (avAutostart v)
          (avRebootQuirk v)
          (avCpuModel v)
      )
      dummyTaskId
  case vmResult of
    Left err -> pure $ Left $ "VM '" <> avName v <> "': " <> err
    Right vmId -> createOneVmAttachments ctx keyMap diskMap nwMap v (toSqlKey vmId)

createOneVmAttachments
  :: ActionContext
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> ApplyVm
  -> VmId
  -> IO (Either Text Int64)
createOneVmAttachments ctx keyMap diskMap nwMap v vmId = do
  let state = acState ctx
  -- Attach drives
  driveResult <- attachDrives state diskMap vmId (avDrives v) (avName v)
  case driveResult of
    Left err -> pure $ Left err
    Right () -> do
      -- Create network interfaces
      niResult <- createNetIfs state nwMap vmId (avNetworkInterfaces v) (avName v) (avNode v)
      case niResult of
        Left err -> pure $ Left err
        Right () -> do
          -- Create shared directories
          forM_ (avSharedDirs v) $ \sd ->
            runSqlPool
              ( insert_
                  SharedDir
                    { sharedDirVmId = vmId
                    , sharedDirPath = asdPath sd
                    , sharedDirTag = asdTag sd
                    , sharedDirCache = asdCache sd
                    , sharedDirReadOnly = asdReadOnly sd
                    }
              )
              (ssDbPool state)

          -- Attach SSH keys
          keyResult <- attachSshKeys state keyMap vmId (avSshKeys v) (avName v)
          case keyResult of
            Left err -> pure $ Left err
            Right () -> do
              -- Insert cloud-init config if provided
              forM_ (avCloudInitConfig v) $ \cic ->
                runSqlPool
                  ( insert_ $
                      CloudInit
                        vmId
                        (cicyUserData cic)
                        (cicyNetworkConfig cic)
                        (cicyInjectSshKeys cic)
                  )
                  (ssDbPool state)
              -- Generate cloud-init ISO if cloud-init is enabled
              when (effectiveCloudInit v) $ do
                _ <- runAction state (acClientName ctx) (RegenerateCloudInit (fromSqlKey vmId) (avName v))
                pure ()
              pure $ Right $ fromSqlKey vmId

attachDrives :: ServerState -> Map.Map Text Int64 -> VmId -> [ApplyDrive] -> Text -> IO (Either Text ())
attachDrives state diskMap vmId drives vmName = go drives
  where
    go [] = pure $ Right ()
    go (d : ds) = do
      mDiskId <- resolveByName state UniqueDiskImageName diskMap (adrDisk d)
      case mDiskId of
        Nothing -> pure $ Left $ "VM '" <> vmName <> "': disk '" <> adrDisk d <> "' not found"
        Just diskId -> do
          runSqlPool
            ( insert_
                Drive
                  { driveVmId = vmId
                  , driveDiskImageId = toSqlKey diskId
                  , driveInterface = adrInterface d
                  , driveMedia = adrMedia d
                  , driveReadOnly = adrReadOnly d
                  , driveCacheType = adrCacheType d
                  , driveDiscard = adrDiscard d
                  }
            )
            (ssDbPool state)
          go ds

createNetIfs :: ServerState -> Map.Map Text Int64 -> VmId -> [ApplyNetIf] -> Text -> Text -> IO (Either Text ())
createNetIfs state nwMap vmId netIfs vmName vmNodeRef = go netIfs
  where
    go [] = pure $ Right ()
    go (ni : nis) = case validateNetIf ni of
      Just err -> pure $ Left ("VM '" <> vmName <> "': " <> err)
      Nothing -> do
        mGate <- runSqlPool (NetIfH.checkVmNodeAllowsNicType vmId (aniType ni)) (ssDbPool state)
        case mGate of
          Just err -> pure $ Left ("VM '" <> vmName <> "': " <> err)
          Nothing -> doInsertNic ni nis
    doInsertNic ni nis = do
      mNetworkId <- case aniNetwork ni of
        Nothing -> pure $ Right Nothing
        Just nwName -> do
          mId <- resolveByNameFilter state (\nm -> [NetworkName ==. nm]) (\nid -> [NetworkNodeId ==. nid]) nwMap nwName vmNodeRef
          case mId of
            Nothing -> pure $ Left $ "VM '" <> vmName <> "': network '" <> nwName <> "' not found"
            Just nid -> pure $ Right $ Just nid
      case mNetworkId of
        Left err -> pure $ Left err
        Right networkId -> do
          mac <- maybe generateMacAddress pure (aniMac ni)
          runSqlPool
            ( insert_
                NetworkInterface
                  { networkInterfaceVmId = vmId
                  , networkInterfaceInterfaceType = aniType ni
                  , networkInterfaceHostDevice = fromMaybe "" (aniHostDevice ni)
                  , networkInterfaceMacAddress = mac
                  , networkInterfaceNetworkId = fmap toSqlKey networkId
                  , networkInterfaceGuestIpAddresses = Nothing
                  , networkInterfaceIpAddress = Nothing
                  }
            )
            (ssDbPool state)
          go nis
    validateNetIf ni = case aniType ni of
      NetBridge
        | maybe True T.null (aniHostDevice ni) ->
            Just "bridge interface requires hostDevice (the host bridge name)"
      _ -> Nothing

attachSshKeys :: ServerState -> Map.Map Text Int64 -> VmId -> [Text] -> Text -> IO (Either Text ())
attachSshKeys state keyMap vmId keyNames vmName = go keyNames
  where
    go [] = pure $ Right ()
    go (kn : kns) = do
      mKeyId <- resolveByName state UniqueSshKeyName keyMap kn
      case mKeyId of
        Nothing -> pure $ Left $ "VM '" <> vmName <> "': SSH key '" <> kn <> "' not found"
        Just keyId -> do
          runSqlPool (insert_ $ VmSshKey vmId (toSqlKey keyId)) (ssDbPool state)
          go kns

--------------------------------------------------------------------------------
-- Name Resolution
--------------------------------------------------------------------------------

-- | Format an exception into a user-friendly error message.
-- Strips common Haskell exception wrappers and SQL noise.
formatException :: SomeException -> Text
formatException e =
  let msg = T.pack (show e)
   in fromMaybe msg (extractDetail msg)
  where
    extractDetail msg
      | "already exists" `T.isInfixOf` T.toLower msg = Just "already exists"
      | "unique" `T.isInfixOf` T.toLower msg = Just "already exists (duplicate name)"
      | "violates unique constraint" `T.isInfixOf` T.toLower msg = Just "already exists (duplicate name)"
      | "violates foreign key" `T.isInfixOf` T.toLower msg = Just "referenced resource not found"
      | otherwise = Nothing

-- | Resolve a name to a DB ID. Checks in-config map first, then DB by unique constraint.
resolveByName
  :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record)
  => ServerState
  -> (Text -> Unique record)
  -> Map.Map Text Int64
  -> Text
  -> IO (Maybe Int64)
resolveByName state mkUnique localMap name = case Map.lookup name localMap of
  Just rid -> pure $ Just rid
  Nothing -> do
    mEntity <- runSqlPool (getBy (mkUnique name)) (ssDbPool state)
    pure $ fmap (fromSqlKey . entityKey) mEntity

-- | Same as 'resolveByName' but uses a 'selectList' filter instead
-- of a 'Unique' constructor — needed for entities (Vm, Network)
-- whose uniqueness is now composite (per-node) and so don't
-- expose a single-field name constraint.
--
-- @mNodeRef@ disambiguates name collisions across nodes. When set,
-- the caller is expected to pass the apply's @node:@ value as a
-- name or numeric id; the resolved 'NodeId' is appended to the
-- filter so two VMs named @web@ on different nodes resolve to the
-- correct one. Empty @mNodeRef@ falls back to "name only" — fine
-- for single-node deployments where the name is globally unique.
resolveByNameFilter
  :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record)
  => ServerState
  -> (Text -> [Filter record])
  -- ^ name-only filter constructor
  -> (NodeId -> [Filter record])
  -- ^ node-only filter constructor (combined with @mkFilter name@
  -- via list concatenation when @nodeRef@ resolves)
  -> Map.Map Text Int64
  -> Text
  -- ^ entity name
  -> Text
  -- ^ node ref (empty == no node filter)
  -> IO (Maybe Int64)
resolveByNameFilter state mkFilter mkNodeFilter localMap name nodeRef =
  case Map.lookup name localMap of
    Just rid -> pure $ Just rid
    Nothing -> do
      nodeFilter <-
        if T.null nodeRef
          then pure []
          else do
            mNid <- resolveNode (Ref nodeRef) (ssDbPool state)
            pure $ case mNid of
              Right n -> mkNodeFilter (toSqlKey n :: NodeId)
              Left _ -> []
      entities <- runSqlPool (selectList (mkFilter name ++ nodeFilter) []) (ssDbPool state)
      case entities of
        [e] -> pure $ Just (fromSqlKey (entityKey e))
        _ -> pure Nothing

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data ApplyAction = ApplyAction
  { aaConfig :: ApplyConfig
  , aaSkipExisting :: Bool
  }

instance Action ApplyAction where
  actionSubsystem _ = SubApply
  actionCommand _ = "apply"
  actionExecute ctx a = handleApplyExecute ctx (aaConfig a) (aaSkipExisting a)

-- | Apply-specific disk creation that handles overlay/clone name resolution.
data ApplyDiskCreate = ApplyDiskCreate
  { adcConfig :: ApplyDisk
  , adcDiskMap :: Map.Map Text Int64
  }

instance Action ApplyDiskCreate where
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . adName . adcConfig
  actionExecute ctx a =
    let d = adcConfig a
        state = acState ctx
        ephem = adEphemeral d
        nodeRef = adNode d
     in case (adImport d, adOverlay d, adClone d, adRegister d) of
          (Just importPath, _, _, _) ->
            actionExecute ctx (DiskImportAction (adName d) importPath (adPath d) (fmap enumToText (adFormat d)) (fmap checksumSpecToImport (adChecksum d)) ephem nodeRef)
          (_, _, _, Just registerPath)
            | isHttpUrl registerPath -> pure $ RespError $ "Disk '" <> adName d <> "': register requires a local path, not a URL"
            | otherwise -> do
                let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath registerPath)
                case adBacking d of
                  Nothing -> actionExecute ctx (DiskRegister (adName d) registerPath (Just format) Nothing ephem nodeRef)
                  Just backingName -> do
                    mBackingId <- resolveByName state UniqueDiskImageName (adcDiskMap a) backingName
                    case mBackingId of
                      Nothing -> pure $ RespError $ "backing disk '" <> backingName <> "' not found"
                      Just backingId -> actionExecute ctx (DiskRegister (adName d) registerPath (Just format) (Just backingId) ephem nodeRef)
          (_, Just backingName, _, _) -> do
            mBackingId <- resolveByName state UniqueDiskImageName (adcDiskMap a) backingName
            case mBackingId of
              Nothing -> pure $ RespError $ "backing disk '" <> backingName <> "' not found"
              Just backingId -> actionExecute ctx (DiskCreateOverlay (adName d) backingId (adSizeMb d) (adPath d) ephem)
          (_, _, Just cloneName, _) -> do
            mSourceId <- resolveByName state UniqueDiskImageName (adcDiskMap a) cloneName
            case mSourceId of
              Nothing -> pure $ RespError $ "source disk '" <> cloneName <> "' not found"
              Just sourceId -> actionExecute ctx (DiskClone (adName d) sourceId Nothing (adPath d) ephem)
          _ ->
            let format = fromMaybe FormatQcow2 (adFormat d)
                sizeMb = fromMaybe 10240 (adSizeMb d)
             in actionExecute ctx (DiskCreate (adName d) format (fromIntegral sizeMb) (adPath d) ephem nodeRef)

-- | Apply-specific VM creation with attachments (drives, netifs, SSH keys, cloud-init).
data ApplyVmCreate = ApplyVmCreate
  { avcKeyMap :: Map.Map Text Int64
  , avcDiskMap :: Map.Map Text Int64
  , avcNwMap :: Map.Map Text Int64
  , avcVm :: ApplyVm
  }

instance Action ApplyVmCreate where
  actionSubsystem _ = SubVm
  actionCommand _ = "create"
  actionEntityName = Just . avName . avcVm
  actionExecute ctx a = do
    result <- createOneVm ctx (avcKeyMap a) (avcDiskMap a) (avcNwMap a) (avcVm a)
    case result of
      Left err -> pure $ RespError err
      Right vmId -> pure $ RespVmCreated vmId

-- | Apply-specific template creation: takes an already-parsed 'TemplateYaml'
-- and inserts it via 'insertTemplateYaml'. Avoids re-serialising YAML just to
-- have the server parse it again.
newtype ApplyTemplateCreate = ApplyTemplateCreate {atcYaml :: TemplateYaml}

instance Action ApplyTemplateCreate where
  actionSubsystem _ = SubTemplate
  actionCommand _ = "create"
  actionEntityName = Just . tyName . atcYaml
  actionExecute ctx a = do
    let ty = atcYaml a
        state = acState ctx
    case validateName "Template" (tyName ty) of
      Left err -> pure $ RespError err
      Right () -> do
        now <- liftIO getCurrentTime
        result <- liftIO $ runSqlPool (insertTemplateYaml ty now) (ssDbPool state)
        case result of
          Left err -> pure $ RespError err
          Right tid -> pure $ RespTemplateCreated (fromSqlKey tid)
