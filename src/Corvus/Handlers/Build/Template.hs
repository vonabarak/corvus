{-# LANGUAGE OverloadedStrings #-}

-- | Template resolution and validation for the build pipeline.
--
-- Looks up templates by name, validates guest-agent preconditions,
-- and instantiates the template into a bake VM with cleanup tracking.
module Corvus.Handlers.Build.Template
  ( resolveTemplateAndValidate
  , instantiateBakeVm
  , setupTargetDisk
  , sanitizeNameFragment
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT)
import Corvus.Action (mkActionContext, runActionAsSubtask)
import Corvus.Handlers.Build.Cleanup (CleanupStack, detachByTag, push)
import Corvus.Handlers.Build.CleanupBakeVm (cleanupBakeVm)
import Corvus.Handlers.Disk.Agent (getImageSizeViaAgent)
import Corvus.Handlers.Disk.Attach (DiskAttach (..))
import Corvus.Handlers.Disk.Create (DiskCreate (..))
import Corvus.Handlers.Disk.Derive (DiskCreateOverlay (..))
import Corvus.Handlers.Disk.Maintenance (DiskDelete (..))
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Handlers.Template (TemplateInstantiate (..), TemplateInstantiateResolved (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Build (Build (..), BuildStrategy (..), BuildTarget (..), btFormat, btSize)
import Corvus.Types
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (runSqlPool)

-- | Validate the captured template rather than resolving floating inputs a
-- second time. The installer strategy does not require a guest agent.
resolveTemplateAndValidate :: Build -> LoggingT IO (Either Text Int64)
resolveTemplateAndValidate b = pure $ case buildResolvedTemplate b of
  Nothing -> Left "build template snapshot missing"
  Just details
    | buildStrategy b /= BuildStrategyInstaller && not (tvdGuestAgent details) ->
        Left ("template '" <> buildTemplate b <> "' must have guestAgent: true")
    | otherwise -> Right (tvdId details)

-- | Instantiate the template into a bake VM and register
-- its cleanup destructor immediately so a later failure tears it down.
instantiateBakeVm
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> Maybe TemplateDetails
  -> Int64
  -- ^ template id
  -> Text
  -- ^ bake VM name
  -> Text
  -- ^ node reference
  -> LoggingT IO (Either Text Int64)
instantiateBakeVm state parentTaskId stack snapshot templateId bakeVmName nodeRef = do
  let context = mkActionContext state parentTaskId "system"
  resp <- liftIO $ case snapshot of
    Just details -> runActionAsSubtask context (TemplateInstantiateResolved details bakeVmName nodeRef)
    Nothing -> runActionAsSubtask context (TemplateInstantiate templateId bakeVmName nodeRef)
  case resp of
    RespTemplateInstantiated vmIdLong -> do
      liftIO $
        push stack "bake-vm" $
          cleanupBakeVm state parentTaskId vmIdLong
      pure $ Right vmIdLong
    RespError err -> pure $ Left $ "instantiate template: " <> err
    other ->
      pure $ Left $ "instantiate template: unexpected response: " <> T.pack (show other)

-- | Prepare the target disk for the artifact, per strategy.
-- Returns @(artifactDiskId, needFlatten)@:
--
--   * overlay   — the bake VM's first drive is the artifact; the
--                 backing chain must be flattened at publish time.
--   * installer — same, but the drive was freshly created so there
--                 is no chain to flatten.
--   * fromScratch — create a new empty target disk sized from the
--                 build's @target.size@ and attach it to the bake VM.
setupTargetDisk
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> Int64
  -- ^ bake VM id
  -> Build
  -> BuildTarget
  -> Text
  -- ^ ephemeral target name
  -> Text
  -- ^ build's target node (passes through to DiskCreate so the
  -- target lands on the same kernel as the bake VM)
  -> LoggingT IO (Either Text (Int64, Bool))
setupTargetDisk state parentTaskId stack vmIdLong build target targetTmpName buildNodeRef = case buildStrategy build of
  BuildStrategyOverlay -> firstDriveAsArtifact True
  BuildStrategyInstaller -> firstDriveAsArtifact False
  BuildStrategyFromScratch -> createAndAttachTarget
  BuildStrategyOfflineOverlay -> createOverlayTarget
  where
    firstDriveAsArtifact needFlatten = do
      mDrive <-
        liftIO $
          runSqlPool
            ( selectFirst
                [DriveVmId ==. toSqlKey vmIdLong]
                [Asc DriveId]
            )
            (ssDbPool state)
      case mDrive of
        Nothing -> pure $ Left "instantiated bake VM has no drives"
        Just (Entity _ drv) -> case driveDiskImageId drv of
          Nothing -> pure $ Left "instantiated bake VM has no drive media"
          Just diskImageId ->
            pure $ Right (fromSqlKey diskImageId, needFlatten)

    createAndAttachTarget = do
      let size = fromMaybe 10737418240 (btSize target)
      -- ephemeral=True for the bake-VM-attached disk; publish
      -- produces a separate CLONE with the target flag, so the bake disk stays
      -- ephemeral whether the build succeeds or fails.
      diskResp <-
        liftIO $
          runActionAsSubtask
            (mkActionContext state parentTaskId "system")
            (DiskCreate targetTmpName (btFormat target) size Nothing True buildNodeRef)
      case diskResp of
        RespDiskCreated diskIdLong -> trackAndAttach diskIdLong
        RespError err -> pure $ Left $ "create target disk: " <> err
        _ -> pure $ Left "create target disk: unexpected response"

    createOverlayTarget = case buildResolvedSourceImage build of
      Nothing -> pure $ Left "build source image snapshot missing"
      Just sourceId -> do
        mVm <- liftIO $ runSqlPool (get (toSqlKey vmIdLong)) (ssDbPool state)
        case mVm of
          Nothing -> pure $ Left "bake VM not found"
          Just vm -> do
            let nid = vmNodeId vm
            path <- liftIO $ resolveDiskPath (ssDbPool state) (ssQemuConfig state) (toSqlKey sourceId) nid
            sourceSize <- liftIO $ getImageSizeViaAgent state nid path
            case sourceSize of
              Nothing -> pure $ Left "cannot determine source image virtual size"
              Just size | maybe False (< size) (btSize target) -> pure $ Left "target.size cannot be smaller than source image virtual size"
              Just _ -> do
                resp <-
                  liftIO $
                    runActionAsSubtask
                      (mkActionContext state parentTaskId "system")
                      (DiskCreateOverlay targetTmpName sourceId (btSize target) Nothing True (Just nid))
                case resp of
                  RespDiskCreated diskId -> trackAndAttach diskId
                  RespError err -> pure $ Left $ "create target overlay: " <> err
                  other -> pure $ Left $ "create target overlay: unexpected response: " <> T.pack (show other)

    trackAndAttach diskIdLong = do
      liftIO $ push stack "orphan-target-disk" $ do
        _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (DiskDelete diskIdLong)
        pure ()
      attachResp <-
        liftIO $
          runActionAsSubtask
            (mkActionContext state parentTaskId "system")
            (DiskAttach vmIdLong diskIdLong InterfaceVirtio Nothing False False CacheWriteback)
      case attachResp of
        RespDiskAttached _ -> do
          liftIO $ detachByTag stack "orphan-target-disk"
          pure $ Right (diskIdLong, False)
        RespError err -> pure $ Left $ "attach target disk: " <> err
        _ -> pure $ Left "attach target disk: unexpected response"

-- | Sanitise a build name for use in a generated VM/disk name. Keeps
-- alphanumerics and dashes; everything else becomes a single dash.
sanitizeNameFragment :: Text -> Text
sanitizeNameFragment t =
  let cleaned = T.map (\c -> if isSafe c then c else '-') t
      collapsed = T.intercalate "-" $ filter (not . T.null) (T.split (== '-') cleaned)
   in T.take 32 (if T.null collapsed then "build" else collapsed)
  where
    isSafe c = c `elem` ('-' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'])
