{-# LANGUAGE OverloadedStrings #-}

-- | Template resolution and validation for the build pipeline.
--
-- Looks up templates by name, validates guest-agent preconditions,
-- and instantiates the template into a bake VM with cleanup tracking.
module Corvus.Handlers.Build.Template
  ( resolveTemplateAndValidate
  , instantiateBakeVm
  , setupTargetDisk
  , resolveTemplateIdOrErr
  , sanitizeNameFragment
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN)
import Corvus.Action (mkActionContext, runActionAsSubtask)
import Corvus.Handlers.Build.Cleanup (CleanupStack, push)
import Corvus.Handlers.Build.CleanupBakeVm (cleanupBakeVm)
import Corvus.Handlers.Disk.Attach (DiskAttach (..))
import Corvus.Handlers.Disk.Create (DiskCreate (..))
import Corvus.Handlers.Disk.Db (listDiskImageNodes, recordDiskImageNode)
import Corvus.Handlers.Disk.Maintenance (DiskDelete (..))
import Corvus.Handlers.Disk.Path (resolveDiskFilePathPure)
import Corvus.Handlers.Template (TemplateInstantiate (..))
import Corvus.Handlers.Vm (VmDelete (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Build (BuildStrategy (..), BuildTarget (..), btFormat, btSizeGb)
import Corvus.Types
import Data.Int (Int64)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Look up a template by name, returning its id and whether guest-agent
-- is enabled. Both of those have to be true for a build to proceed.
resolveTemplateIdOrErr :: ServerState -> Text -> IO (Either Text (Int64, Bool))
resolveTemplateIdOrErr state name = do
  mEntity <- runSqlPool (getBy (UniqueTemplateVmName name)) (ssDbPool state)
  pure $ case mEntity of
    Nothing -> Left $ "template '" <> name <> "' not found"
    Just (Entity key tpl) -> Right (fromSqlKey key, templateVmGuestAgent tpl)

-- | Resolve the build's template by name and enforce the
-- guest-agent precondition. The installer strategy doesn't need QGA
-- (vendor autounattend drives everything); every other strategy does.
resolveTemplateAndValidate
  :: ServerState
  -> BuildStrategy
  -> Text
  -- ^ template name
  -> LoggingT IO (Either Text Int64)
resolveTemplateAndValidate state strategy tplName = do
  r <- liftIO $ resolveTemplateIdOrErr state tplName
  pure $ case r of
    Left err -> Left err
    Right (templateId, hasGuestAgent)
      | strategy /= BuildStrategyInstaller && not hasGuestAgent ->
          Left ("template '" <> tplName <> "' must have guestAgent: true")
      | otherwise -> Right templateId

-- | Instantiate the template into a bake VM and register
-- its cleanup destructor immediately so a later failure tears it down.
instantiateBakeVm
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> Int64
  -- ^ template id
  -> Text
  -- ^ bake VM name
  -> Text
  -- ^ node reference
  -> LoggingT IO (Either Text Int64)
instantiateBakeVm state parentTaskId stack templateId bakeVmName nodeRef = do
  resp <-
    liftIO $
      runActionAsSubtask
        (mkActionContext state parentTaskId "system")
        ( TemplateInstantiate
            { tiTemplateId = templateId
            , tiName = bakeVmName
            , tiNodeRef = nodeRef
            }
        )
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
--                 build's @target.sizeGb@ and attach it to the bake VM.
setupTargetDisk
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> Int64
  -- ^ bake VM id
  -> BuildStrategy
  -> BuildTarget
  -> Text
  -- ^ ephemeral target name
  -> Text
  -- ^ build's target node (passes through to DiskCreate so the
  -- target lands on the same kernel as the bake VM)
  -> LoggingT IO (Either Text (Int64, Bool))
setupTargetDisk state parentTaskId stack vmIdLong strategy target targetTmpName buildNodeRef = case strategy of
  BuildStrategyOverlay -> firstDriveAsArtifact True
  BuildStrategyInstaller -> firstDriveAsArtifact False
  BuildStrategyFromScratch -> createAndAttachTarget
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
      let sizeMb = fromIntegral (btSizeGb target) * 1024
      -- ephemeral=True for the bake-VM-attached disk; publish
      -- produces a CLONE (non-ephemeral) so the bake disk stays
      -- ephemeral whether the build succeeds or fails. If
      -- @--build-cache@ leaves cache rows behind, 'cleanupBakeVm'
      -- skips the @VmDelete@ and the bake VM + this ephemeral
      -- disk both survive for future cache hits.
      diskResp <-
        liftIO $
          runActionAsSubtask
            (mkActionContext state parentTaskId "system")
            (DiskCreate targetTmpName (btFormat target) sizeMb Nothing True buildNodeRef)
      case diskResp of
        RespDiskCreated diskIdLong -> attachTarget diskIdLong
        RespError err -> pure $ Left $ "create target disk: " <> err
        _ -> pure $ Left "create target disk: unexpected response"

    attachTarget diskIdLong = do
      attachResp <-
        liftIO $
          runActionAsSubtask
            (mkActionContext state parentTaskId "system")
            ( DiskAttach
                vmIdLong
                diskIdLong
                InterfaceVirtio
                Nothing
                False
                False
                CacheWriteback
            )
      case attachResp of
        RespDiskAttached _ -> pure $ Right (diskIdLong, False)
        RespError err -> do
          -- The disk was created but couldn't attach: register a
          -- cleanup so a failed attach doesn't strand it.
          liftIO $
            push stack "orphan-target-disk" $ do
              _ <-
                runActionAsSubtask
                  (mkActionContext state parentTaskId "system")
                  (DiskDelete diskIdLong)
              pure ()
          pure $ Left $ "attach target disk: " <> err
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
