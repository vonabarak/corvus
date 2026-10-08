{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Upload disk-image handlers.
module Corvus.Handlers.Disk.Upload
  ( DiskUploadPlan (..)
  , DiskUploadFinalize (..)
  , checkDiskUpload
  , prepareDiskUpload
  )
where

import Control.Monad.IO.Class (liftIO)
import Corvus.Action
import Corvus.Handlers.Disk.Agent (deleteImageViaAgent, getImageInfoViaAgent)
import Corvus.Handlers.Disk.Db (recordDiskImageNode)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, sanitizeDiskName)
import Corvus.Handlers.Disk.Placement (nodeBasePathFor)
import Corvus.Handlers.Resolve (resolveErrorMessage, resolveNode)
import Corvus.Handlers.Scheduler (pickNodeForDisk)
import Corvus.Images
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageInfo (..))
import Corvus.Protocol
import Corvus.Protocol.Disk (UploadIfExists (..))
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (runSqlPool)

-- | Read-only policy decision, before reserving an ID or opening a writer.
-- A tagged publication checks that tag, not the family's latest version.
checkDiskUpload :: ServerState -> Text -> DriveFormat -> UploadIfExists -> Maybe Text -> IO (Either Text (Maybe DiskImageId))
checkDiskUpload state name format policy digest =
  case sanitizeDiskName name of
    Left err -> pure (Left err)
    Right _ -> runSqlPool decide (ssDbPool state)
  where
    decide = do
      existing <- imageByName name
      case existing of
        Nothing -> pure (Right Nothing)
        Just (Entity key _) -> case policy of
          UploadError -> pure (Left "upload target already exists")
          UploadSkip -> pure (Right (Just key))
          UploadOverwrite -> pure (Right Nothing)
          UploadUpdate -> do
            matches <- maybe (pure False) (matchesUploadIdentity key format) digest
            pure (Right (if matches then Just key else Nothing))

-- | Resolved destination and reserved ID for a client-upload stream.
-- The completed image and its tags are published only by finalization.
data DiskUploadPlan = DiskUploadPlan
  { dupImageId :: !M.DiskImageId
  , dupName :: !Text
  , dupFormat :: !DriveFormat
  , dupEphemeral :: !Bool
  , dupNodeId :: !M.NodeId
  , dupFilePath :: !FilePath
  , dupSourcePath :: !(Maybe Text)
  , dupStoredPath :: !Text
  }

prepareDiskUpload
  :: ServerState -> Text -> Text -> DriveFormat -> Maybe Text -> Bool -> Text -> Maybe Text -> IO (Either Text DiskUploadPlan)
prepareDiskUpload state clientName name format mPath ephemeral nodeRefText sourcePath =
  case sanitizeDiskName name of
    Left err -> pure (Left err)
    Right safeName -> do
      let placeOn nid = do
            basePath <- nodeBasePathFor state nid
            reserved <- runAction state clientName (DiskUploadReserveId safeName)
            case reserved of
              RespDiskCreated rawId -> do
                let imageId = M.toSqlKey rawId
                defaultPath <- resolveDiskFilePath imageId basePath mPath (T.unpack safeName <> "." <> T.unpack (enumToText format))
                pure (Right (DiskUploadPlan imageId safeName format ephemeral nid defaultPath sourcePath (makeRelativeToBase basePath defaultPath)))
              _ -> pure (Left "Failed to reserve an image ID for upload")
      -- Empty text or capnp's unset-EntityRef default ('byId 0')
      -- both mean "no explicit placement" — defer to the scheduler.
      if T.null nodeRefText || nodeRefText == "0"
        then do
          eNid <- pickNodeForDisk state
          case eNid of
            Left err -> pure (Left err)
            Right nid -> placeOn nid
        else do
          r <- resolveNode (Ref nodeRefText) (ssDbPool state)
          case r of
            Left re -> pure (Left (resolveErrorMessage re))
            Right nidRaw -> placeOn (M.toSqlKey nidRaw)

-- Reserving an upload ID is a mutation even though no image is exposed yet.
newtype DiskUploadReserveId = DiskUploadReserveId Text

instance Action DiskUploadReserveId where
  actionSubsystem _ = SubDisk
  actionCommand _ = "upload-reserve"
  actionEntityName (DiskUploadReserveId name) = Just name
  actionExecute ctx _ = do
    key <- runSqlPool reserveImageId (ssDbPool (acState ctx))
    pure (RespDiskCreated (fromSqlKey key))

data DiskUploadFinalize = DiskUploadFinalize
  { dufPlan :: !DiskUploadPlan
  , dufDigest :: !Text
  }

instance Action DiskUploadFinalize where
  actionSubsystem _ = SubDisk
  actionCommand _ = "upload"
  actionEntityName = Just . dupName . dufPlan
  actionExecute ctx a = handleDiskUploadFinalize (acState ctx) (dufPlan a) (dufDigest a)

handleDiskUploadFinalize :: ServerState -> DiskUploadPlan -> Text -> IO Response
handleDiskUploadFinalize state plan digest = runServerLogging state $ do
  inspected <- liftIO $ getImageInfoViaAgent state (dupNodeId plan) (dupFilePath plan)
  case inspected of
    Left err -> discard err
    Right info
      | iiFormat info /= dupFormat plan -> discard "Uploaded image format differs from requested format"
      | otherwise -> do
          now <- liftIO getCurrentTime
          liftIO $
            runSqlPool
              ( do
                  key <-
                    publishImageWithId
                      (dupImageId plan)
                      DiskImage
                        { diskImageName = dupName plan
                        , diskImageFormat = dupFormat plan
                        , diskImageSize = Just (iiVirtualSize info)
                        , diskImageCreatedAt = now
                        , diskImageBackingImageId = Nothing
                        , diskImageEphemeral = dupEphemeral plan
                        }
                  recordUploadIdentity key digest (dupSourcePath plan)
                  recordDiskImageNode key (dupNodeId plan) (dupStoredPath plan)
                  pure (RespDiskCreated (fromSqlKey key))
              )
              (ssDbPool state)
  where
    discard err = do
      _ <- liftIO $ deleteImageViaAgent state (dupNodeId plan) (dupFilePath plan)
      pure (RespError err)
