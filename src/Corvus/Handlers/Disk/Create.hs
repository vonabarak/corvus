{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Create disk-image handlers.
module Corvus.Handlers.Disk.Create
  ( DiskCreate (..)
  , DiskRegister (..)
  )
where

import Corvus.Action
import Corvus.Images

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Disk.Agent
  ( createImageViaAgent
  , getImageInfoViaAgent
  , getImageSizeViaAgent
  )
import Corvus.Handlers.Disk.Db (recordDiskImageNode)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, sanitizeDiskName)
import Corvus.Model
import Corvus.Node.Image (ImageInfo (..), ImageResult (..))
import Corvus.Protocol
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql (runSqlPool)
import System.FilePath ((</>))

import Corvus.Handlers.Disk.Placement (nodeBasePathFor, withSelectedDiskNode)

-- | Create a new disk image. An empty/zero @nodeRefText@ means
-- "no explicit placement" — defer to 'pickNodeForDisk'.
handleDiskCreate :: ServerState -> Text -> DriveFormat -> Int64 -> Maybe Text -> Bool -> Text -> IO Response
handleDiskCreate state name format size mPath ephemeral nodeRefText = runServerLogging state $ do
  logInfoN $ "Creating disk image: " <> name <> " (" <> T.pack (show size) <> " bytes)"
  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName ->
      withSelectedDiskNode state nodeRefText $
        createDiskOnNode state safeName format size mPath ephemeral

createDiskOnNode state safeName format size mPath ephemeral nid = do
  basePath <- liftIO $ nodeBasePathFor state nid
  reservedId <- liftIO $ runSqlPool reserveImageId (ssDbPool state)
  let fileName = T.unpack safeName <> "." <> T.unpack (enumToText format)
  filePath <- liftIO $ resolveDiskFilePath reservedId basePath mPath fileName
  result <- liftIO $ createImageViaAgent state nid filePath format size
  case result of
    ImageError err -> do
      logWarnN $ "Failed to create image: " <> err
      pure $ RespError err
    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
    ImageNotFound -> pure $ RespError "Unexpected error during creation"
    ImageSuccess -> do
      actualSize <- liftIO $ getImageSizeViaAgent state nid filePath
      now <- liftIO getCurrentTime
      let storedPath = makeRelativeToBase basePath filePath
      diskId <-
        liftIO $
          runSqlPool
            ( do
                dkey <-
                  publishImageWithId
                    reservedId
                    DiskImage
                      { diskImageName = safeName
                      , diskImageFormat = format
                      , diskImageSize = actualSize
                      , diskImageCreatedAt = now
                      , diskImageBackingImageId = Nothing
                      , diskImageEphemeral = ephemeral
                      }
                recordDiskImageNode dkey nid storedPath
                pure dkey
            )
            (ssDbPool state)
      logInfoN $ "Created disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
      pure $ RespDiskCreated $ fromSqlKey diskId

-- | Register an existing disk image file.
-- Format and size are auto-detected via qemu-img info.
-- If format is provided, it is used instead of auto-detection.
handleDiskRegister
  :: ServerState
  -> Text
  -> Text
  -> Maybe DriveFormat
  -> Maybe Int64
  -> Bool
  -> Text
  -- ^ node ref (name or id); empty / @"0"@ defers to the scheduler
  -> IO Response
handleDiskRegister state name filePath mFormat mBackingDiskId ephemeral nodeRefText =
  case void (sanitizeDiskName name) of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Registering disk image: " <> name <> " at " <> filePath
      withSelectedDiskNode state nodeRefText $
        registerDiskOnNode state name filePath mFormat mBackingDiskId ephemeral

-- | Register a file on one selected node. The caller has already validated
-- the public name and resolved placement; this routine owns only image
-- inspection and persistence, including the concurrent-register recovery.
registerDiskOnNode state name filePath mFormat mBackingDiskId ephemeral nid = do
  basePath <- liftIO $ nodeBasePathFor state nid
  let storedPath = makeRelativeToBase basePath (T.unpack filePath)
      resolvedPath =
        if "/" `isPrefixOf` T.unpack storedPath
          then T.unpack storedPath
          else basePath </> T.unpack storedPath
  inspection <- liftIO $ getImageInfoViaAgent state nid resolvedPath
  case inspection of
    Left err -> pure $ RespError err
    Right info -> do
      let format = fromMaybe (iiFormat info) mFormat
          size = Just (iiVirtualSize info)
      now <- liftIO getCurrentTime
      diskId <-
        liftIO $
          runSqlPool
            ( do
                key <-
                  publishImage
                    DiskImage
                      { diskImageName = name
                      , diskImageFormat = format
                      , diskImageSize = size
                      , diskImageCreatedAt = now
                      , diskImageBackingImageId = fmap toSqlKey mBackingDiskId
                      , diskImageEphemeral = ephemeral
                      }
                recordDiskImageNode key nid storedPath
                pure key
            )
            (ssDbPool state)
      pure $ RespDiskCreated $ fromSqlKey diskId

data DiskCreate = DiskCreate
  { dcrName :: Text
  , dcrFormat :: DriveFormat
  , dcrSize :: Int64
  , dcrPath :: Maybe Text
  , dcrEphemeral :: Bool
  , dcrNodeRef :: Text
  -- ^ Target node reference (name or numeric id). Empty / @"0"@
  -- defers to 'Corvus.Handlers.Scheduler.pickNodeForDisk'.
  }

instance Action DiskCreate where
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . dcrName
  actionExecute ctx a = handleDiskCreate (acState ctx) (dcrName a) (dcrFormat a) (dcrSize a) (dcrPath a) (dcrEphemeral a) (dcrNodeRef a)

data DiskRegister = DiskRegister
  { drgName :: Text
  , drgPath :: Text
  , drgFormat :: Maybe DriveFormat
  , drgBackingDiskId :: Maybe Int64
  , drgEphemeral :: Bool
  , drgNodeRef :: Text
  -- ^ Node that hosts the file. Empty / @"0"@ defers to
  -- 'Corvus.Handlers.Scheduler.pickNodeForDisk'.
  }

instance Action DiskRegister where
  actionSubsystem _ = SubDisk
  actionCommand _ = "register"
  actionEntityName = Just . drgName
  actionExecute ctx a = handleDiskRegister (acState ctx) (drgName a) (drgPath a) (drgFormat a) (drgBackingDiskId a) (drgEphemeral a) (drgNodeRef a)
