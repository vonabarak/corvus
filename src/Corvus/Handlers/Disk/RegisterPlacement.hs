{-# LANGUAGE OverloadedStrings #-}

-- | Register a replica of a particular version without publishing new tags.
module Corvus.Handlers.Disk.RegisterPlacement (DiskRegisterPlacement (..)) where

import Corvus.Action
import Corvus.Handlers.Disk.Agent (getImageInfoViaAgent)
import Corvus.Handlers.Disk.Db (recordDiskImageNode)
import Corvus.Handlers.Disk.Path (makeRelativeToBase)
import Corvus.Handlers.Disk.Placement (nodeBasePathFor)
import Corvus.Model
import Corvus.Node.Image (ImageInfo (..))
import Corvus.Protocol
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (get)
import Database.Persist.Sql (runSqlPool)
import System.FilePath (isAbsolute, (</>))

data DiskRegisterPlacement = DiskRegisterPlacement Int64 Int64 Text

instance Action DiskRegisterPlacement where
  actionSubsystem _ = SubDisk
  actionCommand _ = "registerPlacement"
  actionEntityId (DiskRegisterPlacement imageId _ _) = Just (fromIntegral imageId)
  actionExecute ctx (DiskRegisterPlacement imageId nodeId path) = do
    let state = acState ctx
        key = toSqlKey imageId
        nid = toSqlKey nodeId
    image <- runSqlPool (get key) (ssDbPool state)
    case image of
      Nothing -> pure RespDiskNotFound
      Just disk -> do
        base <- nodeBasePathFor state nid
        let requested = T.unpack path
            absolute = if isAbsolute requested then requested else base </> requested
        inspected <- getImageInfoViaAgent state nid absolute
        case inspected of
          Left err -> pure (RespError err)
          Right info
            | iiFormat info /= diskImageFormat disk -> pure (RespError "Replica format differs from image version")
            | maybe False (/= iiVirtualSize info) (diskImageSize disk) -> pure (RespError "Replica size differs from image version")
            | otherwise -> do
                runSqlPool (recordDiskImageNode key nid (makeRelativeToBase base absolute)) (ssDbPool state)
                pure RespDiskOk
