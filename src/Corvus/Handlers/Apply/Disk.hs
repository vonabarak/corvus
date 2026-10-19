{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Apply.Disk (ApplyDiskCreate (..), matchesDiskImport) where

import Control.Applicative ((<|>))
import Corvus.Action
import Corvus.Handlers.Apply.Resolve (resolveDiskName)
import Corvus.Handlers.Apply.Validation (checksumSpecToImport)
import Corvus.Handlers.Disk.Create (DiskCreate (..), DiskRegister (..))
import Corvus.Handlers.Disk.Derive (DiskClone (..), DiskCreateOverlay (..))
import Corvus.Handlers.Disk.Import (DiskImportAction (..))
import Corvus.ImageOperationGuard (ImageAccess (..))
import Corvus.Images (matchesImportIdentity)
import Corvus.Model
import Corvus.Node.Image (detectFormatFromPath, detectFormatFromUrl, isHttpUrl)
import Corvus.Protocol
import Corvus.Schema.Apply (ApplyDisk (..))
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Database.Persist.Sql (runSqlPool)

data ApplyDiskCreate = ApplyDiskCreate {adcConfig :: ApplyDisk, adcDiskMap :: Map.Map Text Int64}
instance Action ApplyDiskCreate where
  actionImageResources state a = do
    let d = adcConfig a
    sources <- mapM (resolveDiskName state (adcDiskMap a)) (catMaybes [adOverlay d, adClone d, adBacking d])
    resources <- concat <$> mapM (imageResources state SharedImage) (catMaybes sources)
    pure $ resources ++ familyResource (adName d)
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . adName . adcConfig
  actionExecute ctx a =
    let d = adcConfig a; state = acState ctx; ephem = adEphemeral d; nodeRef = adNode d
     in case (adImport d, adOverlay d, adClone d, adRegister d) of
          (Just importPath, _, _, _) -> executeWithImageLeases ctx (DiskImportAction (adName d) importPath (adPath d) (enumToText <$> adFormat d) (checksumSpecToImport <$> adChecksum d) ephem nodeRef)
          (_, _, _, Just registerPath)
            | isHttpUrl registerPath -> pure $ RespError $ "Disk '" <> adName d <> "': register requires a local path, not a URL"
            | otherwise -> do
                let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath registerPath)
                case adBacking d of
                  Nothing -> executeWithImageLeases ctx (DiskRegister (adName d) registerPath (Just format) Nothing ephem nodeRef)
                  Just backingName -> do
                    mBackingId <- resolveDiskName state (adcDiskMap a) backingName
                    maybe (pure $ RespError $ "backing disk '" <> backingName <> "' not found") (\backingId -> executeWithImageLeases ctx $ DiskRegister (adName d) registerPath (Just format) (Just backingId) ephem nodeRef) mBackingId
          (_, Just backingName, _, _) -> do
            mBackingId <- resolveDiskName state (adcDiskMap a) backingName
            maybe (pure $ RespError $ "backing disk '" <> backingName <> "' not found") (\backingId -> executeWithImageLeases ctx $ DiskCreateOverlay (adName d) backingId (adSize d) (adPath d) ephem Nothing) mBackingId
          (_, _, Just cloneName, _) -> do
            mSourceId <- resolveDiskName state (adcDiskMap a) cloneName
            maybe (pure $ RespError $ "source disk '" <> cloneName <> "' not found") (\sourceId -> executeWithImageLeases ctx $ DiskClone (adName d) sourceId Nothing (adPath d) ephem Nothing) mSourceId
          _ -> executeWithImageLeases ctx $ DiskCreate (adName d) (fromMaybe FormatQcow2 $ adFormat d) (fromIntegral $ fromMaybe 10240 $ adSize d) (adPath d) ephem nodeRef

-- | Compare the verified import identity of the currently selected version.
matchesDiskImport :: ServerState -> ApplyDisk -> Int64 -> IO Bool
matchesDiskImport state disk imageId =
  case (adImport disk, adChecksum disk) of
    (Just source, Just checksum) ->
      case adFormat disk <|> detectFormatFromUrl source of
        Just format -> runSqlPool (matchesImportIdentity (toSqlKey imageId) format $ checksumSpecToImport checksum) (ssDbPool state)
        Nothing -> pure False
    _ -> pure False
