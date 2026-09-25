{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Apply.Disk (ApplyDiskCreate (..)) where

import Control.Applicative ((<|>))
import Corvus.Action
import Corvus.Handlers.Apply.Resolve (resolveByName)
import Corvus.Handlers.Apply.Validation (checksumSpecToImport)
import Corvus.Handlers.Disk.Create (DiskCreate (..), DiskRegister (..))
import Corvus.Handlers.Disk.Derive (DiskClone (..), DiskCreateOverlay (..))
import Corvus.Handlers.Disk.Import (DiskImportAction (..))
import Corvus.Model
import Corvus.Node.Image (detectFormatFromPath, isHttpUrl)
import Corvus.Protocol
import Corvus.Schema.Apply (ApplyDisk (..))
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data ApplyDiskCreate = ApplyDiskCreate {adcConfig :: ApplyDisk, adcDiskMap :: Map.Map Text Int64}
instance Action ApplyDiskCreate where
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . adName . adcConfig
  actionExecute ctx a =
    let d = adcConfig a; state = acState ctx; ephem = adEphemeral d; nodeRef = adNode d
     in case (adImport d, adOverlay d, adClone d, adRegister d) of
          (Just importPath, _, _, _) -> actionExecute ctx (DiskImportAction (adName d) importPath (adPath d) (enumToText <$> adFormat d) (checksumSpecToImport <$> adChecksum d) ephem nodeRef)
          (_, _, _, Just registerPath)
            | isHttpUrl registerPath -> pure $ RespError $ "Disk '" <> adName d <> "': register requires a local path, not a URL"
            | otherwise -> do
                let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath registerPath)
                case adBacking d of
                  Nothing -> actionExecute ctx (DiskRegister (adName d) registerPath (Just format) Nothing ephem nodeRef)
                  Just backingName -> do
                    mBackingId <- resolveByName state UniqueDiskImageName (adcDiskMap a) backingName
                    maybe (pure $ RespError $ "backing disk '" <> backingName <> "' not found") (\backingId -> actionExecute ctx $ DiskRegister (adName d) registerPath (Just format) (Just backingId) ephem nodeRef) mBackingId
          (_, Just backingName, _, _) -> do
            mBackingId <- resolveByName state UniqueDiskImageName (adcDiskMap a) backingName
            maybe (pure $ RespError $ "backing disk '" <> backingName <> "' not found") (\backingId -> actionExecute ctx $ DiskCreateOverlay (adName d) backingId (adSizeMb d) (adPath d) ephem) mBackingId
          (_, _, Just cloneName, _) -> do
            mSourceId <- resolveByName state UniqueDiskImageName (adcDiskMap a) cloneName
            maybe (pure $ RespError $ "source disk '" <> cloneName <> "' not found") (\sourceId -> actionExecute ctx $ DiskClone (adName d) sourceId Nothing (adPath d) ephem) mSourceId
          _ -> actionExecute ctx $ DiskCreate (adName d) (fromMaybe FormatQcow2 $ adFormat d) (fromIntegral $ fromMaybe 10240 $ adSizeMb d) (adPath d) ephem nodeRef
