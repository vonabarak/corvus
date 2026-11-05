{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Apply.Disk (ApplyDiskCreate (..), prepareApplyDisk, matchesApplyDisk) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Corvus.Action
import Corvus.Handlers.Apply.Resolve (resolveDiskName)
import Corvus.Handlers.Apply.Validation (checksumSpecToImport)
import Corvus.Handlers.Disk.Agent (sourceIdentityViaAgent)
import Corvus.Handlers.Disk.Create (DiskCreate (..), DiskRegister (..))
import Corvus.Handlers.Disk.Derive (DiskClone (..), DiskCreateOverlay (..))
import Corvus.Handlers.Disk.Import (DiskImportAction (..))
import Corvus.Handlers.Disk.Path (makeRelativeToBase)
import Corvus.Handlers.Disk.Placement (nodeBasePathFor)
import Corvus.Handlers.Scheduler (pickNodeForDisk)
import Corvus.Images.ApplyIdentity (matchesApplyIdentity)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist
import System.FilePath (isRelative, normalise, (</>))

import Corvus.Model
import Corvus.Node.Image (detectFormatFromPath, detectFormatFromUrl, isHttpUrl)
import Corvus.Protocol
import Corvus.Schema.Apply (ApplyDisk (..))
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import Data.Text (Text)
import Database.Persist.Sql (runSqlPool)

data ApplyDiskCreate = ApplyDiskCreate {adcConfig :: ApplyDisk, adcDiskMap :: Map.Map Text Int64, adcIdentity :: Maybe Text}
instance Action ApplyDiskCreate where
  actionResources state a = do
    let d = adcConfig a
    sources <- mapM (resolveDiskName state (adcDiskMap a)) (catMaybes [adOverlay d, adClone d, adBacking d])
    resources <- concat <$> mapM (imageResources state SharedAccess) (catMaybes sources)
    pure $ resources ++ familyResource (adName d)
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . adName . adcConfig
  actionExecute ctx a =
    let d = adcConfig a; state = acState ctx; ephem = adEphemeral d; nodeRef = adNode d
     in case (adImport d, adOverlay d, adClone d, adRegister d) of
          (Just importPath, _, _, _) -> executeWithOperationLeases ctx (DiskImportAction (adcIdentity a) (adName d) importPath (adPath d) (enumToText <$> adFormat d) (checksumSpecToImport <$> adChecksum d) ephem nodeRef)
          (_, _, _, Just registerPath)
            | isHttpUrl registerPath -> pure $ RespError $ "Disk '" <> adName d <> "': register requires a local path, not a URL"
            | otherwise -> do
                let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath registerPath)
                case adBacking d of
                  Nothing -> executeWithOperationLeases ctx (DiskRegister (adcIdentity a) (adName d) registerPath (Just format) Nothing ephem nodeRef)
                  Just backingName -> do
                    mBackingId <- resolveDiskName state (adcDiskMap a) backingName
                    maybe (pure $ RespError $ "backing disk '" <> backingName <> "' not found") (\backingId -> executeWithOperationLeases ctx $ DiskRegister (adcIdentity a) (adName d) registerPath (Just format) (Just backingId) ephem nodeRef) mBackingId
          (_, Just backingName, _, _) -> do
            mBackingId <- resolveDiskName state (adcDiskMap a) backingName
            maybe (pure $ RespError $ "backing disk '" <> backingName <> "' not found") (\backingId -> executeWithOperationLeases ctx $ DiskCreateOverlay (adcIdentity a) (adName d) backingId (adSize d) (adPath d) ephem (readNode nodeRef)) mBackingId
          (_, _, Just cloneName, _) -> do
            mSourceId <- resolveDiskName state (adcDiskMap a) cloneName
            maybe (pure $ RespError $ "source disk '" <> cloneName <> "' not found") (\sourceId -> executeWithOperationLeases ctx $ DiskClone (adcIdentity a) (adName d) sourceId Nothing (adPath d) ephem (readNode nodeRef)) mSourceId
          _ -> executeWithOperationLeases ctx $ DiskCreate (adcIdentity a) (adName d) (fromMaybe FormatQcow2 $ adFormat d) (fromIntegral $ fromMaybe 10240 $ adSize d) (adPath d) ephem nodeRef

readNode :: Text -> Maybe NodeId
readNode ref = case reads (T.unpack ref) of [(x, "")] -> Just (toSqlKey x); _ -> Nothing

-- Resolve inputs before comparison, including inputs that have disappeared.
-- Unspecified placement reuses an existing placement rather than rescheduling.
prepareApplyDisk :: ServerState -> ApplyDisk -> Map.Map Text Int64 -> Maybe Int64 -> IO (Either Text (ApplyDisk, Text))
prepareApplyDisk state d pins existing = do
  sources <- mapM (resolveDiskName state pins) (catMaybes [adOverlay d, adClone d, adBacking d])
  if Nothing `elem` sources
    then pure $ Left "Apply disk source not found"
    else do
      placements <- runSqlPool (selectList [DiskImageNodeDiskImageId <-. map toSqlKey (maybeToList existing ++ catMaybes sources)] [Asc DiskImageNodeId]) (ssDbPool state)
      let preferred = case existing of
            Nothing -> placements
            Just eid -> filter ((== toSqlKey eid) . diskImageNodeDiskImageId . entityVal) placements ++ placements
          automatic = case preferred of
            Entity _ placement : _ -> pure $ Right $ diskImageNodeNodeId placement
            [] -> pickNodeForDisk state
      pinned <- pinNode state (adNode d) automatic
      case pinned of
        Left r -> pure $ Left $ fromMaybe "Disk placement failed" (snd (classifyResponse r))
        Right node -> do
          let nid = fromMaybe (toSqlKey 0) (readNode node)
          base <- nodeBasePathFor state nid
          let absolute p = T.pack $ normalise $ if isRelative (T.unpack p) then base </> T.unpack p else T.unpack p
              local = case adImport d of Just path | not (isHttpUrl path) -> Just (absolute path); _ -> Nothing
          content <- maybe (pure $ Right Nothing) (fmap (fmap Just) . sourceIdentityViaAgent state nid . T.unpack) local
          pure $ do
            digest <- content
            let kind = case (adImport d, adOverlay d, adClone d, adRegister d) of (Just _, _, _, _) -> "import"; (_, Just _, _, _) -> "overlay"; (_, _, Just _, _) -> "clone"; (_, _, _, Just _) -> "register"; _ -> "create" :: Text
                format
                  | kind == "import" = adFormat d <|> (adImport d >>= detectFormatFromUrl) <|> (adImport d >>= detectFormatFromPath)
                  | kind == "clone" = Nothing
                  | otherwise = Just (fromMaybe FormatQcow2 (adFormat d))
                size = if kind == "clone" || kind == "import" || kind == "register" then Nothing else Just (fromMaybe 10240 (adSize d))
                checksum = fmap (\c -> let (alg, value, target) = checksumSpecToImport c in (alg, T.toLower value, target)) (adChecksum d)
                recipe = TE.decodeUtf8 $ LBS.toStrict $ encode $ object ["version" .= (1 :: Int), "kind" .= kind, "format" .= format, "size" .= size, "path" .= (absolute <$> adPath d), "register" .= (absolute <$> adRegister d), "sources" .= sources, "content" .= digest, "checksum" .= checksum, "ephemeral" .= adEphemeral d, "node" .= node]
            Right (d {adNode = node, adImport = local <|> adImport d}, recipe)

matchesApplyDisk :: ServerState -> ApplyDisk -> Map.Map Text Int64 -> Text -> Int64 -> IO Bool
matchesApplyDisk state d pins recipe imageId = do
  let key = toSqlKey imageId
      nid = fromMaybe (toSqlKey 0) (readNode (adNode d))
  base <- nodeBasePathFor state nid
  backing <- traverse (resolveDiskName state pins) (adBacking d)
  runSqlPool
    ( do
        disk <- get key
        placement <- getBy (UniqueDiskImageOnNode key nid)
        let metadata = maybe False (\x -> diskImageEphemeral x == adEphemeral d) disk && isJust placement
        case adRegister d of
          Just path -> pure $ metadata && maybe False (\x -> diskImageFormat x == fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath path) && diskImageBackingImageId x == (toSqlKey <$> join backing)) disk && maybe False (\(Entity _ x) -> normalise (T.unpack (diskImageNodeFilePath x)) == normalise (T.unpack (makeRelativeToBase base (if isRelative (T.unpack path) then base </> T.unpack path else T.unpack path)))) placement
          Nothing -> do
            recorded <- matchesApplyIdentity key recipe
            pure $ metadata && recorded
    )
    (ssDbPool state)
