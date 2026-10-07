{-# LANGUAGE OverloadedStrings #-}

-- | Transactional image publication and selector lookup. Image names are
-- families; only tags, never image records, are replaced by publication.
module Corvus.Images
  ( reserveImageId
  , publishImageWithId
  , publishImage
  , resolveImage
  , imageByName
  , imageTags
  , assignImageTag
  , removeImageTag
  , recordImportIdentity
  , matchesImportIdentity
  , deleteImageTags
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Corvus.DiskSelector
import Corvus.Model
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (Single (..), SqlPersistT, rawSql)
import Database.Persist.SqlBackend (getRDBMS)

checked :: Either Text a -> SqlPersistT IO a
checked = either (fail . T.unpack) pure

resolveImage :: DiskSelector -> SqlPersistT IO (Maybe DiskImageId)
resolveImage (ImageId imageId) = do
  let key = toSqlKey imageId
  found <- get key
  pure (key <$ found)
resolveImage (ImageTag name tag) = do
  found <- getBy (UniqueDiskImageTag name tag)
  pure (diskImageTagDiskImageId . entityVal <$> found)

imageByName :: Text -> SqlPersistT IO (Maybe (Entity DiskImage))
imageByName value = do
  selector <- checked (parseDiskSelector value)
  key <- resolveImage selector
  case key of
    Nothing -> pure Nothing
    Just k -> fmap (Entity k) <$> get k

imageTags :: DiskImageId -> SqlPersistT IO [Text]
imageTags key = map (diskImageTagTag . entityVal) <$> selectList [DiskImageTagDiskImageId ==. key] [Asc DiskImageTagTag]

assignImageTag :: DiskImageId -> Text -> SqlPersistT IO ()
assignImageTag key tag = do
  checked (validateImageTag tag)
  disk <- getJust key
  lockImageName (diskImageName disk)
  _ <- upsert (DiskImageTag (diskImageName disk) tag key) [DiskImageTagDiskImageId =. key]
  pure ()

-- | Consume an ID without publishing an image. The insert/delete pair stays
-- inside one transaction, so readers never observe a reservation. Both
-- PostgreSQL sequences and SQLite AUTOINCREMENT retain the consumed ID.
reserveImageId :: SqlPersistT IO DiskImageId
reserveImageId = do
  now <- liftIO getCurrentTime
  key <- insert (DiskImage "_id-reservation" FormatRaw Nothing now Nothing True)
  delete key
  pure key

publishImage :: DiskImage -> SqlPersistT IO DiskImageId
publishImage disk = do
  (name, tag) <- checked (publicationSelector (diskImageName disk))
  lockImageName name
  key <- insert disk {diskImageName = name}
  publishTags key tag
  pure key

-- | Publish using an ID reserved before writing the node-side file.
publishImageWithId :: DiskImageId -> DiskImage -> SqlPersistT IO DiskImageId
publishImageWithId key disk = do
  (name, tag) <- checked (publicationSelector (diskImageName disk))
  lockImageName name
  insertKey key disk {diskImageName = name}
  publishTags key tag
  pure key

publishTags :: DiskImageId -> Text -> SqlPersistT IO ()
publishTags key tag = do
  assignImageTag key tag
  assignImageTag key "latest"

removeImageTag :: DiskImageId -> Text -> SqlPersistT IO ()
removeImageTag key tag = do
  checked (validateImageTag tag)
  unless (tag /= "latest") (fail "The latest tag cannot be removed; move it or delete its image")
  disk <- getJust key
  lockImageName (diskImageName disk)
  deleteWhere [DiskImageTagDiskImageId ==. key, DiskImageTagTag ==. tag]

-- | Call before deleting the image, in the same database transaction.
deleteImageTags :: DiskImageId -> SqlPersistT IO ()
deleteImageTags key = do
  disk <- getJust key
  lockImageName (diskImageName disk)
  latest <- getBy (UniqueDiskImageTag (diskImageName disk) "latest")
  deleteWhere [DiskImageTagDiskImageId ==. key]
  forM_ latest $ \(Entity _ row) ->
    if diskImageTagDiskImageId row /= key
      then pure ()
      else do
        replacement <- selectFirst [DiskImageName ==. diskImageName disk, DiskImageId !=. key] [Desc DiskImageCreatedAt, Desc DiskImageId]
        forM_ replacement $ \(Entity k _) -> assignImageTag k "latest"

-- Serialize tag moves and deletion within a family on PostgreSQL. SQLite
-- serializes writers at the database level. Advisory hash collisions merely
-- serialize two unrelated names; they cannot compromise uniqueness.
lockImageName :: Text -> SqlPersistT IO ()
lockImageName name = do
  backend <- getRDBMS
  when (backend == "postgresql") $ do
    _ <- rawSql "SELECT 1 FROM pg_advisory_xact_lock(hashtext(?))" [PersistText name] :: SqlPersistT IO [Single Int]
    pure ()

-- | Called in the same transaction as publication and placement.
recordImportIdentity :: DiskImageId -> (Text, Text, Text) -> Text -> SqlPersistT IO ()
recordImportIdentity key (algorithm, digest, target) url =
  insert_ $ DiskImageImportIdentity key (T.toLower algorithm) (T.toLower digest) (T.toLower target) (Just url)

matchesImportIdentity :: DiskImageId -> DriveFormat -> (Text, Text, Text) -> SqlPersistT IO Bool
matchesImportIdentity key format (algorithm, digest, target) = do
  image <- get key
  identity <- getBy $ UniqueDiskImageImportIdentity key
  pure $ case (image, identity) of
    (Just disk, Just (Entity _ stored)) ->
      diskImageFormat disk == format
        && diskImageImportIdentityAlgorithm stored == T.toLower algorithm
        && diskImageImportIdentityDigest stored == T.toLower digest
        && diskImageImportIdentityTarget stored == T.toLower target
    _ -> False
