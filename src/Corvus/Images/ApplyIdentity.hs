module Corvus.Images.ApplyIdentity (recordApplyIdentity, matchesApplyIdentity) where

import Corvus.Model
import qualified Crypto.Hash as Hash
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist
import Database.Persist.Sql

-- The recipe is canonical text. The baseline never examines output bytes:
-- guest writes are allowed; changes to disk configuration invalidate it.
baseline :: DiskImage -> Text
baseline d = T.pack $ show (diskImageFormat d, diskImageSize d, diskImageBackingImageId d, diskImageEphemeral d)

recordApplyIdentity :: Maybe Text -> DiskImageId -> SqlPersistT IO ()
recordApplyIdentity Nothing _ = pure ()
recordApplyIdentity (Just recipe) key = do
  d <- getJust key
  paths <- placements key
  insert_ $ DiskImageApplyIdentity key (fingerprint recipe) (manifest recipe d paths)

matchesApplyIdentity :: DiskImageId -> Text -> SqlPersistT IO Bool
matchesApplyIdentity key recipe = do
  identity <- getBy (UniqueDiskImageApplyIdentity key)
  disk <- get key
  paths <- placements key
  pure $ case (identity, disk) of
    (Just (Entity _ x), Just d) -> case (eitherDecodeStrict (TE.encodeUtf8 (diskImageApplyIdentityInputs x)) :: Either String (Text, Text, [(Int64, Text)])) of
      Right (savedRecipe, savedBaseline, savedPaths) -> diskImageApplyIdentityFingerprint x == fingerprint recipe && savedRecipe == recipe && savedBaseline == baseline d && not (null savedPaths) && all (`elem` paths) savedPaths
      Left _ -> False
    _ -> False

fingerprint :: Text -> Text
fingerprint recipe = T.pack $ show (Hash.hash (TE.encodeUtf8 recipe) :: Hash.Digest Hash.SHA256)
placements :: DiskImageId -> SqlPersistT IO [(Int64, Text)]
placements key = map (\(Entity _ p) -> (fromSqlKey (diskImageNodeNodeId p), diskImageNodeFilePath p)) <$> selectList [DiskImageNodeDiskImageId ==. key] [Asc DiskImageNodeId]

manifest :: Text -> DiskImage -> [(Int64, Text)] -> Text
manifest recipe d paths = TE.decodeUtf8 $ LBS.toStrict $ encode (recipe, baseline d, paths)
