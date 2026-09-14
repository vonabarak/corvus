{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generic entity reference resolution and name validation.
module Corvus.Handlers.Resolve
  ( -- * Resolution
    ResolveError (..)
  , resolveErrorMessage
  , resolveRef
  , resolveVm
  , resolveDisk
  , resolveNetwork
  , resolveSshKey
  , resolveTemplate
  , resolveNode
  , resolveSnapshot

    -- * Validation
  , validateName
  )
where

import Corvus.Model
  ( DiskImage
  , DiskImageId
  , Entity (..)
  , Key
  , Network
  , Node
  , SharedDir (..)
  , SharedDirId
  , Snapshot (..)
  , SnapshotId
  , SshKey
  , TemplateVm
  , Unique (..)
  , Vm
  , VmId
  , fromSqlKey
  , toSqlKey
  )
import qualified Corvus.Model as M
import Corvus.Protocol (Ref (..))
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Filter, (==.))
import Database.Persist.Sql
  ( PersistEntity
  , PersistEntityBackend
  , SqlBackend
  , ToBackendKey
  , get
  , getBy
  , runSqlPool
  , selectList
  )
import Text.Read (readMaybe)

-- | A structured failure to resolve an entity reference, carrying the
-- entity kind and the offending ref so the caller can both render a
-- human message and pick the right wire error code.
data ResolveError
  = RefNotFound
      { reEntity :: Text
      , reRef :: Text
      }
  | RefAmbiguous
      { reEntity :: Text
      , reRef :: Text
      , reCount :: Int
      }
  deriving (Eq, Show)

-- | Render a 'ResolveError' to its canonical human-readable message.
resolveErrorMessage :: ResolveError -> Text
resolveErrorMessage (RefNotFound entity ref) =
  if T.all (`elem` ['0' .. '9']) ref
    then entity <> " #" <> ref <> " not found"
    else entity <> " '" <> ref <> "' not found"
resolveErrorMessage (RefAmbiguous entity ref count) =
  entity
    <> " '"
    <> ref
    <> "' is ambiguous: "
    <> T.pack (show count)
    <> " matches across nodes; use the numeric id"

-- | Resolve a 'Ref' to a database key.
-- If the ref text parses as an integer, look up by ID.
-- Otherwise, look up by the entity's unique name constraint.
resolveRef
  :: forall record
   . ( PersistEntity record
     , PersistEntityBackend record ~ SqlBackend
     , ToBackendKey SqlBackend record
     )
  => (Text -> Unique record)
  -- ^ Unique constraint constructor (e.g., 'UniqueName')
  -> Text
  -- ^ Entity type name for error messages
  -> Ref
  -> Pool SqlBackend
  -> IO (Either ResolveError Int64)
resolveRef mkUnique typeName (Ref refText) pool =
  case readMaybe (T.unpack refText) of
    Just numId -> do
      mEntity <- runSqlPool (get (toSqlKey numId :: Key record)) pool
      case mEntity of
        Nothing -> pure $ Left $ RefNotFound typeName refText
        Just _ -> pure $ Right numId
    Nothing -> do
      mEntity <- runSqlPool (getBy (mkUnique refText)) pool
      case mEntity of
        Nothing -> pure $ Left $ RefNotFound typeName refText
        Just (Entity key _) -> pure $ Right (fromSqlKey key)

-- | Resolve a reference whose textual form is a (non-unique) name
-- column. Numeric → id lookup. Otherwise → 'selectList' with the
-- caller-supplied filter, expecting exactly one match. Used by
-- 'resolveVm' and 'resolveNetwork' where the cluster-wide
-- 'UniqueName' / 'UniqueNetworkName' constraints are now scoped
-- to a node (multi-node refactor) and the name alone may match
-- zero, one, or many rows.
resolveByName
  :: forall record
   . ( PersistEntity record
     , PersistEntityBackend record ~ SqlBackend
     , ToBackendKey SqlBackend record
     )
  => Text
  -- ^ Entity type name for error messages
  -> (Text -> [Filter record])
  -- ^ Build the name-filter for 'selectList'
  -> Ref
  -> Pool SqlBackend
  -> IO (Either ResolveError Int64)
resolveByName typeName mkFilter (Ref refText) pool =
  case readMaybe (T.unpack refText) of
    Just numId -> do
      mEntity <- runSqlPool (get (toSqlKey numId :: Key record)) pool
      case mEntity of
        Nothing -> pure $ Left $ RefNotFound typeName refText
        Just _ -> pure $ Right numId
    Nothing -> do
      entities <- runSqlPool (selectList (mkFilter refText) []) pool
      case entities of
        [] -> pure $ Left $ RefNotFound typeName refText
        [Entity key _] -> pure $ Right (fromSqlKey key)
        many -> pure $ Left $ RefAmbiguous typeName refText (length many)

-- | Resolve a VM reference. Cluster-wide name lookup; rejects
-- ambiguous matches because 'UniqueVmNamePerNode' only guarantees
-- uniqueness within a node.
resolveVm :: Ref -> Pool SqlBackend -> IO (Either ResolveError Int64)
resolveVm = resolveByName @Vm "VM" (\n -> [M.VmName ==. n])

-- | Resolve a disk image reference.
resolveDisk :: Ref -> Pool SqlBackend -> IO (Either ResolveError Int64)
resolveDisk = resolveRef @DiskImage UniqueDiskImageName "Disk image"

-- | Resolve a network reference. Same per-node-uniqueness pattern
-- as 'resolveVm'.
resolveNetwork :: Ref -> Pool SqlBackend -> IO (Either ResolveError Int64)
resolveNetwork = resolveByName @Network "Network" (\n -> [M.NetworkName ==. n])

-- | Resolve an SSH key reference.
resolveSshKey :: Ref -> Pool SqlBackend -> IO (Either ResolveError Int64)
resolveSshKey = resolveRef @SshKey UniqueSshKeyName "SSH key"

-- | Resolve a template reference.
resolveTemplate :: Ref -> Pool SqlBackend -> IO (Either ResolveError Int64)
resolveTemplate = resolveRef @TemplateVm UniqueTemplateVmName "Template"

-- | Resolve a node reference. Node names are globally unique
-- (the data model bootstraps the cluster, not per-node), so the
-- single-field 'UniqueNodeName' constraint applies.
resolveNode :: Ref -> Pool SqlBackend -> IO (Either ResolveError Int64)
resolveNode = resolveRef @Node UniqueNodeName "Node"

-- | Resolve a snapshot reference within a disk.
-- If numeric, look up by ID and verify it belongs to the given disk.
-- Otherwise, look up by (diskImageId, name) unique constraint.
resolveSnapshot :: Ref -> Int64 -> Pool SqlBackend -> IO (Either ResolveError Int64)
resolveSnapshot (Ref refText) diskId pool =
  case readMaybe (T.unpack refText) of
    Just numId -> do
      mSnap <- runSqlPool (get (toSqlKey numId :: SnapshotId)) pool
      case mSnap of
        Nothing -> pure $ Left $ RefNotFound "Snapshot" refText
        Just snap ->
          if snapshotDiskImageId snap == toSqlKey diskId
            then pure $ Right numId
            else pure $ Left $ RefNotFound "Snapshot" refText
    Nothing -> do
      mSnap <- runSqlPool (getBy (UniqueSnapshot (toSqlKey diskId :: DiskImageId) refText)) pool
      case mSnap of
        Nothing -> pure $ Left $ RefNotFound "Snapshot" refText
        Just (Entity key _) -> pure $ Right (fromSqlKey key)

-- | Reject all-digit names to prevent ambiguity with numeric ID refs.
validateName :: Text -> Text -> Either Text ()
validateName typeName name
  | T.null name = Left $ typeName <> " name cannot be empty"
  | T.all (`elem` ['0' .. '9']) name = Left $ typeName <> " name cannot be all digits (would be ambiguous with numeric IDs)"
  | otherwise = Right ()
