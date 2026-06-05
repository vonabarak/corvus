{-# LANGUAGE DeriveGeneric #-}

-- | Disk and snapshot response data.
module Corvus.Protocol.Disk
  ( DiskImageInfo (..)
  , DiskImagePlacement (..)
  , SnapshotInfo (..)
  )
where

import Corvus.Model (DriveFormat)
import Corvus.Protocol.JsonOptions (innerOptions)
import Corvus.Protocol.NamedRef (NamedRef)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Per-node placement of a logical disk image — the on-disk
-- @file_path@ on a specific node. A logical 'DiskImage' may
-- have zero, one, or many placements (zero is unusual but
-- possible while an import is in flight; multi is the
-- intentional state once Phase 3 ships and an operator has
-- replicated an image via @rsync@ + @crv disk register@).
data DiskImagePlacement = DiskImagePlacement
  { dipNode :: !NamedRef
  , dipFilePath :: !Text
  }
  deriving (Eq, Show, Generic)

-- | Disk image info for list/show view
data DiskImageInfo = DiskImageInfo
  { diiId :: !Int64
  , diiName :: !Text
  , diiPlacements :: ![DiskImagePlacement]
  -- ^ Per-node placements: where the on-disk file actually
  -- lives. Multi-node deployments have one entry per node the
  -- image has been replicated to; single-node deployments have
  -- exactly one entry.
  , diiFormat :: !DriveFormat
  , diiSizeMb :: !(Maybe Int)
  , diiCreatedAt :: !UTCTime
  , diiAttachedTo :: ![NamedRef]
  -- ^ VMs this disk is attached to.
  , diiBackingImage :: !(Maybe NamedRef)
  -- ^ Backing image for overlays; 'Nothing' for standalone disks.
  , diiEphemeral :: !Bool
  -- ^ Ephemeral disks are auto-deleted with the VM they are attached
  -- to. Cloud-init ISOs and template-instantiated disks default to
  -- ephemeral; everything else defaults to non-ephemeral.
  }
  deriving (Eq, Show, Generic)

-- | Snapshot info
data SnapshotInfo = SnapshotInfo
  { sniId :: !Int64
  , sniName :: !Text
  , sniCreatedAt :: !UTCTime
  , sniSizeMb :: !(Maybe Int)
  , sniLive :: !Bool
  -- ^ Whether this snapshot was taken via QMP against a running
  -- VM (@True@) vs offline via @qemu-img snapshot -c@ (@False@).
  , sniQuiesced :: !Bool
  -- ^ Whether QGA @guest-fsfreeze-freeze@ was active when the
  -- snapshot was stamped. Implies filesystem-level consistency.
  }
  deriving (Eq, Show, Generic)

instance ToJSON DiskImageInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON DiskImagePlacement where
  toJSON = genericToJSON innerOptions

instance ToJSON SnapshotInfo where
  toJSON = genericToJSON innerOptions
