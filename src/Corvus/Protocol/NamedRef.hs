{-# LANGUAGE DeriveGeneric #-}

-- | Output-only reference to another entity, carried as @{id, name}@.
--
-- Used wherever one Corvus entity's response refers to another (a
-- drive's disk image, a VM's node, a NIC's network, …) so JSON
-- consumers and the WebUI can render a link plus label without a
-- follow-up RPC. Mirrors @Common.NamedRef@ in
-- @schema/common.capnp@.
--
-- Distinct from 'Corvus.Wire.Common.EntityRef', which is the
-- input-side union of @id @or@ name@ used to look an entity up.
--
-- See @CLAUDE.md ## Project Rules / Cross-entity references@ for
-- the convention this type encodes.
module Corvus.Protocol.NamedRef
  ( NamedRef (..)
  )
where

import Corvus.Protocol.JsonOptions (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | An entity's persistent-key id together with its display name.
-- Optional references use 'Maybe NamedRef'; the wire sentinel
-- (@id == 0@) is translated at the converter boundary, so consumers
-- only ever see @Just@ or @Nothing@.
data NamedRef = NamedRef
  { nrId :: !Int64
  , nrName :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON NamedRef where
  toJSON = genericToJSON innerOptions
