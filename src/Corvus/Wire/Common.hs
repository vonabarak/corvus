{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Conversion between Haskell domain types and the Cap'n Proto
-- structs defined in @schema/common.capnp@ ('EntityRef',
-- 'StatusInfo', 'ViewGrant').
module Corvus.Wire.Common
  ( -- * Entity references
    EntityRef (..)
  , toCapnpEntityRef
  , fromCapnpEntityRef
  , entityRefFromText

    -- * Status info
  , toCapnpStatusInfo
  , fromCapnpStatusInfo

    -- * View grant
  , ViewGrant (..)
  , toCapnpViewGrant
  , fromCapnpViewGrant
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Common as CGCommon
import qualified Corvus.Protocol as P
import Corvus.Wire.Errors (WireError (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal, signed)

-- ---------------------------------------------------------------------
-- Entity references
-- ---------------------------------------------------------------------

-- | Typed representation of an entity reference. Mirrors the union in
-- 'Capnp.Gen.Common.EntityRef' but with a Haskell-natural sum type.
data EntityRef
  = -- | Numeric persistent-key id.
    RefById !Int64
  | -- | Symbolic name (the entity's unique 'name' column).
    RefByName !Text
  deriving (Eq, Show)

toCapnpEntityRef :: EntityRef -> C.Parsed CGCommon.EntityRef
toCapnpEntityRef = \case
  RefById n -> CGCommon.EntityRef {CGCommon.union' = CGCommon.EntityRef'id n}
  RefByName t -> CGCommon.EntityRef {CGCommon.union' = CGCommon.EntityRef'name t}

fromCapnpEntityRef :: C.Parsed CGCommon.EntityRef -> Either WireError EntityRef
fromCapnpEntityRef CGCommon.EntityRef {CGCommon.union' = u} = case u of
  CGCommon.EntityRef'id n -> Right (RefById n)
  CGCommon.EntityRef'name t -> Right (RefByName t)
  CGCommon.EntityRef'unknown' _ -> Left (WireMissingUnionVariant "EntityRef")

-- | Client-side CLI heuristic: a bare token that parses as a
-- non-negative integer is treated as an id, otherwise as a name.
-- Library users wanting unambiguous behaviour should construct
-- 'RefById' / 'RefByName' directly instead of going through this.
entityRefFromText :: Text -> EntityRef
entityRefFromText t =
  case signed decimal t of
    Right (n, rest) | T.null rest, n >= 0 -> RefById n
    _ -> RefByName t

-- ---------------------------------------------------------------------
-- Status info
-- ---------------------------------------------------------------------

toCapnpStatusInfo :: P.StatusInfo -> C.Parsed CGCommon.StatusInfo
toCapnpStatusInfo P.StatusInfo {..} =
  CGCommon.StatusInfo
    { CGCommon.uptimeSeconds = fromIntegral siUptime
    , CGCommon.connections = fromIntegral siConnections
    , CGCommon.version = siVersion
    , CGCommon.protocolVersion = fromIntegral siProtocolVersion
    , CGCommon.namespacePid = maybe 0 fromIntegral siNamespacePid
    }

fromCapnpStatusInfo :: C.Parsed CGCommon.StatusInfo -> P.StatusInfo
fromCapnpStatusInfo CGCommon.StatusInfo {..} =
  P.StatusInfo
    { P.siUptime = fromIntegral uptimeSeconds
    , P.siConnections = fromIntegral connections
    , P.siVersion = version
    , P.siProtocolVersion = fromIntegral protocolVersion
    , P.siNamespacePid =
        if namespacePid == 0 then Nothing else Just (fromIntegral namespacePid)
    }

-- ---------------------------------------------------------------------
-- View grant
-- ---------------------------------------------------------------------

-- | Short-lived SPICE access credentials returned by @vm.viewGrant@.
-- Mirrors 'C.Parsed CGCommon.ViewGrant'.
data ViewGrant = ViewGrant
  { vgHost :: !Text
  , vgPort :: !Int
  , vgPassword :: !Text
  , vgTtlSeconds :: !Int
  }
  deriving (Eq, Show)

toCapnpViewGrant :: ViewGrant -> C.Parsed CGCommon.ViewGrant
toCapnpViewGrant ViewGrant {..} =
  CGCommon.ViewGrant
    { CGCommon.host = vgHost
    , CGCommon.port = fromIntegral vgPort
    , CGCommon.password = vgPassword
    , CGCommon.ttlSeconds = fromIntegral vgTtlSeconds
    }

fromCapnpViewGrant :: C.Parsed CGCommon.ViewGrant -> ViewGrant
fromCapnpViewGrant CGCommon.ViewGrant {..} =
  ViewGrant
    { vgHost = host
    , vgPort = fromIntegral port
    , vgPassword = password
    , vgTtlSeconds = fromIntegral ttlSeconds
    }
