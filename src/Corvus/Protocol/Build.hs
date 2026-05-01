{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | @crv build@ response data.
--
-- The two records here mirror "Corvus.Protocol.Apply" exactly in
-- shape and deriving syntax (plain @deriving (Generic, Binary)@, no
-- 'DerivingStrategies'). Earlier attempts using a newtype for
-- 'BuildResult' or explicit @deriving stock@ \/ @deriving anyclass@
-- caused cabal-install (Gentoo's haskell-cabal eclass) to drop the
-- module's @$tc*_closure@ TypeRep symbols on the floor at link time
-- — \"undefined reference to ...$tcBuildOne_closure\" when the
-- @gen-python-client@ executable linked against the library. Stack
-- builds happened to dodge this. Keeping the deriving style identical
-- to the other Protocol/* modules avoids the issue.
module Corvus.Protocol.Build
  ( BuildResult (..)
  , BuildOne (..)
  )
where

import Corvus.Protocol.Aeson (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Per-build result for one entry in a @builds:@ list.
data BuildOne = BuildOne
  { boName :: !Text
  , boArtifactDiskId :: !(Maybe Int64)
  -- ^ Registered disk ID on success
  , boError :: !(Maybe Text)
  -- ^ Error message on failure
  }
  deriving (Eq, Show, Generic, Binary)

-- | Aggregate result returned by a @ReqBuild@ call.
--
-- Single-field 'data' rather than a 'newtype': 'newtype' triggers a
-- @-Wderiving-defaults@ warning under the project's combination of
-- @DeriveAnyClass@ + @GeneralizedNewtypeDeriving@, and that warning
-- coincided with the cabal-install link-time symbol dropout described
-- on the module. HLint's matching \"Use newtype instead of data\"
-- suggestion is suppressed for this module in @.hlint.yaml@.
data BuildResult = BuildResult
  { brBuilds :: ![BuildOne]
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON BuildOne where
  toJSON = genericToJSON innerOptions

instance ToJSON BuildResult where
  toJSON = genericToJSON innerOptions
