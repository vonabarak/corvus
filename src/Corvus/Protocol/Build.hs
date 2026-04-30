{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | @crv build@ response data.
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Aggregate result returned by a @ReqBuild@ call.
--
-- Modelled as a single-field 'data' rather than a 'newtype' so the
-- 'Binary' instance can use the @anyclass@ strategy unambiguously.
-- With a newtype + both @DeriveAnyClass@ and
-- @GeneralizedNewtypeDeriving@ enabled, GHC emits a
-- @-Wderiving-defaults@ warning *and* — observed under cabal-install
-- in Gentoo's haskell-cabal eclass — silently fails to expose the
-- module's @$tc*_closure@ symbols, breaking executables that take a
-- 'Typeable' on the type.
{-# ANN type BuildResult ("HLint: ignore Use newtype instead of data" :: String) #-}
data BuildResult = BuildResult
  { brBuilds :: ![BuildOne]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

instance ToJSON BuildOne where
  toJSON = genericToJSON innerOptions

instance ToJSON BuildResult where
  toJSON = genericToJSON innerOptions
