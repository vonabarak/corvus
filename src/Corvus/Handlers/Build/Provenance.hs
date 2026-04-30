{-# LANGUAGE OverloadedStrings #-}

-- | Render the @\/etc\/corvus-build-info@ file written into every artifact
-- as the last provisioner step. Records what produced the image, when,
-- and from what — so future operators can answer "what's running on this
-- disk?" without having to dig through task history.
module Corvus.Handlers.Build.Provenance
  ( renderBuildInfo
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)

-- | Render a YAML-ish record. Plain key/value lines, no quoting (values
-- are constrained to safe characters by the build schema).
renderBuildInfo
  :: Text
  -- ^ build name
  -> UTCTime
  -- ^ build start time
  -> Text
  -- ^ source template name
  -> Text
  -- ^ corvus version
  -> Text
renderBuildInfo name buildTime template version =
  T.unlines
    [ "build_name: " <> name
    , "build_date: " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" buildTime)
    , "source_template: " <> template
    , "corvus_version: " <> version
    ]
