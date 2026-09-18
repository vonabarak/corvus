{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cloud-init image generation RPCs.
module Corvus.NodeAgentClient.CloudInit
  ( cloudInitGenerateIso
  )
where

import qualified Capnp.Gen.Nodeagent as CGNA
import Corvus.NodeAgentClient.Core
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Cloud-init

cloudInitGenerateIso
  :: NodeAgentClient
  -> T.Text
  -- ^ Target directory; agent @mkdir -p@'s it.
  -> T.Text
  -- ^ Composed user-data text.
  -> T.Text
  -- ^ Composed meta-data text.
  -> Maybe T.Text
  -- ^ Optional network-config text.
  -> IO (Either NodeAgentError T.Text)
cloudInitGenerateIso nac targetDir userData metaData mNetworkConfig = remote $ do
  let (networkConfig, hasNetworkConfig) = case mNetworkConfig of
        Just nc -> (nc, True)
        Nothing -> ("", False)
  CGNA.Session'cloudInitGenerateIso'results {CGNA.isoPath = p} <-
    callOn
      #cloudInitGenerateIso
      CGNA.Session'cloudInitGenerateIso'params
        { CGNA.targetDir = targetDir
        , CGNA.userData = userData
        , CGNA.metaData = metaData
        , CGNA.networkConfig = networkConfig
        , CGNA.hasNetworkConfig = hasNetworkConfig
        }
      (nacSession nac)
  pure p

-- ---------------------------------------------------------------------------
-- VM abstraction — wire types are defined in "Corvus.Node.VmSpec"
-- and re-exported above so the daemon-side client surface is
-- self-contained.
