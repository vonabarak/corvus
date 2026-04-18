{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core daemon handlers.
-- This module contains handlers for daemon management:
-- ping, status, shutdown.
module Corvus.Handlers.Core
  ( -- * Handlers
    handlePing
  , handleStatus
  , handleShutdown
  )
where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Corvus.Protocol
import Corvus.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Paths_corvus (version)

-- | Daemon version string: @<package version>-<short git hash>@.
-- @version@ is baked in at compile time by Cabal via 'Paths_corvus';
-- the git hash is baked in by the 'gitrev' TH splice.
versionString :: Text
versionString =
  T.pack (showVersion version)
    <> "-"
    <> T.take 8 (T.pack $(gitHash))

--------------------------------------------------------------------------------
-- Core Handlers
--------------------------------------------------------------------------------

-- | Handle ping request
handlePing :: IO Response
handlePing = pure RespPong

-- | Handle status request
handleStatus :: ServerState -> IO Response
handleStatus state = do
  now <- getCurrentTime
  let uptimeSecs = floor $ diffUTCTime now (ssStartTime state)
  connCount <- readTVarIO (ssConnectionCount state)
  nsPid <- readTVarIO (ssNamespacePid state)
  pure $
    RespStatus
      StatusInfo
        { siUptime = uptimeSecs
        , siConnections = connCount
        , siVersion = versionString
        , siProtocolVersion = protocolVersion
        , siNamespacePid = nsPid
        }

-- | Handle shutdown request
handleShutdown :: ServerState -> IO Response
handleShutdown state = do
  atomically $ writeTVar (ssShutdownFlag state) True
  pure $ RespShutdownAck True
