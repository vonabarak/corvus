{-# LANGUAGE OverloadedStrings #-}

-- | Core daemon handlers.
-- This module contains handlers for daemon management:
-- ping, status, shutdown.
module Corvus.Handlers.Core
  ( -- * Handlers
    handlePing,
    handleStatus,
    handleShutdown,

    -- * Version
    version,
  )
where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Corvus.Protocol
import Corvus.Types
import Data.Text (Text)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- | Version string
version :: Text
version = "0.1.0"

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
  pure $
    RespStatus
      StatusInfo
        { siUptime = uptimeSecs,
          siConnections = connCount,
          siVersion = version
        }

-- | Handle shutdown request
handleShutdown :: ServerState -> IO Response
handleShutdown state = do
  atomically $ writeTVar (ssShutdownFlag state) True
  pure $ RespShutdownAck True
