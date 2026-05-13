{-# LANGUAGE OverloadedStrings #-}

-- | Build command handler for the Corvus client.
--
-- The legacy 'BuildEvent' stream (length-prefixed 'Data.Binary' frames
-- riding an upgraded socket) is gone. Cap'n Proto streaming via a
-- @BuildEventSink@ cap lands in Phase 6; until then @crv build@
-- prints a clean "streaming not yet implemented" error and returns
-- with a non-zero exit code.
module Corvus.Client.Commands.Build
  ( handleBuild
  )
where

import Corvus.Client.Capnp.Connection (CapnpConnection)
import Corvus.Client.Output (emitError)
import Corvus.Client.Types (OutputFormat, WaitOptions)

-- | Top-level handler for @crv build FILE@.
--
-- Phase 5 stub. Both the async and @--wait@ paths require either:
--
-- * a Cap'n Proto async @Daemon.build@ method that returns a parent
--   task id, or
-- * a @BuildEventSink@ streaming cap.
--
-- Neither is wired yet; emit a clean error and return failure so
-- automated callers can detect the gap. Phase 6 lifts this stub.
handleBuild :: OutputFormat -> CapnpConnection -> FilePath -> WaitOptions -> IO Bool
handleBuild fmt _conn _path _waitOpts = do
  emitError
    fmt
    "not_implemented"
    "crv build requires Cap'n Proto streaming (Phase 6, not yet implemented)"
    (putStrLn "Build is not yet wired over Cap'n Proto; see Phase 6 of the migration plan.")
  pure False
