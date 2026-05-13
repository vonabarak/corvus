{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build command handler for the Corvus client.
--
-- @crv build FILE@ uploads a YAML pipeline to the daemon and
-- (with @--wait@) streams each 'BuildEvent' to stdout via the
-- Cap'n Proto @BuildEventSink@ cap.
module Corvus.Client.Commands.Build
  ( handleBuild
  )
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (emitError, emitOkWith, isStructured)
import Corvus.Client.Types (OutputFormat, WaitOptions (..))
import Corvus.Model (EnumText (..), TaskResult (..))
import Corvus.Protocol.Build (BuildEvent (..), BuildOne (..), BuildResult (..))
import Data.Aeson (toJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Top-level handler for @crv build FILE@.
--
-- With @--wait@: streams 'BuildEvent's in real time and blocks until
-- the daemon closes the sink. Without @--wait@: kicks off the build
-- and returns immediately with the parent task id.
handleBuild :: OutputFormat -> CapnpConnection -> FilePath -> WaitOptions -> IO Bool
handleBuild fmt conn path waitOpts = do
  contentResult <- try (TIO.readFile path) :: IO (Either SomeException T.Text)
  case contentResult of
    Left e -> do
      emitError fmt "io_error" (T.pack (show e)) $
        putStrLn ("Error reading " <> path <> ": " <> show e)
      pure False
    Right yaml -> runBuild fmt conn yaml (woWait waitOpts)

runBuild :: OutputFormat -> CapnpConnection -> T.Text -> Bool -> IO Bool
runBuild fmt conn yaml wait = do
  done <- newEmptyMVar :: IO (MVar ())
  -- Aggregate state surfaced after the stream ends: did any
  -- top-level step fail, and the final per-build summary.
  successVar <- newEmptyMVar :: IO (MVar Bool)
  let onEvent ev = when wait (renderEvent ev)
      onEnd = do
        _ <- tryPutMVar successVar True
        putMVar done ()
  r <- try (CR.rpcBuild conn yaml onEvent onEnd) :: IO (Either SomeException Int64)
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error invoking build: " <> show e)
      pure False
    Right tid -> do
      if wait
        then do
          takeMVar done
          _ <- takeMVar successVar
          pure True
        else do
          emitOkWith fmt [("taskId", toJSON tid)] $
            putStrLn ("Build started; task id: " <> show tid)
          pure True
  where
    -- Render one BuildEvent to stdout. Mirrors the pre-Phase-5
    -- legacy renderer: a one-line summary per event.
    renderEvent ev = case ev of
      BuildLogLine t ->
        TIO.putStrLn t
      StepStart idx kind msg ->
        TIO.putStrLn ("[step " <> T.pack (show idx) <> " " <> kind <> "] " <> msg)
      StepOutput _ line ->
        TIO.putStrLn line
      StepEnd idx result mMsg ->
        TIO.putStrLn
          ( "[step "
              <> T.pack (show idx)
              <> " "
              <> enumToText result
              <> "] "
              <> maybe "" (" - " <>) mMsg
          )
      BuildEnd (Left err) ->
        TIO.putStrLn ("Build error: " <> err)
      BuildEnd (Right diskId) ->
        TIO.putStrLn ("Build artifact disk id: " <> T.pack (show diskId))
      PipelineEnd (BuildResult builds) -> do
        TIO.putStrLn "Pipeline finished:"
        mapM_ renderOne builds
    renderOne bo = do
      let prefix = "  - " <> boName bo
      case (boArtifactDiskId bo, boError bo) of
        (Just did, _) ->
          TIO.putStrLn (prefix <> " -> disk " <> T.pack (show did))
        (_, Just err) ->
          TIO.putStrLn (prefix <> " FAILED: " <> err)
        _ ->
          TIO.putStrLn (prefix <> " (no result)")
