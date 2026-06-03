{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Apply command handler for the Corvus client.
--
-- Reads a YAML config file and sends it to the daemon. With
-- @--wait@ on a human-readable output format the call streams
-- per-phase / per-entity events (and live download progress)
-- through an 'ApplyEventSink' and renders them to stdout. With
-- @--output json@ — or when the user didn't pass @--wait@ — the
-- old non-streaming path is kept: machine-readable output stays
-- intact, and fire-and-forget still just returns the task id.
module Corvus.Client.Commands.Apply
  ( handleApply
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (emitError, emitResult)
import Corvus.Client.Types (OutputFormat (..), WaitOptions (..))
import Corvus.Model (EnumText (..), TaskResult (..))
import Corvus.Protocol (ApplyCreated (..), ApplyEvent (..), ApplyResult (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | Handle apply command: read YAML file and send to daemon
handleApply :: OutputFormat -> CapnpConnection -> FilePath -> Bool -> WaitOptions -> IO Bool
handleApply fmt conn path skipExisting waitOpts = do
  exists <- doesFileExist path
  if not exists
    then do
      emitError fmt "file_not_found" (T.pack $ "File not found: " ++ path) $
        putStrLn $
          "Error: File not found: " ++ path
      pure False
    else do
      content <- TIO.readFile path
      let wait = woWait waitOpts
          useStreaming = wait && isHumanFormat fmt
      if useStreaming
        then runStreaming fmt conn content skipExisting
        else runLegacy fmt conn content skipExisting wait

-- | Streaming path: export an 'ApplyEventSink', render each event
-- to stdout, block on @end()@.
runStreaming :: OutputFormat -> CapnpConnection -> Text -> Bool -> IO Bool
runStreaming fmt conn yaml skipExisting = do
  done <- newEmptyMVar
  successVar <- newIORef True
  let onEvent ev = do
        renderEvent ev
        case ev of
          ApplyEnd TaskSuccess _ _ -> pure ()
          ApplyEnd {} -> modifyIORef' successVar (const False)
          _ -> pure ()
      onEnd = do
        _ <- tryPutMVar done ()
        pure ()
  r <- try @SomeException (CR.rpcApplyStream conn yaml skipExisting onEvent onEnd)
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Apply failed: " ++ show e)
      pure False
    Right _tid -> do
      takeMVar done
      readIORef successVar

-- | Legacy non-streaming path. Mirrors the pre-streaming behaviour:
-- @wait=True@ prints a summary, @wait=False@ prints the task id.
runLegacy :: OutputFormat -> CapnpConnection -> Text -> Bool -> Bool -> IO Bool
runLegacy fmt conn yaml skipExisting wait = do
  r <- try @SomeException (CR.rpcApply conn yaml skipExisting wait)
  case r of
    Right (result, taskId) ->
      if wait
        then do
          emitResult fmt result $ printApplyResult result
          pure True
        else do
          emitResult fmt (T.pack $ show taskId) $
            putStrLn $
              "Apply started (task ID: " ++ show taskId ++ ")"
          pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Apply failed: " ++ show e)
      pure False

-- | True iff the output format is human-readable. JSON/YAML keep
-- the legacy synchronous path so machine consumers don't have to
-- learn the streaming protocol.
isHumanFormat :: OutputFormat -> Bool
isHumanFormat = \case
  JsonOutput -> False
  YamlOutput -> False
  _ -> True

renderEvent :: ApplyEvent -> IO ()
renderEvent ev = do
  case ev of
    ApplyLogLine t ->
      TIO.putStrLn t
    PhaseStart phase total ->
      TIO.putStrLn $ "[" <> phase <> "] creating " <> T.pack (show total)
    EntityStart phase name kind ->
      TIO.putStrLn $ "[" <> phase <> "] " <> kind <> " " <> name <> ": starting"
    EntityEnd phase name TaskSuccess _ eid
      | eid > 0 ->
          TIO.putStrLn $ "[" <> phase <> "] " <> name <> ": ok (id " <> T.pack (show eid) <> ")"
      | otherwise ->
          TIO.putStrLn $ "[" <> phase <> "] " <> name <> ": ok"
    EntityEnd phase name result msg _eid ->
      TIO.putStrLn $
        "["
          <> phase
          <> "] "
          <> name
          <> ": "
          <> enumToText result
          <> (if T.null msg then "" else " - " <> msg)
    DownloadStart name url ->
      TIO.putStrLn $ "[disks] downloading " <> name <> " from " <> url
    DownloadProgress name downloaded total ->
      renderProgress name downloaded total
    DownloadEnd name True _ ->
      TIO.putStrLn $ "[disks] download complete: " <> name
    DownloadEnd name False errMsg ->
      TIO.putStrLn $ "[disks] download failed: " <> name <> " — " <> errMsg
    ApplyEnd TaskSuccess _ _ ->
      TIO.putStrLn "Apply finished: success"
    ApplyEnd result msg _ ->
      TIO.putStrLn $ "Apply finished: " <> enumToText result <> (if T.null msg then "" else " — " <> msg)
  hFlush stdout

renderProgress :: Text -> Int64 -> Int64 -> IO ()
renderProgress name downloaded total =
  if total <= 0
    then
      TIO.putStrLn $ "[disks] " <> name <> "  " <> humanBytes downloaded
    else
      let pct :: Double
          pct = 100 * fromIntegral downloaded / fromIntegral total
       in TIO.putStrLn $
            "[disks] "
              <> name
              <> "  "
              <> humanBytes downloaded
              <> " / "
              <> humanBytes total
              <> "  "
              <> T.pack (printf "%.1f %%" pct)

-- | Format a byte count as a short human-readable string (e.g.
-- @"12.3 MB"@). Used by the live download progress renderer.
humanBytes :: Int64 -> Text
humanBytes n
  | n < 1024 = T.pack (show n) <> " B"
  | n < 1024 * 1024 = T.pack (printf "%.1f KB" (fromIntegral n / 1024.0 :: Double))
  | n < 1024 * 1024 * 1024 = T.pack (printf "%.1f MB" (fromIntegral n / (1024 * 1024) :: Double))
  | otherwise = T.pack (printf "%.2f GB" (fromIntegral n / (1024 * 1024 * 1024) :: Double))

-- | Print apply result in human-readable format
printApplyResult :: ApplyResult -> IO ()
printApplyResult result = do
  let keys = arSshKeys result
      disks = arDisks result
      networks = arNetworks result
      vms = arVms result
      total = length keys + length disks + length networks + length vms

  putStrLn $ "Applied " ++ show total ++ " resources:"

  unless (null keys) $ do
    putStrLn $ "  SSH keys (" ++ show (length keys) ++ "):"
    mapM_ (\c -> putStrLn $ "    - " ++ T.unpack (acName c) ++ " (id: " ++ show (acId c) ++ ")") keys

  unless (null disks) $ do
    putStrLn $ "  Disks (" ++ show (length disks) ++ "):"
    mapM_ (\c -> putStrLn $ "    - " ++ T.unpack (acName c) ++ " (id: " ++ show (acId c) ++ ")") disks

  unless (null networks) $ do
    putStrLn $ "  Networks (" ++ show (length networks) ++ "):"
    mapM_ (\c -> putStrLn $ "    - " ++ T.unpack (acName c) ++ " (id: " ++ show (acId c) ++ ")") networks

  unless (null vms) $ do
    putStrLn $ "  VMs (" ++ show (length vms) ++ "):"
    mapM_ (\c -> putStrLn $ "    - " ++ T.unpack (acName c) ++ " (id: " ++ show (acId c) ++ ")") vms
