{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Apply command handler for the Corvus client.
-- Reads a YAML config file and sends it to the daemon for processing.
module Corvus.Client.Commands.Apply
  ( handleApply
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (emitError, emitResult)
import Corvus.Client.Types (OutputFormat, WaitOptions (..))
import Corvus.Protocol (ApplyCreated (..), ApplyResult (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)

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
      r <- try @SomeException (CR.rpcApply conn content skipExisting wait)
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
