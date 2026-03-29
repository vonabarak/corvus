{-# LANGUAGE OverloadedStrings #-}

-- | Apply command handler for the Corvus client.
-- Reads a YAML config file and sends it to the daemon for processing.
module Corvus.Client.Commands.Apply
  ( handleApply
  )
where

import Control.Monad (unless)
import Corvus.Client.Connection
import Corvus.Client.Output (isStructured, outputError, outputResult)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Protocol (ApplyCreated (..), ApplyResult (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)

-- | Handle apply command: read YAML file and send to daemon
handleApply :: OutputFormat -> Connection -> FilePath -> Bool -> IO Bool
handleApply fmt conn path skipExisting = do
  exists <- doesFileExist path
  if not exists
    then do
      if isStructured fmt
        then outputError fmt "file_not_found" (T.pack $ "File not found: " ++ path)
        else putStrLn $ "Error: File not found: " ++ path
      pure False
    else do
      content <- TIO.readFile path
      resp <- applyConfig conn content skipExisting
      case resp of
        Left err -> do
          if isStructured fmt
            then outputError fmt "rpc_error" (T.pack $ show err)
            else putStrLn $ "Error: " ++ show err
          pure False
        Right (ApplyOk result) -> do
          if isStructured fmt
            then outputResult fmt result
            else printApplyResult result
          pure True
        Right (ApplyFailed msg) -> do
          if isStructured fmt
            then outputError fmt "apply_failed" msg
            else putStrLn $ "Apply failed: " ++ T.unpack msg
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
