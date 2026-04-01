{-# LANGUAGE OverloadedStrings #-}

-- | Task history command handlers for the Corvus client.
module Corvus.Client.Commands.Task
  ( handleTaskList
  , handleTaskShow
  , handleTaskWait
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Corvus.Client.Connection (Connection)
import Corvus.Client.Output (isStructured, outputError, outputResult)
import Corvus.Client.Rpc (taskList, taskShow)
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (EnumText (..), TaskResult (..), TaskSubsystem)
import Corvus.Protocol (TaskInfo (..))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | Handle task list command
handleTaskList :: OutputFormat -> Connection -> Int -> Maybe Text -> Maybe Text -> IO Bool
handleTaskList fmt conn limit mSubStr mResultStr = do
  let mSub = mSubStr >>= eitherToMaybe . enumFromText
      mResult = mResultStr >>= eitherToMaybe . enumFromText
  resp <- taskList conn limit mSub mResult
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right tasks -> do
      if isStructured fmt
        then outputResult fmt tasks
        else do
          if null tasks
            then putStrLn "No tasks found."
            else do
              putStrLn $
                printf
                  "%-6s %-10s %-12s %-22s %-8s %-20s %-10s %s"
                  ("ID" :: String)
                  ("SUBSYSTEM" :: String)
                  ("COMMAND" :: String)
                  ("ENTITY" :: String)
                  ("RESULT" :: String)
                  ("STARTED" :: String)
                  ("DURATION" :: String)
                  ("MESSAGE" :: String)
              putStrLn $ replicate 110 '-'
              now <- getCurrentTime
              mapM_ (printTaskRow now) tasks
      pure True

-- | Handle task show command
handleTaskShow :: OutputFormat -> Connection -> Int64 -> IO Bool
handleTaskShow fmt conn taskId = do
  resp <- taskShow conn taskId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right Nothing -> do
      if isStructured fmt
        then outputError fmt "not_found" "Task not found"
        else putStrLn "Task not found"
      pure False
    Right (Just info) -> do
      if isStructured fmt
        then outputResult fmt info
        else printTaskDetail info
      pure True

-- | Handle task wait command — poll until task finishes
handleTaskWait :: OutputFormat -> Connection -> Int64 -> IO Bool
handleTaskWait fmt conn taskId = do
  -- Get initial task info
  resp <- taskShow conn taskId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right Nothing -> do
      if isStructured fmt
        then outputError fmt "not_found" "Task not found"
        else putStrLn "Task not found"
      pure False
    Right (Just info)
      | tiResult info /= TaskRunning -> do
          if isStructured fmt
            then outputResult fmt info
            else
              putStrLn $
                "Task "
                  ++ show taskId
                  ++ " already completed: "
                  ++ T.unpack (enumToText (tiResult info))
                  ++ maybe "" (\d -> " (" ++ formatDuration d ++ ")") (durationOf info)
          pure True
      | otherwise -> do
          unless (isStructured fmt) $ do
            putStr $
              "Waiting for task "
                ++ show taskId
                ++ " ("
                ++ T.unpack (enumToText (tiSubsystem info))
                ++ " "
                ++ T.unpack (tiCommand info)
                ++ maybe "" (\n -> " " ++ T.unpack n) (tiEntityName info)
                ++ ")..."
            hFlush stdout
          pollUntilDone fmt conn taskId

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

pollUntilDone :: OutputFormat -> Connection -> Int64 -> IO Bool
pollUntilDone fmt conn taskId = do
  threadDelay 1000000
  resp <- taskShow conn taskId
  case resp of
    Left err -> do
      putStrLn ""
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error polling task: " ++ show err
      pure False
    Right Nothing -> do
      putStrLn ""
      putStrLn "Task disappeared"
      pure False
    Right (Just info)
      | tiResult info == TaskRunning -> pollUntilDone fmt conn taskId
      | otherwise -> do
          if isStructured fmt
            then outputResult fmt info
            else
              putStrLn $
                "\nTask completed: "
                  ++ T.unpack (enumToText (tiResult info))
                  ++ maybe "" (\d -> " (" ++ formatDuration d ++ ")") (durationOf info)
                  ++ maybe "" (\m -> " — " ++ T.unpack m) (tiMessage info)
          pure (tiResult info == TaskSuccess)

printTaskRow :: UTCTime -> TaskInfo -> IO ()
printTaskRow now info =
  putStrLn $
    printf
      "%-6d %-10s %-12s %-22s %-8s %-20s %-10s %s"
      (tiId info)
      (T.unpack $ enumToText $ tiSubsystem info)
      (T.unpack $ tiCommand info)
      (entityLabel info)
      (T.unpack $ enumToText $ tiResult info)
      (formatTimestamp $ tiStartedAt info)
      (durationLabel now info)
      (T.unpack $ fromMaybe "-" $ tiMessage info)

printTaskDetail :: TaskInfo -> IO ()
printTaskDetail info = do
  putStrLn $ "Task ID:    " ++ show (tiId info)
  putStrLn $ "Subsystem:  " ++ T.unpack (enumToText (tiSubsystem info))
  putStrLn $ "Command:    " ++ T.unpack (tiCommand info)
  putStrLn $ "Entity:     " ++ entityLabel info
  putStrLn $ "Result:     " ++ T.unpack (enumToText (tiResult info))
  putStrLn $ "Started:    " ++ formatTimestamp (tiStartedAt info)
  putStrLn $ "Finished:   " ++ maybe "-" formatTimestamp (tiFinishedAt info)
  putStrLn $ "Duration:   " ++ maybe "-" formatDuration (durationOf info)
  putStrLn $ "Message:    " ++ T.unpack (fromMaybe "-" (tiMessage info))

entityLabel :: TaskInfo -> String
entityLabel info = case (tiEntityName info, tiEntityId info) of
  (Just name, Just eid) -> T.unpack name ++ " (" ++ show eid ++ ")"
  (Just name, Nothing) -> T.unpack name
  (Nothing, Just eid) -> show eid
  (Nothing, Nothing) -> "-"

formatTimestamp :: UTCTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

durationOf :: TaskInfo -> Maybe Double
durationOf info = do
  finish <- tiFinishedAt info
  pure $ realToFrac $ diffUTCTime finish (tiStartedAt info)

durationLabel :: UTCTime -> TaskInfo -> String
durationLabel now info = case tiFinishedAt info of
  Just finish -> formatDuration (realToFrac $ diffUTCTime finish (tiStartedAt info))
  Nothing ->
    if tiResult info == TaskRunning
      then formatDuration (realToFrac $ diffUTCTime now (tiStartedAt info)) ++ "..."
      else "-"

formatDuration :: Double -> String
formatDuration d
  | d < 1.0 = printf "%.0fms" (d * 1000)
  | d < 60.0 = printf "%.1fs" d
  | d < 3600.0 = printf "%.0fm%.0fs" (d / 60 :: Double) (mod' d 60)
  | otherwise = printf "%.0fh%.0fm" (d / 3600 :: Double) (mod' (d / 60) 60)
  where
    mod' a b = a - b * fromIntegral (floor (a / b) :: Int)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing
