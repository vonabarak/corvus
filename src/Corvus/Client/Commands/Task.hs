{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Task history command handlers for the Corvus client.
module Corvus.Client.Commands.Task
  ( handleTaskList
  , handleTaskShow
  , handleTaskWait
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitResult, isStructured, printTable)
import Corvus.Client.Types (OutputFormat)
import Corvus.Model (EnumText (..), TaskResult (..))
import Corvus.Protocol (TaskInfo (..))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import System.IO (hFlush, hPutStr, stderr, stdout)
import Text.Printf (printf)

-- | Handle task list command. Subsystem / result filters are
-- currently not threaded through the Cap'n Proto wrapper; the
-- wrapper takes only @limit@ and the daemon applies the rest of the
-- filtering server-side once the schema gains those params again.
handleTaskList :: OutputFormat -> TableOpts -> CapnpConnection -> Int -> Maybe Text -> Maybe Text -> Bool -> IO Bool
handleTaskList fmt tableOpts conn limit _mSubStr _mResultStr _includeSubtasks = do
  r <- try @SomeException (CR.rpcTaskList conn limit)
  case r of
    Right tasks -> do
      emitResult fmt tasks $
        if null tasks
          then putStrLn "No tasks found."
          else do
            now <- getCurrentTime
            printTable tableOpts (taskColumns now) tasks
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle task show command
handleTaskShow :: OutputFormat -> CapnpConnection -> Int64 -> IO Bool
handleTaskShow fmt conn taskId = do
  r <- try @SomeException (CR.rpcTaskShow conn taskId)
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False
    Right info -> do
      emitResult fmt info $ do
        printTaskDetail info
        childResp <- try @SomeException (CR.rpcTaskListChildren conn taskId)
        case childResp of
          Right children | not (null children) -> do
            putStrLn ""
            putStrLn "Subtasks:"
            putStrLn $
              printf
                "  %-6s %-10s %-18s %-22s %-10s %-10s %s"
                ("ID" :: String)
                ("SUBSYSTEM" :: String)
                ("COMMAND" :: String)
                ("ENTITY" :: String)
                ("RESULT" :: String)
                ("DURATION" :: String)
                ("MESSAGE" :: String)
            putStrLn $ "  " ++ replicate 106 '-'
            now <- getCurrentTime
            mapM_ (printSubtaskRow now) children
          _ -> pure ()
      pure True

-- | Handle task wait command — poll until task finishes or timeout
handleTaskWait :: OutputFormat -> CapnpConnection -> Int64 -> Maybe Int -> IO Bool
handleTaskWait fmt conn taskId mTimeout = do
  r <- try @SomeException (CR.rpcTaskShow conn taskId)
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False
    Right info
      | isTaskPending (tiResult info) -> do
          startTime <- getCurrentTime
          unless (isStructured fmt) $ do
            putStr $
              "\rWaiting for task "
                ++ show taskId
                ++ " ("
                ++ T.unpack (enumToText (tiSubsystem info))
                ++ " "
                ++ T.unpack (tiCommand info)
                ++ maybe "" (\n -> " " ++ T.unpack n) (tiEntityName info)
                ++ ")..."
            hFlush stdout
          pollUntilDone fmt conn taskId startTime mTimeout
      | otherwise -> do
          emitResult fmt info $ printCompletionMessage taskId info
          pure (tiResult info == TaskSuccess)

-- | A task is still pending if it's running or not yet started
isTaskPending :: TaskResult -> Bool
isTaskPending TaskRunning = True
isTaskPending TaskNotStarted = True
isTaskPending _ = False

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Poll interval in microseconds based on elapsed time.
pollInterval :: Double -> Int
pollInterval elapsed
  | elapsed < 3.0 = 200000
  | otherwise = 1000000

pollUntilDone :: OutputFormat -> CapnpConnection -> Int64 -> UTCTime -> Maybe Int -> IO Bool
pollUntilDone fmt conn taskId startTime mTimeout = do
  now <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime now startTime) :: Double

  case mTimeout of
    Just timeout | elapsed >= fromIntegral timeout -> do
      unless (isStructured fmt) $ putStrLn ""
      emitError fmt "timeout" ("Task did not complete within " <> T.pack (show timeout) <> "s") $
        putStrLn $
          "Timeout: task did not complete within " ++ show timeout ++ "s"
      pure False
    _ -> do
      threadDelay (pollInterval elapsed)
      r <- try @SomeException (CR.rpcTaskShow conn taskId)
      case r of
        Left e -> do
          unless (isStructured fmt) $ putStrLn ""
          emitError fmt "rpc_error" (T.pack $ show e) $
            putStrLn $
              "Error polling task: " ++ show e
          pure False
        Right info
          | isTaskPending (tiResult info) -> do
              unless (isStructured fmt) $ do
                hPutStr stderr $ "\r\x1b[K" ++ "Waiting... (" ++ formatDuration elapsed ++ " elapsed)"
                hFlush stderr
              pollUntilDone fmt conn taskId startTime mTimeout
          | otherwise -> do
              unless (isStructured fmt) $ do
                hPutStr stderr "\r\x1b[K"
                hFlush stderr
              emitResult fmt info $ printCompletionMessage taskId info
              pure (tiResult info == TaskSuccess)

-- | Print a human-readable completion message with full details
printCompletionMessage :: Int64 -> TaskInfo -> IO ()
printCompletionMessage taskId info =
  putStrLn $
    "Task "
      ++ show taskId
      ++ " completed: "
      ++ T.unpack (enumToText (tiResult info))
      ++ maybe "" (\d -> " (" ++ formatDuration d ++ ")") (durationOf info)
      ++ " — "
      ++ T.unpack (enumToText (tiSubsystem info))
      ++ " "
      ++ T.unpack (tiCommand info)
      ++ maybe "" (\n -> " " ++ T.unpack n) (tiEntityName info)
      ++ maybe "" (\m -> ": " ++ T.unpack m) (tiMessage info)

-- | Column definitions for the @task list@ table.
taskColumns :: UTCTime -> [Column TaskInfo]
taskColumns now =
  [ Column "ID" RightAlign (show . tiId)
  , Column "SUBSYSTEM" LeftAlign (T.unpack . enumToText . tiSubsystem)
  , Column "COMMAND" LeftAlign (T.unpack . tiCommand)
  , Column "ENTITY" LeftAlign entityLabel
  , Column "CLIENT" LeftAlign (T.unpack . tiClientName)
  , Column "RESULT" LeftAlign (T.unpack . enumToText . tiResult)
  , Column "STARTED" LeftAlign (formatTimestamp . tiStartedAt)
  , Column "DURATION" RightAlign (durationLabel now)
  , Column "MESSAGE" LeftAlign (T.unpack . fromMaybe "-" . tiMessage)
  ]

printSubtaskRow :: UTCTime -> TaskInfo -> IO ()
printSubtaskRow now info =
  putStrLn $
    printf
      "  %-6d %-10s %-18s %-22s %-10s %-10s %s"
      (tiId info)
      (T.unpack $ enumToText $ tiSubsystem info)
      (T.unpack $ tiCommand info)
      (entityLabel info)
      (T.unpack $ enumToText $ tiResult info)
      (durationLabel now info)
      (T.unpack $ fromMaybe "-" $ tiMessage info)

printTaskDetail :: TaskInfo -> IO ()
printTaskDetail info = do
  putStrLn $ "Task ID:    " ++ show (tiId info)
  case tiParentId info of
    Just pid -> putStrLn $ "Parent:     " ++ show pid
    Nothing -> pure ()
  putStrLn $ "Subsystem:  " ++ T.unpack (enumToText (tiSubsystem info))
  putStrLn $ "Command:    " ++ T.unpack (tiCommand info)
  putStrLn $ "Entity:     " ++ entityLabel info
  putStrLn $ "Client:     " ++ T.unpack (tiClientName info)
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
    if isTaskPending (tiResult info)
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
