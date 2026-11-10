{-# LANGUAGE OverloadedStrings #-}

-- | Task messages for long disk operations. Byte updates are throttled;
-- phase transitions and the last byte update are always published.
module Corvus.Handlers.Disk.Progress (DiskProgress, newDiskProgress, formatDiskProgress) where

import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Monad (when)
import Corvus.Action (ActionContext (..), pushTaskProgress)
import Corvus.Size (formatSize)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey)
import GHC.Clock (getMonotonicTimeNSec)

type DiskProgress = Text -> Int64 -> Int64 -> IO ()

newDiskProgress :: ActionContext -> IO DiskProgress
newDiskProgress ctx = do
  previous <- newMVar (Nothing, Nothing)
  let publish phase completed total =
        pushTaskProgress
          (acState ctx)
          (fromSqlKey (acTaskId ctx))
          (formatDiskProgress phase completed total)
          (completed, total)
  pure $ \phase completed total -> modifyMVar_ previous $ \(lastUpdate, pending) -> do
    now <- getMonotonicTimeNSec
    let changed = case lastUpdate of
          Nothing -> True
          Just (at, oldPhase, oldCompleted, oldTotal) ->
            phase /= oldPhase
              || completed < oldCompleted
              || total /= oldTotal
              || now - at >= 1000000000
              || (total > 0 && completed >= total && completed /= oldCompleted)
    when changed $ do
      -- Unknown-length transfers cannot mark their last byte count with a
      -- known total. Flush the last suppressed count before leaving its phase.
      case pending of
        Just (oldPhase, oldCompleted, oldTotal)
          | oldPhase /= phase ->
              publish oldPhase oldCompleted oldTotal
        _ -> pure ()
      publish phase completed total
    pure $
      if changed
        then (Just (now, phase, completed, total), Nothing)
        else (lastUpdate, Just (phase, completed, total))

formatDiskProgress :: Text -> Int64 -> Int64 -> Text
formatDiskProgress phase completed total
  | total > 0 = phase <> ": " <> size completed <> " / " <> size total <> " (" <> percent <> "%)"
  | completed > 0 = phase <> ": " <> size completed
  | otherwise = phase
  where
    size = T.pack . formatSize . max 0
    percent = T.pack (show (min 100 (max 0 (toInteger completed * 100 `div` toInteger total))))
