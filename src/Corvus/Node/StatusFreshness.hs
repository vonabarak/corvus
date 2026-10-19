-- | Connection-local status progress. Use a monotonic clock: node clocks and
-- persisted healthchecks cannot reliably detect a lost RPC subscription.
module Corvus.Node.StatusFreshness
  ( StatusFreshness
  , newStatusFreshness
  , recordStatusSnapshot
  , waitForStaleStatus
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)

-- | The clock is injectable so deadline tests need no wall-clock sleeps.
data StatusFreshness = StatusFreshness (IO Double) (TVar Double)

newStatusFreshness :: IO Double -> IO StatusFreshness
newStatusFreshness clock = StatusFreshness clock <$> (clock >>= newTVarIO)

-- | Refresh only after the complete snapshot has been processed successfully.
-- Empty snapshots are progress too; failures leave the previous deadline intact.
recordStatusSnapshot :: StatusFreshness -> IO a -> IO a
recordStatusSnapshot (StatusFreshness clock lastProcessed) process = do
  result <- process
  now <- clock
  atomically $ writeTVar lastProcessed now
  pure result

-- | Wait until shutdown or the deadline expires. Returns the elapsed silence on
-- expiry. The caller supplies its polling delay and a monotonic deadline in seconds.
waitForStaleStatus :: StatusFreshness -> IO Bool -> IO () -> Double -> IO (Maybe Double)
waitForStaleStatus (StatusFreshness clock lastProcessed) stopping pause deadline = loop
  where
    loop = do
      stop <- stopping
      if stop
        then pure Nothing
        else do
          now <- clock
          previous <- readTVarIO lastProcessed
          let elapsed = now - previous
          if elapsed >= deadline
            then pure (Just elapsed)
            else pause >> loop
