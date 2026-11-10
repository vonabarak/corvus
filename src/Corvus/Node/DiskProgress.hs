{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Best-effort observers never run on the byte writer's hot path.
module Corvus.Node.DiskProgress (newDiskProgressSink, withByteProgress) where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Control.Exception as E
import Control.Monad (forever, when)
import Corvus.Logging (synchronousException)
import Corvus.Rpc.Streams (callSink)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import System.Timeout (timeout)

newDiskProgressSink :: C.Client CGS.DiskProgressSink -> IO (Text -> Int64 -> Int64 -> IO ())
newDiskProgressSink sink = do
  active <- newIORef True
  pure $ \phase completed total -> do
    enabled <- readIORef active
    when enabled $ do
      result <-
        timeout 5000000 $
          E.tryJust synchronousException $
            callSink
              #progress
              CGS.DiskProgressSink'progress'params
                { CGS.phase = phase
                , CGS.completed = completed
                , CGS.total = total
                }
              sink
      case result of
        Just (Right ()) -> pure ()
        _ -> writeIORef active False

withByteProgress :: IO Int64 -> (Int64 -> IO ()) -> IO a -> IO a
withByteProgress readBytes report action = do
  result <- withAsync (forever (readBytes >>= report >> threadDelay 250000)) $ const action
  readBytes >>= report
  pure result
