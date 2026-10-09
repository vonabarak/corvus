-- | Reentrant mutation leases. Cleanup needs an exclusive lease; ordinary
-- actions share leases, including child threads spawned by an orchestrator.
module Corvus.ImageOperationGuard (ImageOperationGuard, newImageOperationGuard, withImageOperationGuard) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Exception (bracket_)
import qualified Data.Map.Strict as Map

newtype ImageOperationGuard
  = ImageOperationGuard
      (TVar (Map.Map ThreadId Int, Maybe (ThreadId, Int)))

newImageOperationGuard :: IO ImageOperationGuard
newImageOperationGuard = ImageOperationGuard <$> newTVarIO (Map.empty, Nothing)

-- | No writer preference: a reader may await a new reader child thread.
-- Giving waiting writers priority would deadlock that operation tree.
withImageOperationGuard :: ImageOperationGuard -> Bool -> IO a -> IO a
withImageOperationGuard (ImageOperationGuard var) exclusive operation = do
  tid <- myThreadId
  let acquire = atomically $ do
        (readers, writer) <- readTVar var
        case writer of
          Just (owner, count)
            | owner == tid ->
                writeTVar var (readers, Just (owner, count + 1))
          Just _ -> retry
          Nothing | exclusive -> do
            check (Map.null readers)
            writeTVar var (readers, Just (tid, 1))
          Nothing -> writeTVar var (Map.insertWith (+) tid 1 readers, Nothing)
      release = atomically $ do
        (readers, writer) <- readTVar var
        case writer of
          Just (owner, count)
            | owner == tid ->
                writeTVar var (readers, if count == 1 then Nothing else Just (owner, count - 1))
          _ -> writeTVar var (Map.update (\n -> if n == 1 then Nothing else Just (n - 1)) tid readers, writer)
  bracket_ acquire release operation
