{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Owned worker groups. Shutdown joins all admitted children; expected owned
-- cancellation is handled around their entire lifetime, including registration.
module Supervisors
  ( Supervisor
  , withSupervisor
  , withSupervisorReporting
  , supervise
  , superviseSTM
  ) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, myThreadId)
import Control.Concurrent.Async (link, mapConcurrently_, withAsync)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, unless, when)
import qualified Data.Map.Strict as Map
import GHC.Conc (getUncaughtExceptionHandler)

-- | Private to this supervisor implementation. Unexpected AsyncCancelled is
-- deliberately not swallowed.
data OwnedShutdown = OwnedShutdown deriving stock (Show)

instance Exception OwnedShutdown where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

data Supervisor = Supervisor
  { children :: TVar (Map.Map ThreadId (TMVar ()))
  , stopped :: TVar Bool
  , runQ :: TQueue (IO ())
  , reportFailure :: SomeException -> IO ()
  }

-- | Use the runtime reporter for unexpected child failures.
withSupervisor :: (Supervisor -> IO a) -> IO a
withSupervisor = withSupervisorReporting (\err -> getUncaughtExceptionHandler >>= ($ err))

-- | Supply a contextual reporter for unexpected child failures. Parent failures
-- retain their original exception; ordinary scope exit quietly stops workers.
withSupervisorReporting :: (SomeException -> IO ()) -> (Supervisor -> IO a) -> IO a
withSupervisorReporting report action =
  bracket acquire shutdown $ \sup ->
    withAsync (forever $ atomically (readTQueue (runQ sup)) >>= supervise sup) $ \scheduler -> do
      link scheduler
      action sup
  where
    acquire = Supervisor <$> newTVarIO Map.empty <*> newTVarIO False <*> newTQueueIO <*> pure report

shutdown :: Supervisor -> IO ()
shutdown Supervisor {children, stopped} = do
  kids <- atomically $ do
    writeTVar stopped True
    readTVar children
  mapConcurrently_ (`throwTo` OwnedShutdown) (Map.keys kids)
  atomically $ mapM_ readTMVar (Map.elems kids)

-- | Admit a worker atomically with its start gate. A concurrent shutdown either
-- owns and joins it, or prevents its body from starting at all.
supervise :: Supervisor -> IO () -> IO ()
supervise sup@Supervisor {children, stopped} task = mask_ $ do
  gate <- newEmptyTMVarIO
  done <- newEmptyTMVarIO
  tid <- forkIOWithUnmask $ \unmask -> do
    me <- myThreadId
    let finish = atomically $ do
          modifyTVar' children (Map.delete me)
          putTMVar done ()
        run = do
          admitted <- atomically $ takeTMVar gate
          when admitted (unmask task)
        report err = case fromException err :: Maybe OwnedShutdown of
          Just _ -> pure ()
          Nothing -> reportFailure sup err
    let ownedOnly (err :: SomeException) = case fromException err :: Maybe OwnedShutdown of
          Just _ -> pure ()
          Nothing -> throwIO err
    ((run `catch` report) `catch` ownedOnly) `finally` finish
  atomically $ do
    closed <- readTVar stopped
    unlessClosed closed tid done
    putTMVar gate (not closed)
  where
    unlessClosed closed tid done = unless closed $ modifyTVar' children (Map.insert tid done)

-- | Enqueue only while the supervisor is alive. Queued bodies that have not
-- been admitted when shutdown begins are never run.
superviseSTM :: Supervisor -> IO () -> STM ()
superviseSTM Supervisor {stopped, runQ} task = do
  closed <- readTVar stopped
  unless closed $ writeTQueue runQ task
