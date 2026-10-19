-- | Subscriber registrations have identities independent of their capabilities.
-- Dispatch may overlap registration and other dispatches; it only removes the
-- registrations that failed, never writes back an obsolete registry snapshot.
module Corvus.Node.StatusSubscribers
  ( SubscriberRegistry
  , newSubscriberRegistry
  , registerSubscriber
  , dispatchSubscribers
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forM_)
import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map

newtype SubscriberRegistry a = SubscriberRegistry (TVar (Integer, Map.Map Integer a))

newSubscriberRegistry :: IO (SubscriberRegistry a)
newSubscriberRegistry = SubscriberRegistry <$> newTVarIO (0, Map.empty)

registerSubscriber :: SubscriberRegistry a -> a -> IO ()
registerSubscriber (SubscriberRegistry ref) subscriber = atomically $ do
  (next, subscribers) <- readTVar ref
  writeTVar ref (next + 1, Map.insert next subscriber subscribers)

-- | Delivery returns False for a dead registration. Successful deliveries never
-- reinsert registrations that a concurrent dispatcher has already removed.
dispatchSubscribers :: SubscriberRegistry a -> (a -> IO Bool) -> IO ()
dispatchSubscribers (SubscriberRegistry ref) deliver = do
  (_, subscribers) <- readTVarIO ref
  forM_ (Map.toList subscribers) $ \(key, subscriber) -> do
    alive <- deliver subscriber
    if alive
      then pure ()
      else atomically $ modifyTVar' ref $ second (Map.delete key)
