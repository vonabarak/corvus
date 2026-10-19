module Corvus.StatusSubscribersSpec (spec) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Corvus.Node.StatusSubscribers
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "status subscriber registrations" $ do
  it "preserves a new registration added while another delivery fails" $ do
    subscribers <- newSubscriberRegistry
    registerSubscriber subscribers (1 :: Int)
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let blocked _ = putMVar entered () >> takeMVar release >> pure False
    withAsync (dispatchSubscribers subscribers blocked) $ \worker -> do
      timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
      registerSubscriber subscribers 2
      putMVar release ()
      timeout 1000000 (wait worker) `shouldReturn` Just ()
    seen <- newIORef []
    dispatchSubscribers subscribers (\subscriber -> modifyIORef' seen (subscriber :) >> pure True)
    readIORef seen `shouldReturn` [2]

  it "does not resurrect a registration removed by an overlapping dispatch" $ do
    subscribers <- newSubscriberRegistry
    registerSubscriber subscribers ()
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let blocked _ = putMVar entered () >> takeMVar release >> pure True
    withAsync (dispatchSubscribers subscribers blocked) $ \worker -> do
      timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
      dispatchSubscribers subscribers (const (pure False))
      putMVar release ()
      timeout 1000000 (wait worker) `shouldReturn` Just ()
    dispatchSubscribers subscribers (\_ -> expectationFailure "removed registration was restored" >> pure True)

  it "prunes only failed registrations and keeps independent registrations of the same value" $ do
    subscribers <- newSubscriberRegistry
    registerSubscriber subscribers True
    registerSubscriber subscribers False
    registerSubscriber subscribers True
    dispatchSubscribers subscribers pure
    seen <- newIORef []
    dispatchSubscribers subscribers (\subscriber -> modifyIORef' seen (subscriber :) >> pure True)
    readIORef seen `shouldReturn` [True, True]
