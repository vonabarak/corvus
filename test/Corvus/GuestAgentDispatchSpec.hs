{-# LANGUAGE OverloadedStrings #-}

module Corvus.GuestAgentDispatchSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (bracket)
import Corvus.Node.GuestAgent.Connection (GuestAgentConns (..), newGuestAgentConns, tryPersistentConnWhen, withPreparedConn)
import Corvus.Node.GuestAgent.Exec (guestExecWithTimeout)
import Corvus.Node.GuestAgent.Transport (QgaSocket, closeQga, newQgaSocket, recvQga, sendQga)
import Corvus.Node.GuestAgent.Types (GuestExecResult (..))
import Corvus.Qemu.Config (defaultQemuConfig)
import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Family (AF_UNIX), SocketType (Stream), close, defaultProtocol, socketPair)
import Test.Hspec

withCached :: (GuestAgentConns -> QgaSocket -> IO ()) -> IO ()
withCached action = withCachedPeer $ \cache sock _ -> action cache sock

withCachedPeer :: (GuestAgentConns -> QgaSocket -> QgaSocket -> IO ()) -> IO ()
withCachedPeer action = bracket (socketPair AF_UNIX Stream defaultProtocol) (\(a, b) -> close a >> close b) $ \(client, peer) -> do
  sock <- newQgaSocket (\_ _ -> pure ()) 42 1 client
  slot <- newMVar (Just sock)
  cache <- newGuestAgentConns (\_ _ -> pure ())
  atomically $ writeTVar (guestAgentSlots cache) (Map.singleton 1 slot)
  server <- newQgaSocket (\_ _ -> pure ()) 43 1 peer
  action cache sock server

spec :: Spec
spec = describe "non-replayable guest dispatch" $ do
  it "does not replay guest-exec when the guest loses its PID reply" $ withCachedPeer $ \cache _ server -> do
    count <- newIORef (0 :: Int)
    let guest = do
          _ <- recvQga server
          sendQga server $ object ["return" .= object ["id" .= ("linux" :: Text)]]
          _ <- recvQga server
          modifyIORef' count (+ 1)
          closeQga "lost_pid_reply" server
    withAsync guest $ \worker -> do
      result <- guestExecWithTimeout cache defaultQemuConfig 1 "touch /tmp/once" 1
      wait worker
      case result of
        GuestExecError message -> message `shouldSatisfy` T.isInfixOf "outcome unknown"
        other -> expectationFailure (show other)
    readIORef count `shouldReturn` 1
  it "never retries once a mutating dispatch starts" $ withCached $ \cache _ -> do
    count <- newIORef (0 :: Int)
    result <- withPreparedConn cache defaultQemuConfig 1 5 100000 100000 $ \_ -> pure $ do
      modifyIORef' count (+ 1)
      ioError (userError "lost PID reply") :: IO ()
    readIORef count `shouldReturn` 1
    result `shouldSatisfy` either (T.isInfixOf "outcome unknown") (const False)
  it "gives dispatch a fresh budget after preparation" $ withCached $ \cache _ -> do
    result <- withPreparedConn cache defaultQemuConfig 1 1 100000 200000 $ \_ -> do
      threadDelay 70000
      pure $ threadDelay 130000 >> pure True
    result `shouldBe` Right True
  it "reports timeout as unknown without repeating dispatch" $ withCached $ \cache _ -> do
    count <- newIORef (0 :: Int)
    result <- withPreparedConn cache defaultQemuConfig 1 5 100000 10000 $ \_ -> pure $ do
      modifyIORef' count (+ 1)
      threadDelay 100000
    readIORef count `shouldReturn` 1
    result `shouldSatisfy` either (T.isInfixOf "outcome unknown") (const False)
  it "does not enter an operation when lifecycle eligibility changed" $ withCached $ \cache _ -> do
    count <- newIORef (0 :: Int)
    result <- tryPersistentConnWhen (pure False) cache defaultQemuConfig 1 100000 $ \_ -> modifyIORef' count (+ 1)
    result `shouldBe` Nothing
    readIORef count `shouldReturn` 0
