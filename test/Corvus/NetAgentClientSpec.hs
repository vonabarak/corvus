{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.NetAgentClientSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc (ConnConfig (..), handleConn, socketTransport, toClient)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (void)
import qualified Corvus.NetAgentClient as Client
import Corvus.Netd.Caps.NetAgent (newNetAgentCap)
import Corvus.Netd.Caps.Session (newSessionCap)
import Corvus.Netd.Events (newSubscribers)
import Corvus.Netd.Ledger (newNetworkLedger, newTapLedger)
import Data.IORef
import qualified Network.Socket as NS
import Supervisors (withSupervisor)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
-- The real capability logs to process-wide stderr, shared by CLI captures.
spec = sequential $
  describe "NetAgent client" $ do
    it "waits for a successful ping reply before closing the capability" $
      withSupervisor $ \supervisor -> do
        networks <- newNetworkLedger
        taps <- newTapLedger
        subscribers <- newSubscribers (\_ _ -> pure ())
        agent <- newNetAgentCap supervisor networks taps subscribers
        session <- newSessionCap "test" supervisor networks taps subscribers
        agentClient <- C.export @CGN.NetAgent supervisor agent
        sessionClient <- C.export @CGN.Session supervisor session
        result <- Client.ping (Client.NetAgentClient agentClient sessionClient supervisor "test")
        case result of
          Right () -> pure ()
          Left err -> expectationFailure (show err)

    it "calls the body once when it throws after connection setup" $ withAgent $ \port -> do
      calls <- newIORef (0 :: Int)
      result <- try @IOException $ Client.withNetAgentClient "127.0.0.1" port "test" Nothing $ \connection -> do
        atomicModifyIORef' calls (\n -> (n + 1, ()))
        case connection of
          Left err -> expectationFailure (show err)
          Right _ -> throwIO (userError "body failed")
      result `shouldSatisfy` either (const True) (const False)
      readIORef calls `shouldReturn` 1

    it "propagates cancellation and closes an established client" $ withAgent $ \port -> do
      entered <- newEmptyTMVarIO
      withAsync
        ( Client.withNetAgentClient "127.0.0.1" port "test" Nothing $ \case
            Left err -> expectationFailure (show err)
            Right _ -> atomically (putTMVar entered ()) >> atomically retry
        )
        $ \worker -> do
          timeout 2000000 (atomically $ takeTMVar entered) `shouldReturn` Just ()
          timeout 2000000 (cancel worker) `shouldReturn` Just ()
          result <- waitCatch worker
          result `shouldSatisfy` either (const True) (const False)

    it "preserves caller deadlines during a stalled bootstrap" $
      bracket (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol) NS.close $ \listener -> do
        NS.bind listener (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
        NS.listen listener 1
        NS.SockAddrInet port _ <- NS.getSocketName listener
        withAsync (bracket (fst <$> NS.accept listener) NS.close (\_ -> atomically retry)) $ \_ -> do
          calls <- newIORef (0 :: Int)
          result <- timeout 100000 $ Client.withNetAgentClient "127.0.0.1" (fromIntegral port) "test" Nothing (\_ -> modifyIORef' calls (+ 1))
          result `shouldBe` Nothing
          readIORef calls `shouldReturn` 0

withAgent :: (Int -> IO ()) -> IO ()
withAgent action = withSupervisor $ \supervisor -> do
  networks <- newNetworkLedger
  taps <- newTapLedger
  subscribers <- newSubscribers (\_ _ -> pure ())
  agent <- newNetAgentCap supervisor networks taps subscribers
  client <- C.export @CGN.NetAgent supervisor agent
  bracket (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol) NS.close $ \listener -> do
    NS.bind listener (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
    NS.listen listener 1
    NS.SockAddrInet port _ <- NS.getSocketName listener
    withAsync
      ( bracket (fst <$> NS.accept listener) NS.close $ \peer ->
          handleConn (socketTransport peer C.defaultLimit) C.def {debugMode = False, bootstrap = Just (toClient client)}
      )
      $ \_ ->
        void $ action (fromIntegral port)
