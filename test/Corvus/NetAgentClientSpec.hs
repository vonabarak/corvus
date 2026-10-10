{-# LANGUAGE TypeApplications #-}

module Corvus.NetAgentClientSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Netagent as CGN
import qualified Corvus.NetAgentClient as Client
import Corvus.Netd.Caps.NetAgent (newNetAgentCap)
import Corvus.Netd.Caps.Session (newSessionCap)
import Corvus.Netd.Events (newSubscribers)
import Corvus.Netd.Ledger (newNetworkLedger, newTapLedger)
import Supervisors (withSupervisor)
import Test.Hspec

spec :: Spec
-- The real capability logs to process-wide stderr, shared by CLI captures.
spec = sequential $
  describe "NetAgent client" $
    it "waits for a successful ping reply before closing the capability" $
      withSupervisor $ \supervisor -> do
        networks <- newNetworkLedger
        taps <- newTapLedger
        subscribers <- newSubscribers
        agent <- newNetAgentCap supervisor networks taps subscribers
        session <- newSessionCap "test" supervisor networks taps subscribers
        agentClient <- C.export @CGN.NetAgent supervisor agent
        sessionClient <- C.export @CGN.Session supervisor session
        result <- Client.ping (Client.NetAgentClient agentClient sessionClient supervisor "test")
        case result of
          Right () -> pure ()
          Left err -> expectationFailure (show err)
