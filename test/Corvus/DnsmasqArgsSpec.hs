{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Corvus.Netd.Dnsmasq.buildDnsmasqArgs' and the
-- DNS-server (de)serialisation helpers in
-- 'Corvus.NetAgentClient.Spec'.
--
-- 'buildDnsmasqArgs' is the pure projection of a
-- 'DnsmasqStartParams' into the dnsmasq command line; it's what
-- decides whether @--dhcp-option=option:dns-server,…@ is emitted
-- (the gap that left integration-test guests without DNS until
-- this slice landed).
--
-- The encode/decode pair is the daemon's bridge between the
-- 'Network' row's comma-joined 'Text' column and the structured
-- '[Text]' list every layer above uses.
module Corvus.DnsmasqArgsSpec (spec) where

import Corvus.NetAgentClient.Spec (decodeDnsServers, encodeDnsServers)
import Corvus.Netd.Dnsmasq
  ( DnsmasqStartParams (..)
  , buildDnsmasqArgs
  )
import Test.Hspec

baseParams :: DnsmasqStartParams
baseParams =
  DnsmasqStartParams
    { dspBridge = "corvus-br-7"
    , dspListenAddr = "10.0.0.1"
    , dspDhcpRange = "10.0.0.10,10.0.0.100,12h"
    , dspDomain = ""
    , dspExtraArgs = []
    , dspHostReservations = []
    , dspDnsServers = []
    }

spec :: Spec
spec = do
  describe "buildDnsmasqArgs" $ do
    it "omits --dhcp-option=option:dns-server when dspDnsServers is empty" $ do
      let args = buildDnsmasqArgs baseParams
      args `shouldNotContain` ["--dhcp-option=option:dns-server,"]
      filter (\a -> take 21 a == "--dhcp-option=option:") args `shouldBe` []

    it "emits exactly one --dhcp-option=option:dns-server,<joined> for one server" $ do
      let p = baseParams {dspDnsServers = ["1.1.1.1"]}
      buildDnsmasqArgs p
        `shouldContain` ["--dhcp-option=option:dns-server,1.1.1.1"]

    it "comma-joins multiple servers into a single argument" $ do
      let p = baseParams {dspDnsServers = ["1.1.1.1", "8.8.8.8", "9.9.9.9"]}
      buildDnsmasqArgs p
        `shouldContain` ["--dhcp-option=option:dns-server,1.1.1.1,8.8.8.8,9.9.9.9"]

    it "preserves the existing --dhcp-host reservations alongside dns-server" $ do
      let p =
            baseParams
              { dspHostReservations = [("52:54:00:aa:bb:cc", "10.0.0.42")]
              , dspDnsServers = ["1.1.1.1"]
              }
          args = buildDnsmasqArgs p
      args `shouldContain` ["--dhcp-host=52:54:00:aa:bb:cc,10.0.0.42"]
      args `shouldContain` ["--dhcp-option=option:dns-server,1.1.1.1"]

  describe "encodeDnsServers / decodeDnsServers" $ do
    it "roundtrips an empty list through the empty string" $ do
      encodeDnsServers [] `shouldBe` ""
      decodeDnsServers "" `shouldBe` []

    it "roundtrips a single-element list" $ do
      encodeDnsServers ["1.1.1.1"] `shouldBe` "1.1.1.1"
      decodeDnsServers "1.1.1.1" `shouldBe` ["1.1.1.1"]

    it "roundtrips a multi-element list" $ do
      let ips = ["1.1.1.1", "8.8.8.8", "9.9.9.9"]
      decodeDnsServers (encodeDnsServers ips) `shouldBe` ips

    it "decode trims surrounding whitespace and skips blanks" $ do
      decodeDnsServers " 1.1.1.1 ,, 8.8.8.8 " `shouldBe` ["1.1.1.1", "8.8.8.8"]
