{-# LANGUAGE OverloadedStrings #-}

module Corvus.SubnetSpec (spec) where

import Corvus.Utils.Subnet
import Test.Hspec

spec :: Spec
spec = do
  describe "validateSubnet" $ do
    it "parses valid CIDR" $ do
      validateSubnet "10.0.1.0/24" `shouldBe` Right "10.0.1.0/24"

    it "normalizes non-network address to network base" $ do
      validateSubnet "10.0.1.5/24" `shouldBe` Right "10.0.1.0/24"

    it "parses /16 subnet" $ do
      validateSubnet "192.168.0.0/16" `shouldBe` Right "192.168.0.0/16"

    it "rejects missing prefix" $ do
      validateSubnet "10.0.1.0" `shouldSatisfy` isLeft

    it "rejects prefix /0" $ do
      validateSubnet "10.0.0.0/0" `shouldSatisfy` isLeft

    it "rejects prefix /31" $ do
      validateSubnet "10.0.0.0/31" `shouldSatisfy` isLeft

    it "rejects prefix /32" $ do
      validateSubnet "10.0.0.0/32" `shouldSatisfy` isLeft

    it "rejects invalid octets" $ do
      validateSubnet "999.0.0.0/24" `shouldSatisfy` isLeft

    it "rejects non-numeric prefix" $ do
      validateSubnet "10.0.0.0/abc" `shouldSatisfy` isLeft

    it "rejects empty string" $ do
      validateSubnet "" `shouldSatisfy` isLeft

  describe "gatewayAddress" $ do
    it "returns first usable IP for /24" $ do
      gatewayAddress "10.0.1.0/24" `shouldBe` Right "10.0.1.1"

    it "returns first usable IP for /16" $ do
      gatewayAddress "172.16.0.0/16" `shouldBe` Right "172.16.0.1"

  describe "dhcpRangeStart" $ do
    it "returns second usable IP for /24" $ do
      dhcpRangeStart "10.0.1.0/24" `shouldBe` Right "10.0.1.2"

  describe "dhcpRangeEnd" $ do
    it "returns last usable IP for /24" $ do
      dhcpRangeEnd "10.0.1.0/24" `shouldBe` Right "10.0.1.254"

    it "returns last usable IP for /28" $ do
      dhcpRangeEnd "10.0.1.0/28" `shouldBe` Right "10.0.1.14"

  describe "subnetMask" $ do
    it "/24 produces 255.255.255.0" $ do
      subnetMask "10.0.0.0/24" `shouldBe` Right "255.255.255.0"

    it "/16 produces 255.255.0.0" $ do
      subnetMask "10.0.0.0/16" `shouldBe` Right "255.255.0.0"

    it "/8 produces 255.0.0.0" $ do
      subnetMask "10.0.0.0/8" `shouldBe` Right "255.0.0.0"

  describe "prefixLength" $ do
    it "extracts prefix from CIDR" $ do
      prefixLength "10.0.0.0/24" `shouldBe` Right "24"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
