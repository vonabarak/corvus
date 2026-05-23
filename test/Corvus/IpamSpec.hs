{-# LANGUAGE OverloadedStrings #-}

-- | Pure unit tests for the IPAM helpers.
module Corvus.IpamSpec (spec) where

import Corvus.Handlers.Network.Ipam (allocateIp, defaultVniBase, vniFromPool)
import Test.Hspec

spec :: Spec
spec = do
  describe "allocateIp" $ do
    it "picks .2 on an empty /24 (.0 net / .1 gateway / .255 bcast reserved)" $
      allocateIp "10.0.0.0/24" [] `shouldBe` Right "10.0.0.2"

    it "skips already-used addresses" $
      allocateIp "10.0.0.0/24" ["10.0.0.2", "10.0.0.3"] `shouldBe` Right "10.0.0.4"

    it "ignores unparseable used entries (they cannot collide)" $
      allocateIp "10.0.0.0/24" ["nonsense"] `shouldBe` Right "10.0.0.2"

    it "rejects an empty CIDR" $
      case allocateIp "" [] of
        Left _ -> pure ()
        Right ip -> expectationFailure ("expected Left, got " <> show ip)

    it "returns Left when the subnet is exhausted" $
      -- /30 has 4 addrs: .0 net, .1 gw, .3 bcast → only .2 is usable.
      case allocateIp "10.0.0.0/30" ["10.0.0.2"] of
        Left _ -> pure ()
        Right ip -> expectationFailure ("expected Left, got " <> show ip)

  describe "vniFromPool" $ do
    it "returns defaultVniBase when the pool is empty" $
      vniFromPool [] `shouldBe` defaultVniBase

    it "skips occupied VNIs in order" $
      vniFromPool [defaultVniBase, defaultVniBase + 1] `shouldBe` defaultVniBase + 2

    it "fills holes" $
      vniFromPool [defaultVniBase + 1] `shouldBe` defaultVniBase
