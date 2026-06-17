{-# LANGUAGE OverloadedStrings #-}

-- | The pure surface of 'Corvus.Netd.HostDns' is now a thin shim
-- over @resolvectl@ IO; the runtime contract is covered by
-- 'integration_tests/tests/test_netd_smoke.py' and
-- 'test_networking_managed.py'.
--
-- This spec exists to keep the module compiled and HSPec-discovered
-- even when nothing pure is left to assert.
module Corvus.HostDnsSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "HostDns" $ do
  it "is exercised end-to-end by the netd integration suite" $
    True `shouldBe` True
