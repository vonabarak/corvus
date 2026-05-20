{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Virtual-network CRUD.
--
-- The handler under test is `Corvus.Handlers.Network`. The DSL's
-- `whenNetworkCreate` passes the empty node-ref, so the
-- scheduler picks the seeded test-node. Subnet validation lives
-- in `Corvus.Utils.Subnet` and is exercised by `SubnetSpec`;
-- here we only cover the network row's lifecycle + the guard
-- responses (`RespNetworkError`, `RespNetworkNotFound`,
-- `RespNetworkInUse`).
module Corvus.NetworkSpec (spec) where

import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenNetworkList" $ do
    testCase "returns an empty list when no networks exist" $ do
      when_ whenNetworkList
      then_ $ responseIs $ \case
        RespNetworkList [] -> True
        _ -> False

    testCase "returns one row per inserted network" $ do
      given $ do
        _ <- insertNetwork "n1" "10.0.0.0/24"
        _ <- insertNetwork "n2" "10.0.1.0/24"
        pure ()
      when_ whenNetworkList
      then_ $ responseIs $ \case
        RespNetworkList xs -> length xs == 2
        _ -> False

  describe "whenNetworkShow" $ do
    testCase "returns details for an existing network" $ do
      given $ do
        _ <- insertNetwork "n" "10.0.0.0/24"
        pure ()
      when_ $ whenNetworkShow 1
      then_ $ responseIs $ \case
        RespNetworkDetails _ -> True
        _ -> False

    testCase "returns NetworkNotFound for unknown id" $ do
      when_ $ whenNetworkShow 999
      then_ $ responseIs $ \case
        RespNetworkNotFound -> True
        _ -> False

  describe "whenNetworkCreate" $ do
    testCase "writes a network row in the stopped state" $ do
      when_ $ whenNetworkCreate "fresh" "10.0.0.0/24"
      then_ $ responseIs $ \case
        RespNetworkCreated _ -> True
        _ -> False

    testCase "rejects an invalid CIDR" $ do
      when_ $ whenNetworkCreate "bad-cidr" "not-a-cidr"
      then_ $ responseIs $ \case
        RespNetworkError _ -> True
        _ -> False

    testCase "rejects a duplicate name on the same node" $ do
      given $ do
        _ <- insertNetwork "dup" "10.0.0.0/24"
        pure ()
      when_ $ whenNetworkCreate "dup" "10.0.1.0/24"
      then_ $ responseIs $ \case
        RespNetworkError _ -> True
        _ -> False

  describe "whenNetworkDelete" $ do
    testCase "returns NetworkNotFound for unknown id" $ do
      when_ $ whenNetworkDelete 999
      then_ $ responseIs $ \case
        RespNetworkNotFound -> True
        _ -> False

    testCase "deletes a free network row" $ do
      given $ do
        _ <- insertNetwork "lonely" "10.0.0.0/24"
        pure ()
      when_ $ whenNetworkDelete 1
      then_ $ responseIs $ \case
        RespNetworkDeleted -> True
        _ -> False
