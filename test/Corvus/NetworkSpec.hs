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

import Corvus.Action (runAction)
import Corvus.Handlers.Network (NetworkCreate (..), NetworkEdit (..), NetworkStart (..), NetworkStop (..))
import qualified Corvus.Model as M
import qualified Corvus.Protocol as P
import qualified Data.Text as T
import Database.Persist (get, update, (=.))
import Test.DSL.Core (runDb)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenNetworkList" $ do
    testCase "returns an empty list when no networks exist" $ do
      _ <- when_ whenNetworkList
      then_ $ responseIs $ \case
        RespNetworkList [] -> True
        _ -> False

    testCase "returns one row per inserted network" $ do
      given $ do
        _ <- insertNetwork "n1" "10.0.0.0/24"
        _ <- insertNetwork "n2" "10.0.1.0/24"
        pure ()
      _ <- when_ whenNetworkList
      then_ $ responseIs $ \case
        RespNetworkList xs -> length xs == 2
        _ -> False

  describe "whenNetworkShow" $ do
    testCase "returns details for an existing network" $ do
      given $ do
        _ <- insertNetwork "n" "10.0.0.0/24"
        pure ()
      _ <- when_ $ whenNetworkShow 1
      then_ $ responseIs $ \case
        RespNetworkDetails _ -> True
        _ -> False

    testCase "returns NetworkNotFound for unknown id" $ do
      _ <- when_ $ whenNetworkShow 999
      then_ $ responseIs $ \case
        RespNetworkNotFound -> True
        _ -> False

  describe "whenNetworkCreate" $ do
    testCase "writes a network row in the stopped state" $ do
      _ <- when_ $ whenNetworkCreate "fresh" "10.0.0.0/24"
      then_ $ responseIs $ \case
        RespNetworkCreated _ -> True
        _ -> False

    testCase "rejects an invalid CIDR" $ do
      _ <- when_ $ whenNetworkCreate "bad-cidr" "not-a-cidr"
      then_ $ responseIs $ \case
        RespNetworkError _ -> True
        _ -> False

    testCase "rejects a duplicate name on the same node" $ do
      given $ do
        _ <- insertNetwork "dup" "10.0.0.0/24"
        pure ()
      _ <- when_ $ whenNetworkCreate "dup" "10.0.1.0/24"
      then_ $ responseIs $ \case
        RespNetworkError _ -> True
        _ -> False

  describe "whenNetworkDelete" $ do
    testCase "returns NetworkNotFound for unknown id" $ do
      _ <- when_ $ whenNetworkDelete 999
      then_ $ responseIs $ \case
        RespNetworkNotFound -> True
        _ -> False

    testCase "deletes a free network row" $ do
      given $ do
        _ <- insertNetwork "lonely" "10.0.0.0/24"
        pure ()
      _ <- when_ $ whenNetworkDelete 1
      then_ $ responseIs $ \case
        RespNetworkDeleted -> True
        _ -> False

  describe "network edit actions" $ do
    testCase "normalizes CIDR and persists every editable field" $ do
      ident <- insertNetwork "net" "10.0.0.0/24"
      _ <- withState (\st -> runAction st "alice" (NetworkEdit ident (Just "10.1.2.3/24") (Just True) (Just True) (Just True) (Just ["1.1.1.1", "8.8.8.8"]) (Just "example.test") (Just False)))
      responseIs (== RespNetworkEdited)
      _ <- whenNetworkShow ident
      responseIs $ \case
        RespNetworkDetails info -> (P.nwiSubnet info, P.nwiDhcp info, P.nwiNat info, P.nwiAutostart info, P.nwiDnsServers info, P.nwiDomain info, P.nwiHostDns info) == ("10.1.2.0/24", True, True, True, ["1.1.1.1", "8.8.8.8"], "example-test", False)
        _ -> False
      _ <- withState (\st -> runAction st "alice" (NetworkEdit ident (Just "") (Just False) (Just False) Nothing (Just []) (Just "") (Just True)))
      responseIs (== RespNetworkEdited)
      row <- runDb $ get (M.toSqlKey ident :: M.NetworkId)
      liftIO $ fmap (\nw -> (M.networkSubnet nw, M.networkDomain nw, M.networkDnsServers nw)) row `shouldBe` Just ("", "", "")
    testCase "allows no-op and autostart changes while running" $ do
      ident <- insertNetwork "net" "10.0.0.0/24"
      runDb $ update (M.toSqlKey ident :: M.NetworkId) [M.NetworkRunning =. True]
      _ <- withState (\st -> runAction st "alice" (NetworkEdit ident Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
      responseIs (== RespNetworkEdited)
      _ <- withState (\st -> runAction st "alice" (NetworkEdit ident Nothing Nothing Nothing (Just True) Nothing Nothing Nothing))
      responseIs (== RespNetworkEdited)
      row <- runDb $ get (M.toSqlKey ident :: M.NetworkId)
      liftIO $ M.networkAutostart <$> row `shouldBe` Just True
    mapM_
      ( \(label, edit, fragment, running) -> testCase label $ do
          ident <- insertNetwork "net" "10.0.0.0/24"
          runDb $ update (M.toSqlKey ident :: M.NetworkId) [M.NetworkRunning =. running]
          originalRow <- runDb $ get (M.toSqlKey ident :: M.NetworkId)
          _ <- withState (\st -> runAction st "alice" (edit ident))
          responseIs $ \case
            RespNetworkError message -> fragment `T.isInfixOf` message
            _ -> False
          updatedRow <- runDb $ get (M.toSqlKey ident :: M.NetworkId)
          liftIO $ updatedRow `shouldBe` originalRow
      )
      [ ("rejects invalid CIDR", \i -> NetworkEdit i (Just "bad") Nothing Nothing Nothing Nothing Nothing Nothing, "Invalid subnet", False)
      , ("rejects DHCP without subnet", \i -> NetworkEdit i (Just "") (Just True) Nothing Nothing Nothing Nothing Nothing, "DHCP requires a subnet", False)
      , ("rejects NAT without subnet", \i -> NetworkEdit i (Just "") Nothing (Just True) Nothing Nothing Nothing Nothing, "NAT requires a subnet", False)
      , ("rejects subnet changes while running", \i -> NetworkEdit i (Just "10.1.0.0/24") Nothing Nothing Nothing Nothing Nothing Nothing, "must be stopped", True)
      , ("rejects DHCP changes while running", \i -> NetworkEdit i Nothing (Just True) Nothing Nothing Nothing Nothing Nothing, "must be stopped", True)
      , ("rejects NAT changes while running", \i -> NetworkEdit i Nothing Nothing (Just True) Nothing Nothing Nothing Nothing, "must be stopped", True)
      , ("rejects DNS changes while running", \i -> NetworkEdit i Nothing Nothing Nothing Nothing (Just []) Nothing Nothing, "must be stopped", True)
      , ("rejects domain changes while running", \i -> NetworkEdit i Nothing Nothing Nothing Nothing Nothing (Just "") Nothing, "must be stopped", True)
      , ("rejects host DNS changes while running", \i -> NetworkEdit i Nothing Nothing Nothing Nothing Nothing Nothing (Just False), "must be stopped", True)
      ]
    testCase "reports missing networks for edit/start/stop" $ do
      _ <- withState (\st -> runAction st "alice" (NetworkEdit 999 Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
      responseIs (== RespNetworkNotFound)
      _ <- withState (\st -> runAction st "alice" (NetworkStart 999))
      responseIs (== RespNetworkNotFound)
      _ <- withState (\st -> runAction st "alice" (NetworkStop 999 False))
      responseIs (== RespNetworkNotFound)
    testCase "fails to start without a netagent and keeps the network stopped" $ do
      ident <- insertNetwork "net" "10.0.0.0/24"
      _ <- withState (\st -> runAction st "alice" (NetworkStart ident))
      responseIs $ \case
        RespNetworkError _ -> True
        _ -> False
      row <- runDb $ get (M.toSqlKey ident :: M.NetworkId)
      liftIO $ M.networkRunning <$> row `shouldBe` Just False
    testCase "rejects a requested node that does not exist" $ do
      _ <- withState (\st -> runAction st "alice" (NetworkCreate "net" "missing" "10.0.0.0/24" False False False [] "" True))
      responseIs $ \case
        RespNetworkError _ -> True
        RespNodeNotFound -> True
        _ -> False
