{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the generic reference resolver in
-- "Corvus.Handlers.Resolve". Every "give me X by name or id"
-- handler funnels through these helpers, so the corner cases
-- (numeric strings that don't correspond to an id, ambiguous
-- names after multi-node, all-digit-name rejection) need to be
-- pinned down here rather than relived in every callsite spec.
module Corvus.ResolveSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Corvus.Handlers.Resolve
  ( resolveDisk
  , resolveNetwork
  , resolveSshKey
  , resolveTemplate
  , resolveVm
  , validateName
  )
import qualified Corvus.Model as M
import Corvus.Protocol (Ref (..))
import Data.Either (isLeft)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist (insert)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (fromSqlKey)
import Test.DSL.Core (TestM, getDbPool, runDb)
import Test.Prelude

withPool :: (Pool SqlBackend -> IO a) -> TestM a
withPool action = do
  pool <- getDbPool
  liftIO $ action pool

spec :: Spec
spec = sequential $ withTestDb $ do
  ----------------------------------------------------------------
  -- validateName (pure)

  describe "validateName" $ do
    testCase "rejects an empty name" $
      liftIO $
        validateName "VM" "" `shouldSatisfy` isLeft
    testCase "rejects an all-digit name (would collide with id refs)" $
      liftIO $
        validateName "VM" "12345" `shouldSatisfy` isLeft
    testCase "accepts a normal name" $
      liftIO $
        validateName "VM" "web-1" `shouldBe` Right ()
    testCase "accepts a name that starts with digits but isn't all-digits" $
      liftIO $
        validateName "VM" "42web" `shouldBe` Right ()

  ----------------------------------------------------------------
  -- resolveVm: cluster-wide name lookup that has to handle
  -- ambiguity (the same VM name can exist on different nodes).

  describe "resolveVm" $ do
    testCase "resolves a numeric ref that exists" $ do
      vmId <- insertVm "v1" VmStopped
      r <- withPool $ resolveVm (Ref "1")
      liftIO $ r `shouldBe` Right vmId

    testCase "returns Left for a numeric ref that doesn't exist" $ do
      r <- withPool $ resolveVm (Ref "999")
      liftIO $ r `shouldSatisfy` isLeft

    testCase "resolves a name when exactly one VM matches" $ do
      _ <- insertVm "alpha" VmStopped
      r <- withPool $ resolveVm (Ref "alpha")
      liftIO $ case r of
        Right _ -> pure ()
        Left e -> fail $ "expected Right, got: " <> show e

    testCase "returns Left for an unknown name" $ do
      r <- withPool $ resolveVm (Ref "no-such-vm")
      liftIO $ r `shouldSatisfy` isLeft

    testCase "errors on ambiguous names that match multiple nodes" $ do
      -- Two VMs with the same name on different nodes (post
      -- multi-node, 'UniqueVmNamePerNode' is per-node, not
      -- cluster-wide). The resolver should refuse rather than
      -- pick one.
      let node1 = M.toSqlKey 1 :: M.NodeId
      node2 <- insertOtherNode "second-node" 19878
      _ <- insertVmOnNode "shared" node1
      _ <- insertVmOnNode "shared" node2
      r <- withPool $ resolveVm (Ref "shared")
      liftIO $ r `shouldSatisfy` isLeft

  ----------------------------------------------------------------
  -- resolveDisk + resolveSshKey: simpler, cluster-unique name
  -- constraint. The two share the underlying 'resolveRef'
  -- helper so a smoke test on each is enough.

  describe "resolveDisk" $ do
    testCase "resolves a numeric ref that exists" $ do
      diskId <- insertDiskImage "img1" "/tmp/img1.qcow2" FormatQcow2
      r <- withPool $ resolveDisk (Ref "1")
      liftIO $ r `shouldBe` Right diskId

    testCase "returns Left for a numeric ref that doesn't exist" $ do
      r <- withPool $ resolveDisk (Ref "12345")
      liftIO $ r `shouldSatisfy` isLeft

    testCase "resolves an existing disk by name" $ do
      _ <- insertDiskImage "ubuntu" "/tmp/u.qcow2" FormatQcow2
      r <- withPool $ resolveDisk (Ref "ubuntu")
      liftIO $ case r of
        Right _ -> pure ()
        Left e -> fail $ "expected Right, got: " <> show e

  describe "resolveSshKey" $ do
    testCase "resolves a numeric ref that exists" $ do
      keyId <- insertSshKey "k" "ssh-ed25519 AAAA-k"
      r <- withPool $ resolveSshKey (Ref "1")
      liftIO $ r `shouldBe` Right keyId

    testCase "returns Left for a name that doesn't exist" $ do
      r <- withPool $ resolveSshKey (Ref "ghost")
      liftIO $ r `shouldSatisfy` isLeft

  describe "resolveTemplate" $ do
    testCase "returns Left for a non-existent template id" $ do
      r <- withPool $ resolveTemplate (Ref "999")
      liftIO $ r `shouldSatisfy` isLeft

  describe "resolveNetwork" $ do
    testCase "resolves an existing network by name" $ do
      _ <- insertNetwork "net1" "10.10.0.0/24"
      r <- withPool $ resolveNetwork (Ref "net1")
      liftIO $ case r of
        Right _ -> pure ()
        Left e -> fail $ "expected Right, got: " <> show e

    testCase "returns Left for an unknown network name" $ do
      r <- withPool $ resolveNetwork (Ref "no-net")
      liftIO $ r `shouldSatisfy` isLeft

-- | Insert a second 'Node' so we can construct an ambiguous-name
-- scenario for 'resolveVm'.
insertOtherNode :: Text -> Int -> TestM M.NodeId
insertOtherNode name port = do
  now <- liftIO getCurrentTime
  runDb $
    insert
      M.Node
        { M.nodeName = name
        , M.nodeHost = "127.0.0.2"
        , M.nodeNodeAgentPort = port
        , M.nodeNetAgentPort = port + 1
        , M.nodeBasePath = "/tmp"
        , M.nodeDescription = Nothing
        , M.nodeAdminState = M.NodeOnline
        , M.nodeCreatedAt = now
        , M.nodeCpuCount = Nothing
        , M.nodeRamMbTotal = Nothing
        , M.nodeRamMbFree = Nothing
        , M.nodeStorageBytesTotal = Nothing
        , M.nodeStorageBytesFree = Nothing
        , M.nodeLoadAvg1 = Nothing
        , M.nodeLoadAvg5 = Nothing
        , M.nodeLoadAvg15 = Nothing
        , M.nodeKernelRelease = Nothing
        , M.nodeAgentVersion = Nothing
        , M.nodeNodeAgentHealthcheck = Nothing
        , M.nodeNetAgentHealthcheck = Nothing
        , M.nodeNetdDisabled = False
        }

-- | Insert a 'Vm' against an explicit node (bypass the default
-- 'test-node'-only DSL helper).
insertVmOnNode :: Text -> M.NodeId -> TestM Int64
insertVmOnNode name nodeKey = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        M.Vm
          { M.vmName = name
          , M.vmNodeId = nodeKey
          , M.vmCreatedAt = now
          , M.vmStatus = M.VmStopped
          , M.vmCpuCount = 2
          , M.vmRamMb = 4096
          , M.vmDescription = Nothing
          , M.vmHeadless = False
          , M.vmGuestAgent = False
          , M.vmCloudInit = False
          , M.vmHealthcheck = Nothing
          , M.vmAutostart = False
          , M.vmSpicePort = Nothing
          , M.vmVsockCid = Nothing
          , M.vmErrorMessage = Nothing
          , M.vmLastErrorAt = Nothing
          , M.vmRebootQuirk = False
          , M.vmCpuModel = "host"
          }
  pure $ fromSqlKey key
