{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end smoke tests for the Cap'n Proto wire.
--
-- Exercises the Phase 3 server (listener + Daemon cap) together
-- with the Phase 4d 'CapnpConnection' and the Phase 4e Rpc
-- wrappers. The intent is to confirm that the bootstrap +
-- client-helper path actually goes round-trip on a temp Unix
-- socket; deeper per-method coverage lives in subsystem-specific
-- tests once the test DSL is flipped over in Phase 4f.
module Corvus.CapnpServerSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Enums as CapnpEnums
import Capnp.Rpc
  ( ConnConfig (..)
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, bracket_, catch)
import qualified Corvus.Client.Capnp.Connection as CC
import qualified Corvus.Client.Capnp.Rpc as CR
import qualified Corvus.Model as M
import Corvus.Node.Server (runNodeAgentServer)
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.Protocol as P
import qualified Corvus.Qemu.Config as Q
import Corvus.Rpc.Server (runCapnpServer)
import Corvus.Types
  ( ListenAddress (..)
  , NodeConns (..)
  , ServerState (..)
  , newAutostartFlags
  , newServerState
  , registerNodeConns
  )
import qualified Corvus.Wire.Common as WC
import Data.Function ((&))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Database.Persist.Sql (toSqlKey)
import Network.Socket
  ( Family (..)
  , SockAddr (..)
  , SocketType (..)
  , close
  , connect
  , socket
  )
import qualified Network.Socket as NS
import qualified System.Directory as Dir
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Database (TestEnv (..))
import Test.Hspec
import Test.Prelude (withTestDb)

spec :: Spec
spec = withTestDb $ do
  describe "Cap'n Proto RPC bootstrap (raw socket)" $ do
    it "accepts a connection and responds to daemon.ping" $ \env -> do
      withSystemTempDirectory "corvus-capnp-test" $ \tmp -> do
        let sockPath = tmp </> "corvus.capnp.sock"
        state <- newServerState (tePool env) Q.defaultQemuConfig
        bracket
          (async (runCapnpServer state (UnixAddress sockPath)))
          cancel
          $ \_ -> do
            waitForSocket sockPath
            bracket (socket AF_UNIX Stream 0) close $ \sock -> do
              connect sock (SockAddrUnix sockPath)
              withConn
                (socketTransport sock C.defaultLimit)
                (C.def {debugMode = False})
                $ \conn -> do
                  rawClient <- requestBootstrap conn
                  let daemon :: C.Client CGCorvus.Daemon
                      daemon = fromClient rawClient
                  _ <-
                    daemon
                      & C.callP #ping C.def
                      >>= C.waitPipeline
                  pure ()

  describe "Corvus.Client.Capnp client wrappers" $ do
    it "rpcPing round-trips" $ \env -> do
      withCapnpDaemon env $ \conn -> CR.rpcPing conn

    it "rpcStatus returns sensible values" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        P.StatusInfo {P.siUptime, P.siConnections} <- CR.rpcStatus conn
        siUptime `shouldSatisfy` (>= 0)
        siConnections `shouldSatisfy` (>= 0)

    it "rpcVmList returns an empty list initially" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        vms <- CR.rpcVmList conn
        vms `shouldBe` []

    -- Smoke-test that each list cap rounds-trips. We only check
    -- the calls succeed (don't compare against @[]@) because the
    -- whole spec shares one test DB and hspec randomizes the
    -- order within a 'describe' block; another test in this file
    -- might have left a row behind by the time this one runs.
    it "rpcDiskList / rpcNetworkList / rpcSshKeyList / rpcTemplateList round-trip" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        _ <- CR.rpcDiskList conn
        _ <- CR.rpcNetworkList conn
        _ <- CR.rpcSshKeyList conn
        _ <- CR.rpcTemplateList conn
        pure ()

  describe "Cap'n Proto mutation wrappers (Phase 4f)" $ do
    it "VM lifecycle: create → list (length 1) → delete → list (empty)" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        -- Empty node ref → daemon's scheduler picks the seeded test-node.
        vid <- CR.rpcVmCreate conn "spec-vm" "" 1 1024 Nothing True False False False False "host"
        vms <- CR.rpcVmList conn
        length vms `shouldBe` 1
        CR.rpcVmDelete conn (WC.RefById vid) False
        vmsAfter <- CR.rpcVmList conn
        vmsAfter `shouldBe` []

    it "SSH key lifecycle: create → list → delete → list (empty)" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        kid <-
          CR.rpcSshKeyCreate
            conn
            "spec-key"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE0123456789abcdef test@example"
        keys <- CR.rpcSshKeyList conn
        length keys `shouldBe` 1
        CR.rpcSshKeyDelete conn (WC.RefById kid)
        keysAfter <- CR.rpcSshKeyList conn
        keysAfter `shouldBe` []

    it "Disk lifecycle: create → list → delete → list (empty)" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        did <- CR.rpcDiskCreate conn "spec-disk" 10 CapnpEnums.DriveFormat'qcow2 False (WC.RefById 0)
        ds <- CR.rpcDiskList conn
        length ds `shouldBe` 1
        CR.rpcDiskDelete conn (WC.RefById did)
        dsAfter <- CR.rpcDiskList conn
        dsAfter `shouldBe` []

  describe "Cap'n Proto streaming (Phase 6a)" $ do
    it "rpcBuild streams events through BuildEventSink until end" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        eventsRef <- newIORef ([] :: [P.BuildEvent])
        done <- newEmptyMVar
        let onEvent ev = atomicModifyIORef' eventsRef (\xs -> (xs ++ [ev], ()))
            onEnd = putMVar done ()
        tid <- CR.rpcBuild conn "" onEvent onEnd
        tid `shouldSatisfy` (> 0)
        -- Give the daemon at most 10 s to finish even on an empty
        -- (invalid) YAML — it should fail validation quickly and
        -- close the sink.
        finished <- timeout 10000000 (takeMVar done)
        finished `shouldBe` Just ()
        evs <- readIORef eventsRef
        evs `shouldSatisfy` (not . null)
        -- The final event is always PipelineEnd, regardless of
        -- success/failure.
        case last evs of
          P.PipelineEnd _ -> pure ()
          other -> expectationFailure ("last event is " <> show other)

waitForSocket :: FilePath -> IO ()
waitForSocket p = go (50 :: Int)
  where
    go 0 = error ("Cap'n Proto socket did not appear: " <> p)
    go n = do
      ok <- Dir.doesPathExist p
      if ok then pure () else threadDelay 10000 >> go (n - 1)

-- | Run an action against the new CapnpConnection on a temp
-- Unix socket. Spawns the daemon's Cap'n Proto server, an
-- in-process @corvus-nodeagent@ on a free TCP port, and a daemon→
-- agent connection that's stashed in 'ssNodeAgent' for handlers
-- that route disk / cloud-init through the agent. Tears
-- everything down on exit.
withCapnpDaemon :: TestEnv -> (CC.CapnpConnection -> IO a) -> IO a
withCapnpDaemon env action =
  withSystemTempDirectory "corvus-capnp-test" $ \tmp ->
    -- Sandbox XDG_RUNTIME_DIR so the in-process nodeagent's
    -- startup/shutdown cleanup (which 'pgrep'-kills QEMU +
    -- virtiofsd and wipes the agent's runtime dir) only ever
    -- targets entries under @tmp/corvus@ — never the
    -- developer's real /run/user/<UID>/corvus that the host
    -- daemon owns. Bracket the env-var change so a later test
    -- (or post-suite shell) sees whatever was inherited.
    withSandboxedXdg tmp $ do
      let sockPath = tmp </> "corvus.capnp.sock"
      state <- newServerState (tePool env) Q.defaultQemuConfig
      naPort <- pickFreePort
      bracket
        (async (runNodeAgentServer "127.0.0.1" naPort Nothing))
        cancel
        $ \_ -> do
          waitForTcp "127.0.0.1" naPort
          NOA.withNodeAgentClient "127.0.0.1" naPort "capnp-spec" Nothing $ \nr -> do
            nac <- case nr of
              Left e -> error ("nodeagent dial failed: " <> show e)
              Right c -> pure c
            -- Register the in-process nodeagent under nodeId=1
            -- (the test DB's bootstrap 'test-node' row that
            -- 'insertDefaultTestNode' installs at id 1). The
            -- 'ncSupervisor' is a stub async — the test's own
            -- bracket controls the connection lifetime, not the
            -- supervisor.
            stubSup <- async (pure ())
            (vmAS, netAS) <- newAutostartFlags
            registerNodeConns
              state
              (toSqlKey 1 :: M.NodeId)
              NodeConns
                { ncNodeAgent = Just nac
                , ncNetAgent = Nothing
                , ncSupervisor = stubSup
                , ncVmAutostartFired = vmAS
                , ncNetAutostartFired = netAS
                }
            bracket
              (async (runCapnpServer state (UnixAddress sockPath)))
              cancel
              $ \_ -> do
                waitForSocket sockPath
                r <- CC.withCapnpConnection (UnixAddress sockPath) Nothing action
                case r of
                  Right a -> pure a
                  Left e -> error (show e)

-- | Run @action@ with @XDG_RUNTIME_DIR@ pointed at @sandbox@,
-- restoring the previous value (or unsetting it if absent) on
-- exit. Used by 'withCapnpDaemon' to confine the in-process
-- nodeagent's startup-and-shutdown cleanup to a temp directory
-- — otherwise the cleanup would 'pgrep'-kill the host daemon's
-- QEMU children and wipe @/run/user/\<UID\>/corvus/@.
withSandboxedXdg :: FilePath -> IO a -> IO a
withSandboxedXdg sandbox action = do
  prev <- lookupEnv "XDG_RUNTIME_DIR"
  let restore = case prev of
        Just v -> setEnv "XDG_RUNTIME_DIR" v
        Nothing -> unsetEnv "XDG_RUNTIME_DIR"
  bracket_ (setEnv "XDG_RUNTIME_DIR" sandbox) restore action

-- | Ask the kernel for a free TCP port on 127.0.0.1. Mild race
-- between close-then-bind; in practice unobservable for serial
-- unit tests.
pickFreePort :: IO Int
pickFreePort = do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.bind sock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
  p <- NS.socketPort sock
  NS.close sock
  pure (fromIntegral p)

-- | Poll for the agent's TCP listener to come up. Up to ~500 ms.
waitForTcp :: String -> Int -> IO ()
waitForTcp host port = go (50 :: Int)
  where
    go 0 = error ("nodeagent listener did not appear: " <> host <> ":" <> show port)
    go n = do
      r <-
        bracket
          (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol)
          NS.close
          $ \s -> do
            ais <- NS.getAddrInfo Nothing (Just host) (Just (show port))
            case ais of
              [] -> pure False
              (ai : _) ->
                fmap (const True) (NS.connect s (NS.addrAddress ai))
                  `catch` (\(_ :: SomeException) -> pure False)
      if r then pure () else threadDelay 10000 >> go (n - 1)
