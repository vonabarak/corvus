{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Capnp.Rpc
  ( ConnConfig (..)
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import qualified Corvus.Client.Capnp.Connection as CC
import qualified Corvus.Client.Capnp.Rpc as CR
import qualified Corvus.Protocol as P
import qualified Corvus.Qemu.Config as Q
import Corvus.Rpc.Server (runCapnpServer)
import Corvus.Types (ListenAddress (..), newServerState)
import Data.Function ((&))
import Network.Socket
  ( Family (..)
  , SockAddr (..)
  , SocketType (..)
  , close
  , connect
  , socket
  )
import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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

    it "rpcDiskList / rpcNetworkList / rpcSshKeyList / rpcTemplateList all start empty" $ \env -> do
      withCapnpDaemon env $ \conn -> do
        ds <- CR.rpcDiskList conn
        ds `shouldBe` []
        ns <- CR.rpcNetworkList conn
        ns `shouldBe` []
        ks <- CR.rpcSshKeyList conn
        ks `shouldBe` []
        ts <- CR.rpcTemplateList conn
        ts `shouldBe` []

waitForSocket :: FilePath -> IO ()
waitForSocket p = go (50 :: Int)
  where
    go 0 = error ("Cap'n Proto socket did not appear: " <> p)
    go n = do
      ok <- Dir.doesPathExist p
      if ok then pure () else threadDelay 10000 >> go (n - 1)

-- | Run an action against the new CapnpConnection on a temp
-- Unix socket. Spawns the server, dials it, runs the action,
-- tears everything down.
withCapnpDaemon :: TestEnv -> (CC.CapnpConnection -> IO a) -> IO a
withCapnpDaemon env action =
  withSystemTempDirectory "corvus-capnp-test" $ \tmp -> do
    let sockPath = tmp </> "corvus.capnp.sock"
    state <- newServerState (tePool env) Q.defaultQemuConfig
    bracket
      (async (runCapnpServer state (UnixAddress sockPath)))
      cancel
      $ \_ -> do
        waitForSocket sockPath
        r <- CC.withCapnpConnection (UnixAddress sockPath) action
        case r of
          Right a -> pure a
          Left e -> error (show e)
