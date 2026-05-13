{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Smoke test that the Cap'n Proto RPC server bootstrap actually
-- accepts a connection and serves the bootstrap interface. This is
-- the Phase 3 integration check called for in the migration plan —
-- it confirms the second-socket listener wiring works end-to-end on
-- a temp Unix socket.
--
-- Method coverage is intentionally narrow (just @ping@); the
-- per-subsystem methods are exercised through 'Corvus.Wire' and the
-- existing handler unit tests, and will be fully driven from
-- 'crv' once Phase 4 lands.
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
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracket)
import qualified Corvus.Qemu.Config as Q
import Corvus.Rpc.Server (runCapnpServer)
import Corvus.Types (ListenAddress (..), ServerState, newServerState)
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
  describe "Cap'n Proto RPC bootstrap" $ do
    it "accepts a connection and responds to daemon.ping" $ \env -> do
      withSystemTempDirectory "corvus-capnp-test" $ \tmp -> do
        let sockPath = tmp </> "corvus.capnp.sock"
        state <- newServerState (tePool env) Q.defaultQemuConfig
        bracket
          (async (runCapnpServer state (UnixAddress sockPath)))
          cancel
          $ \_serverThread -> do
            waitForSocket sockPath
            bracket (socket AF_UNIX Stream 0) close $ \sock -> do
              connect sock (SockAddrUnix sockPath)
              withConn
                (socketTransport sock C.defaultLimit)
                (C.def {debugMode = False})
                $ \conn -> do
                  rawClient <- requestBootstrap conn
                  let daemonClient :: C.Client CGCorvus.Daemon
                      daemonClient = fromClient rawClient
                  _ <-
                    daemonClient
                      & C.callP #ping C.def
                      >>= C.waitPipeline
                  pure ()

waitForSocket :: FilePath -> IO ()
waitForSocket p = go (50 :: Int)
  where
    go 0 = error ("Cap'n Proto socket did not appear: " <> p)
    go n = do
      ok <- Dir.doesPathExist p
      if ok then pure () else threadDelay 10000 >> go (n - 1)
