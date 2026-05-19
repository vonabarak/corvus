{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for the SPICE TCP port allocator.
--
-- Uses the shared 'TestM' DSL (hence 'withTestDb') because the
-- allocator consults the DB for ports already assigned to running
-- VMs. Test ports are drawn from a high, unusual range to minimise
-- the chance of colliding with anything else on the machine.
module Corvus.SpicePortSpec (spec) where

import Control.Concurrent.Async (replicateConcurrently)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Corvus.Model as M
import Corvus.Node.SpicePort (allocateSpicePort, withAllocatedSpicePort)
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Types (ServerState (..))
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (insert)
import Database.Persist.Postgresql (runSqlPool)
import qualified Network.Socket as NS
import Test.DSL.Core (getDbPool, getTempDir, runDb)
import Test.DSL.When (createTestServerState)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "allocateSpicePort" $ do
    testCase "returns the low end of the range when nothing is allocated" $ do
      state <- mkState 55901 55901
      result <- liftIO $ allocateSpicePort state
      liftIO $ result `shouldBe` Right 55901

    testCase "skips a port already held by a running VM in the DB" $ do
      _ <- insertRunningVm "vm-skip-db" 55902
      state <- mkState 55902 55905
      result <- liftIO $ allocateSpicePort state
      liftIO $ case result of
        Right p -> p `shouldSatisfy` (\x -> x > 55902 && x <= 55905)
        Left err -> expectationFailure (T.unpack err)

    testCase "skips a port that is currently bound by another process" $ do
      state <- mkState 55910 55915
      result <- liftIO $ withBusyPort "127.0.0.1" 55910 $ \_ ->
        allocateSpicePort state
      liftIO $ case result of
        Right p -> p `shouldSatisfy` (\x -> x > 55910 && x <= 55915)
        Left err -> expectationFailure (T.unpack err)

    testCase "returns an error when every port in the range is already held in the DB" $ do
      _ <- insertRunningVm "vm-exhaust" 55920
      state <- mkState 55920 55920
      result <- liftIO $ allocateSpicePort state
      liftIO $ case result of
        Left msg -> msg `shouldSatisfy` ("no free SPICE port" `T.isPrefixOf`)
        Right p -> expectationFailure ("unexpected success: " <> show p)

  describe "withAllocatedSpicePort" $ do
    testCase "concurrent allocate+persist all yield distinct ports" $ do
      -- 16 parallel allocators against a 32-port range. The MVar lock
      -- inside withAllocatedSpicePort plus the in-callback DB write
      -- means every later allocator sees the earlier insert, so they
      -- must all pick disjoint ports.
      let n = 16 :: Int
          lo = 55950
          hi = 55981
      state <- mkState lo hi
      let pool = ssDbPool state
      results <- liftIO $ replicateConcurrently n $ do
        e <- withAllocatedSpicePort state $ \port -> do
          now <- getCurrentTime
          _ <-
            runSqlPool
              ( insert
                  M.Vm
                    { M.vmName = "conc-port-" <> T.pack (show port)
                    , M.vmCreatedAt = now
                    , M.vmStatus = M.VmRunning
                    , M.vmCpuCount = 1
                    , M.vmRamMb = 128
                    , M.vmDescription = Nothing
                    , M.vmHeadless = False
                    , M.vmGuestAgent = False
                    , M.vmCloudInit = False
                    , M.vmHealthcheck = Nothing
                    , M.vmAutostart = False
                    , M.vmSpicePort = Just port
                    , M.vmVsockCid = Nothing
                    }
              )
              pool
          pure port
        case e of
          Right p -> pure p
          Left err -> error ("allocator failed: " <> T.unpack err)
      liftIO $ do
        length results `shouldBe` n
        length (nub results) `shouldBe` n
        sort results `shouldSatisfy` all (\p -> p >= lo && p <= hi)

-- | Build a 'ServerState' whose @QemuConfig@ advertises @127.0.0.1@
-- and the supplied inclusive port range. Everything else is inherited
-- from 'createTestServerState'.
mkState :: Int -> Int -> TestM ServerState
mkState lo hi = do
  pool <- getDbPool
  tempDir <- getTempDir
  state <- liftIO $ createTestServerState pool tempDir
  let cfg = ssQemuConfig state
      cfg' =
        cfg
          { qcSpiceBindAddress = "127.0.0.1"
          , qcSpicePortMin = lo
          , qcSpicePortMax = hi
          }
  pure $ state {ssQemuConfig = cfg'}

-- | Insert a running VM holding the given SPICE port. Returns the
-- new DB id (not currently needed by callers, but mirrors the other
-- @insertVm@ helpers).
insertRunningVm :: Text -> Int -> TestM ()
insertRunningVm name port = do
  now <- liftIO getCurrentTime
  _ <-
    runDb $
      insert $
        M.Vm
          { M.vmName = name
          , M.vmCreatedAt = now
          , M.vmStatus = M.VmRunning
          , M.vmCpuCount = 1
          , M.vmRamMb = 128
          , M.vmDescription = Nothing
          , M.vmHeadless = False
          , M.vmGuestAgent = False
          , M.vmCloudInit = False
          , M.vmHealthcheck = Nothing
          , M.vmAutostart = False
          , M.vmSpicePort = Just port
          , M.vmVsockCid = Nothing
          }
  pure ()

-- | Bind a socket to @(addr, port)@ for the duration of the action,
-- ensuring that port appears busy to 'allocateSpicePort'.
withBusyPort :: String -> Int -> (NS.Socket -> IO a) -> IO a
withBusyPort host port =
  bracket acquire NS.close
  where
    acquire = do
      let hints =
            NS.defaultHints
              { NS.addrFlags = [NS.AI_NUMERICHOST, NS.AI_NUMERICSERV]
              , NS.addrSocketType = NS.Stream
              }
      infos <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
      case infos of
        (addr : _) -> do
          sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
          NS.bind sock (NS.addrAddress addr)
          pure sock
        [] -> error "withBusyPort: address did not resolve"
