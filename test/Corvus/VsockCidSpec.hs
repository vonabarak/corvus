{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for the AF_VSOCK CID allocator.
--
-- Uses the shared 'TestM' DSL (hence 'withTestDb') because the
-- allocator consults the DB for CIDs already assigned to VMs. Test
-- CIDs are drawn from a high, unusual range to minimise the chance
-- of colliding with anything else on the machine.
module Corvus.VsockCidSpec (spec) where

import Control.Concurrent.Async (replicateConcurrently)
import Control.Monad.IO.Class (liftIO)
import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Qemu.VsockCid (allocateVsockCid, withAllocatedVsockCid)
import Corvus.Types (ServerState (..))
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (insert)
import Database.Persist.Postgresql (runSqlPool)
import Test.DSL.Core (getDbPool, getTempDir, runDb)
import Test.DSL.When (createTestServerState)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "allocateVsockCid" $ do
    testCase "returns the low end of the range when nothing is allocated" $ do
      state <- mkState 950001 950001
      result <- liftIO $ allocateVsockCid state
      liftIO $ result `shouldBe` Right 950001

    testCase "skips a CID already held by a VM in the DB" $ do
      _ <- insertVmWithCid "vm-cid-skip-db" 950010
      state <- mkState 950010 950015
      result <- liftIO $ allocateVsockCid state
      liftIO $ case result of
        Right c -> c `shouldSatisfy` (\x -> x > 950010 && x <= 950015)
        Left err -> expectationFailure (T.unpack err)

    testCase "returns an error when every CID in the range is taken in the DB" $ do
      _ <- insertVmWithCid "vm-cid-exhaust" 950020
      state <- mkState 950020 950020
      result <- liftIO $ allocateVsockCid state
      liftIO $ case result of
        Left msg -> msg `shouldSatisfy` ("no free vsock CID" `T.isPrefixOf`)
        Right c -> expectationFailure ("unexpected success: " <> show c)

  describe "withAllocatedVsockCid" $ do
    testCase "concurrent allocate+persist all yield distinct CIDs" $ do
      -- 32 parallel allocators against a 64-CID range. The MVar lock
      -- inside withAllocatedVsockCid plus the in-callback DB write
      -- means every later allocator sees the earlier insert, so they
      -- must all pick disjoint CIDs.
      let n = 32 :: Int
          lo = 951001
          hi = 951064
      state <- mkState lo hi
      let pool = ssDbPool state
      results <- liftIO $ replicateConcurrently n $ do
        e <- withAllocatedVsockCid state $ \cid -> do
          now <- getCurrentTime
          _ <-
            runSqlPool
              ( insert
                  ( vmStubWithCid
                      now
                      ("conc-cid-" <> T.pack (show cid))
                      cid
                  )
              )
              pool
          pure cid
        case e of
          Right cid -> pure cid
          Left err -> error ("allocator failed: " <> T.unpack err)
      liftIO $ do
        length results `shouldBe` n
        length (nub results) `shouldBe` n
        sort results `shouldSatisfy` all (\c -> c >= lo && c <= hi)

-- | Build a 'ServerState' whose @QemuConfig@ advertises the supplied
-- inclusive CID range. Everything else is inherited from
-- 'createTestServerState'.
mkState :: Int -> Int -> TestM ServerState
mkState lo hi = do
  pool <- getDbPool
  tempDir <- getTempDir
  state <- liftIO $ createTestServerState pool tempDir
  let cfg = ssQemuConfig state
      cfg' = cfg {qcVsockCidMin = lo, qcVsockCidMax = hi}
  pure $ state {ssQemuConfig = cfg'}

-- | Insert a VM row holding the given vsock CID.
insertVmWithCid :: Text -> Int -> TestM ()
insertVmWithCid name cid = do
  now <- liftIO getCurrentTime
  _ <- runDb $ insert $ vmStubWithCid now name cid
  pure ()

-- | Minimal 'M.Vm' record with the supplied name + CID; all other
-- fields take sensible defaults so the row inserts without errors.
vmStubWithCid :: UTCTime -> Text -> Int -> M.Vm
vmStubWithCid now name cid =
  M.Vm
    { M.vmName = name
    , M.vmCreatedAt = now
    , M.vmStatus = M.VmStopped
    , M.vmCpuCount = 1
    , M.vmRamMb = 128
    , M.vmDescription = Nothing
    , M.vmPid = Nothing
    , M.vmHeadless = False
    , M.vmGuestAgent = False
    , M.vmCloudInit = False
    , M.vmHealthcheck = Nothing
    , M.vmAutostart = False
    , M.vmSpicePort = Nothing
    , M.vmVsockCid = Just cid
    }
