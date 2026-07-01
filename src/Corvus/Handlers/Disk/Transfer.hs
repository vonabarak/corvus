{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Daemon-side orchestrator for inter-agent disk image transfer.
--
-- The daemon never carries bytes. Instead:
--
--   1. It asks the source agent to open the file via
--      'openReadViaAgent', getting a 'DiskReader' cap + token +
--      size + md5.
--   2. It asks the destination agent to dial the source via
--      'importFromPeerViaAgent', passing the source's host + port
--      + the token. The destination opens its own session to
--      the source, claims the reader via @attachReader@, and
--      streams bytes through a local file-writing sink.
--   3. On clean return, the destination has verified the size
--      and md5; the daemon updates the DB if/as the caller
--      requires.
--   4. Daemon drops the 'DiskReader' cap; the source closes the
--      open file handle.
--
-- Failures are reported as 'Left' with a human-readable
-- diagnostic. The agents handle partial-file cleanup on their
-- side; the daemon does not see any partial state.
module Corvus.Handlers.Disk.Transfer
  ( transferImageBetweenNodes
  , stageBackingChain
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import qualified Corvus.Handlers.Disk.Agent as DA
import qualified Corvus.Handlers.Disk.Db as DDb
import qualified Corvus.Handlers.Disk.Path as DP
import qualified Corvus.Model as M
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), get)
import Database.Persist.Sql (runSqlPool)
import System.FilePath (takeFileName, (</>))

-- | Move (well, copy) the contents of @srcPath@ on @srcNode@ to
-- @destPath@ on @destNode@, without routing bytes through the
-- daemon.
--
-- The caller is responsible for inserting / deleting the
-- 'DiskImageNode' DB rows that reflect the new placement.
transferImageBetweenNodes
  :: ServerState
  -> M.NodeId
  -- ^ source node
  -> M.NodeId
  -- ^ destination node
  -> FilePath
  -- ^ source path (absolute, as the source agent sees it)
  -> FilePath
  -- ^ destination path (absolute, as the destination agent sees
  -- it)
  -> IO (Either Text ())
transferImageBetweenNodes state srcNode destNode srcPath destPath = do
  -- Resolve the source node row to its @(host, port)@ — the
  -- destination agent dials it directly to claim the reader.
  mSrcNode <- runSqlPool (get srcNode) (ssDbPool state)
  case mSrcNode of
    Nothing ->
      pure $
        Left
          ( "transferImageBetweenNodes: source node "
              <> T.pack (show (M.fromSqlKey srcNode))
              <> " missing"
          )
    Just src -> runServerLogging state $ do
      let srcHost = M.nodeHost src
          srcPort = M.nodeNodeAgentPort src
      logInfoN $
        "starting inter-agent transfer "
          <> M.nodeName src
          <> ":"
          <> T.pack srcPath
          <> " → node "
          <> T.pack (show (M.fromSqlKey destNode))
          <> ":"
          <> T.pack destPath
      eOpen <- liftIO $ DA.openReadViaAgent state srcNode srcPath 600
      case eOpen of
        Left err -> do
          logWarnN $ "transfer aborted (diskOpenRead): " <> err
          pure (Left err)
        Right result -> do
          let token = NOA.dorToken result
              sz = NOA.dorSizeBytes result
              md5 = NOA.dorMd5 result
          logInfoN $
            "source opened: "
              <> T.pack (show sz)
              <> " bytes, md5 "
              <> md5
              <> "; handing off to destination"
          eImp <-
            liftIO $
              DA.importFromPeerViaAgent
                state
                destNode
                destPath
                srcHost
                srcPort
                token
                sz
                md5
          case eImp of
            Left err -> do
              logWarnN $ "transfer aborted (diskImportFromPeer): " <> err
              pure (Left err)
            Right () -> do
              logInfoN "inter-agent transfer complete"
              pure (Right ())

-- | Copy every missing ancestor of @primaryDisk@'s backing chain
-- from @srcNode@ to @destNode@. Copies run root-first (oldest
-- backing first) so a mid-chain failure leaves the destination in
-- a usable partial state: every overlay that landed already has
-- its backing present locally.
--
-- Returns @Right placements@ with the @(diskKey, storedRelPath)@
-- of newly-created 'DiskImageNode' rows on success, or @Left err@
-- on the first failure. No rollback — partial placements remain
-- on the destination and the operator can clean them up with
-- @crv disk delete@; this matches the opt-in semantics of the
-- @--with-backing-chain@ flag and keeps the helper simple.
--
-- Used by 'handleDiskCopy' \/ 'handleDiskMove' when the operator
-- opts in via @--with-backing-chain@. @vm migrate@ runs its own
-- chain orchestration via the 'MigrationPlan' machinery in
-- "Corvus.Handlers.Vm.Migrate" — sharing is at the primitive
-- level ('DDb.getBackingChainIds', 'DDb.hasPlacementOnNode',
-- 'transferImageBetweenNodes', 'DP.resolveDiskPath',
-- 'DDb.recordDiskImageNode', 'DDb.diskImageNodeFilePathFor').
stageBackingChain
  :: ServerState
  -> M.DiskImageId
  -- ^ primary disk whose chain we're staging
  -> M.NodeId
  -- ^ source node (where the ancestors are read from)
  -> M.NodeId
  -- ^ destination node (where placements get added)
  -> IO (Either Text [(M.DiskImageId, Text)])
stageBackingChain state primaryDisk srcNode destNode = do
  let pool = ssDbPool state
  chainNearestFirst <-
    runSqlPool (DDb.getBackingChainIds (M.fromSqlKey primaryDisk)) pool
  -- Root-first ordering: the immediate-parent first ordering from
  -- the DB walk would copy the top overlay before its ancestor on
  -- the destination, which is wrong if we ever crash mid-stage.
  let chainRootFirst = reverse chainNearestFirst
  daemonBase <- getEffectiveBasePath (ssQemuConfig state)
  mDestRow <- runSqlPool (get destNode) pool
  let destBase = maybe daemonBase (T.unpack . M.nodeBasePath) mDestRow
  go destBase [] chainRootFirst
  where
    pool = ssDbPool state
    go _ acc [] = pure (Right (reverse acc))
    go destBase acc (dkey : rest) = do
      alreadyOnDest <- runSqlPool (DDb.hasPlacementOnNode dkey destNode) pool
      if alreadyOnDest
        then go destBase acc rest
        else do
          srcAbs <- DP.resolveDiskPath pool (ssQemuConfig state) dkey srcNode
          mSrcStored <-
            runSqlPool (DDb.diskImageNodeFilePathFor dkey srcNode) pool
          if null srcAbs
            then
              pure $
                Left $
                  "stageBackingChain: source placement vanished for disk "
                    <> T.pack (show (M.fromSqlKey dkey))
            else do
              -- Preserve the source's relative subdirectory layout
              -- when present. Absolute source paths (off-base
              -- imported disks) rarely round-trip cleanly to a
              -- different host; fall back to the basename under
              -- the destination base, matching @Migrate.runOp@.
              let (destAbs, destRel) = case mSrcStored of
                    Just stored
                      | not ("/" `T.isPrefixOf` stored) ->
                          (destBase </> T.unpack stored, stored)
                    _ ->
                      let bn = takeFileName srcAbs
                       in (destBase </> bn, T.pack bn)
              tResult <-
                transferImageBetweenNodes state srcNode destNode srcAbs destAbs
              case tResult of
                Left err ->
                  pure $
                    Left $
                      "stageBackingChain: failed to copy ancestor "
                        <> T.pack (show (M.fromSqlKey dkey))
                        <> ": "
                        <> err
                Right () -> do
                  runSqlPool (DDb.recordDiskImageNode dkey destNode destRel) pool
                  go destBase ((dkey, destRel) : acc) rest
