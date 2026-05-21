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
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import qualified Corvus.Handlers.Disk.Agent as DA
import qualified Corvus.Model as M
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), get)
import Database.Persist.Postgresql (runSqlPool)

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
