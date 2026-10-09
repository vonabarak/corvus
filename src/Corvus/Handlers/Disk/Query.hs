{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Query disk-image handlers.
module Corvus.Handlers.Disk.Query
  ( handleDiskList
  , handleDiskShow
  )
where

import Corvus.Handlers.Disk.Db (getDiskImageInfo, listDiskImages)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Database.Persist.Sql (runSqlPool)
import System.FilePath ((</>))

import Corvus.Handlers.Disk.Placement (nodeBasePathFor)

-- | List all disk images. The 'diiFilePath' field is rewritten to an
-- absolute path so clients (Makefile, integration scripts) can use it
-- directly without knowing the daemon's disk base.
handleDiskList :: ServerState -> IO Response
handleDiskList state = do
  disks <- runSqlPool listDiskImages (ssDbPool state)
  RespDiskList <$> mapM (absolutizeDiskFilePath state) disks

-- | Show disk image details. As with 'handleDiskList', 'diiFilePath'
-- in the response is absolute.
handleDiskShow :: ServerState -> Int64 -> IO Response
handleDiskShow state diskId = do
  mInfo <- runSqlPool (getDiskImageInfo diskId) (ssDbPool state)
  case mInfo of
    Nothing -> pure RespDiskNotFound
    Just info -> RespDiskInfo <$> absolutizeDiskFilePath state info

-- | Promote each placement's 'dipFilePath' to an absolute path.
-- The DB stores paths relative to the daemon's base; clients
-- need the absolute form.
absolutizeDiskFilePath :: ServerState -> DiskImageInfo -> IO DiskImageInfo
absolutizeDiskFilePath state info = do
  placements <- mapM absolutizePlacement (diiPlacements info)
  pure info {diiPlacements = placements}
  where
    absolutizePlacement p = do
      basePath <- nodeBasePathFor state (toSqlKey (nrId (dipNode p)) :: M.NodeId)
      let raw = T.unpack (dipFilePath p)
          absPath = if "/" `isPrefixOf` raw then raw else basePath </> raw
      pure p {dipFilePath = T.pack absPath}
