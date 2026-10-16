{-# LANGUAGE OverloadedStrings #-}

-- | Keep offline build dependencies on one host before allocating resources.
module Corvus.Handlers.Build.Placement (placeBuild) where

import Corvus.Handlers.Disk.Db (getBackingChainIds, hasPlacementOnNode)
import Corvus.Handlers.Resolve (resolveErrorMessage, resolveNode)
import Corvus.Handlers.Scheduler (pickNodeForExistingDisk)
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Build
import Corvus.Types
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (runSqlPool)

placeBuild :: ServerState -> Build -> IO (Either T.Text Build)
placeBuild state b
  | buildStrategy b /= BuildStrategyOfflineOverlay = pure (Right b)
  | otherwise = case buildResolvedSourceImage b of
      Nothing -> pure $ Left "build source image snapshot missing"
      Just source -> do
        selected <-
          if T.null (buildNode b)
            then pickNodeForExistingDisk state (toSqlKey source)
            else
              either (Left . resolveErrorMessage) (Right . toSqlKey)
                <$> resolveNode (Ref (buildNode b)) (ssDbPool state)
        case selected of
          Left err -> pure $ Left err
          Right nid ->
            runSqlPool
              ( do
                  node <- get nid
                  let dependencies = source : maybe [] (map nrId . mapMaybe tvdiDiskImage . tvdDrives) (buildResolvedTemplate b)
                  chains <- mapM getBackingChainIds dependencies
                  present <- mapM (`hasPlacementOnNode` nid) (map toSqlKey dependencies ++ concat chains)
                  pure $
                    if maybe False ((== NodeOnline) . nodeAdminState) node && and present
                      then Right b {buildNode = T.pack (show (fromSqlKey nid))}
                      else Left "offline-overlay requires source, helper images and backing chains on the selected online node"
              )
              (ssDbPool state)
