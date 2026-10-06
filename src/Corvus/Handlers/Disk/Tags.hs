{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Disk.Tags (DiskTag (..), DiskUntag (..)) where

import Corvus.Action
import Corvus.DiskSelector (validateImageTag)
import Corvus.Images (assignImageTag, removeImageTag)
import Corvus.Model
import Corvus.Protocol
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist (get)
import Database.Persist.Sql (SqlPersistT, runSqlPool)

data DiskTag = DiskTag Int64 Text

instance Action DiskTag where
  actionSubsystem _ = SubDisk
  actionCommand _ = "tag"
  actionEntityId (DiskTag key _) = Just (fromIntegral key)
  actionExecute ctx (DiskTag key tag) = mutateTag (acState ctx) key tag (assignImageTag (toSqlKey key) tag)

data DiskUntag = DiskUntag Int64 Text

instance Action DiskUntag where
  actionSubsystem _ = SubDisk
  actionCommand _ = "untag"
  actionEntityId (DiskUntag key _) = Just (fromIntegral key)
  actionExecute ctx (DiskUntag key tag)
    | tag == "latest" = pure $ RespError "The latest tag cannot be removed; move it or delete its image"
    | otherwise = mutateTag (acState ctx) key tag (removeImageTag (toSqlKey key) tag)

mutateTag :: ServerState -> Int64 -> Text -> SqlPersistT IO () -> IO Response
mutateTag state key tag operation = case validateImageTag tag of
  Left err -> pure $ RespError err
  Right () ->
    runSqlPool
      ( do
          disk <- get (toSqlKey key :: DiskImageId)
          case disk of
            Nothing -> pure RespDiskNotFound
            Just _ -> operation >> pure RespDiskOk
      )
      (ssDbPool state)
