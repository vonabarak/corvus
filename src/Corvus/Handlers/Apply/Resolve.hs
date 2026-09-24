{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Corvus.Handlers.Apply.Resolve (resolveByName, resolveByNameFilter) where

import Corvus.Handlers.Resolve (resolveNode)
import Corvus.Model (NodeId)
import Corvus.Protocol (Ref (..))
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlBackend, fromSqlKey, runSqlPool, toSqlKey)

resolveByName :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record) => ServerState -> (Text -> Unique record) -> Map.Map Text Int64 -> Text -> IO (Maybe Int64)
resolveByName state mkUnique localMap name = case Map.lookup name localMap of
  Just rid -> pure $ Just rid
  Nothing -> fmap (fmap (fromSqlKey . entityKey)) $ runSqlPool (getBy $ mkUnique name) (ssDbPool state)

resolveByNameFilter :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record) => ServerState -> (Text -> [Filter record]) -> (NodeId -> [Filter record]) -> Map.Map Text Int64 -> Text -> Text -> IO (Maybe Int64)
resolveByNameFilter state mkFilter mkNodeFilter localMap name nodeRef = case Map.lookup name localMap of
  Just rid -> pure $ Just rid
  Nothing -> do
    nodeFilter <-
      if T.null nodeRef
        then pure []
        else do
          mNid <- resolveNode (Ref nodeRef) (ssDbPool state)
          pure $ either (const []) (mkNodeFilter . toSqlKey) mNid
    entities <- runSqlPool (selectList (mkFilter name ++ nodeFilter) []) (ssDbPool state)
    pure $ case entities of [e] -> Just (fromSqlKey $ entityKey e); _ -> Nothing
