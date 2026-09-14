-- | Versioned, transactional upgrades. Historical migrations must not import
-- the current Persistent model: each step produces exactly its target schema.
module Corvus.Database.Migration
  ( DatabaseEngine (..)
  , Migration (..)
  , SchemaMigrationError (..)
  , planMigrations
  , executeMigrations
  , renderSchemaMigrationError
  ) where

import Control.Exception (Exception, SomeAsyncException, SomeException, fromException)
import Control.Monad (forM, forM_, unless)
import Control.Monad.Catch (catch, throwM)
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (SqlPersistT)

data DatabaseEngine = DatabasePostgresql | DatabaseSqlite
  deriving (Eq, Show)

data Migration = Migration
  { migrationVersion :: !Int
  , migrationDescription :: !Text
  , migrationExecute :: DatabaseEngine -> SqlPersistT IO ()
  }

data SchemaMigrationError
  = SchemaVersionTooNew {sveStoredVersion :: !Int, sveCurrentVersion :: !Int}
  | SchemaMigrationMissing {sveStoredVersion :: !Int, sveCurrentVersion :: !Int, sveMissingVersion :: !Int}
  | SchemaVersionInvalid !Text
  | SchemaMigrationRegistryInvalid !Text
  | SchemaMigrationFailed !Int !Text !Text
  deriving (Eq, Show)

instance Exception SchemaMigrationError

-- | Check the complete path before any step can change the database.
planMigrations :: Int -> [Migration] -> Int -> Either SchemaMigrationError [Migration]
planMigrations current migrations stored = do
  let versions = map migrationVersion migrations
  unless (current > 0 && all (\v -> v > 1 && v <= current) versions) $
    Left $
      SchemaMigrationRegistryInvalid "migration targets must be between 2 and the current version"
  unless (all ((== 1) . length) $ group $ sort versions) $
    Left $
      SchemaMigrationRegistryInvalid "duplicate migration targets"
  if stored <= 0
    then Left $ SchemaVersionInvalid "stored schema version must be positive"
    else
      if stored > current
        then Left $ SchemaVersionTooNew stored current
        else forM [stored + 1 .. current] $ \target ->
          case filter ((== target) . migrationVersion) migrations of
            [migration] -> Right migration
            _ -> Left $ SchemaMigrationMissing stored current target

-- | The caller owns the transaction and supplies the version writer. Failures
-- must escape that transaction before being converted to an Either result.
executeMigrations :: DatabaseEngine -> (Int -> SqlPersistT IO ()) -> [Migration] -> SqlPersistT IO ()
executeMigrations engine writeVersion migrations =
  forM_ migrations $ \migration ->
    (migrationExecute migration engine >> writeVersion (migrationVersion migration))
      `catch` reportFailure migration
  where
    reportFailure :: Migration -> SomeException -> SqlPersistT IO ()
    reportFailure migration err =
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwM err
        Nothing -> throwM $ SchemaMigrationFailed (migrationVersion migration) (migrationDescription migration) (T.pack $ show err)

renderSchemaMigrationError :: SchemaMigrationError -> Text
renderSchemaMigrationError err = case err of
  SchemaVersionTooNew stored current ->
    "Database schema version " <> number stored <> " is newer than this binary supports (" <> number current <> ")"
  SchemaMigrationMissing stored current target ->
    "Cannot upgrade database schema version "
      <> number stored
      <> " to "
      <> number current
      <> ": required migration "
      <> number (target - 1)
      <> " -> "
      <> number target
      <> " is unavailable; upgrade using a release that still includes this migration first"
  SchemaVersionInvalid reason -> "Invalid database schema version metadata: " <> reason
  SchemaMigrationRegistryInvalid reason -> "Invalid database migration registry: " <> reason
  SchemaMigrationFailed target description reason ->
    "Database migration "
      <> number (target - 1)
      <> " -> "
      <> number target
      <> " ("
      <> description
      <> ") failed: "
      <> reason
  where
    number = T.pack . show
