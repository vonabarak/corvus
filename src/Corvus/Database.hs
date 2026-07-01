{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.Database
  ( DatabaseEngine (..)
  , DatabaseConfig (..)
  , defaultDatabase
  , parseDatabase
  , createDatabasePool
  , runDatabaseMigrations
  , databaseEngineLabel
  )
where

import Control.Monad.Logger (runStdoutLoggingT)
import Corvus.Model (migrateAll)
import Data.ByteString.Char8 (pack)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (SqlBackend, rawExecute, runMigration, runSqlPool)
#if defined(WITH_POSTGRESQL)
import Database.Persist.Postgresql (createPostgresqlPool)
#endif
#if defined(WITH_SQLITE)
import Database.Persist.Sqlite (createSqlitePool)
#endif
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (userError)

data DatabaseEngine
  = DatabasePostgresql
  | DatabaseSqlite
  deriving (Eq, Show)

data DatabaseConfig = DatabaseConfig
  { dcEngine :: !DatabaseEngine
  , dcValue :: !String
  }
  deriving (Eq, Show)

databaseEngineLabel :: DatabaseEngine -> Text
databaseEngineLabel DatabasePostgresql = "PostgreSQL"
databaseEngineLabel DatabaseSqlite = "SQLite"

defaultDatabase :: IO DatabaseConfig
#if defined(WITH_SQLITE)
defaultDatabase = DatabaseConfig DatabaseSqlite <$> defaultSqlitePath
#elif defined(WITH_POSTGRESQL)
defaultDatabase = pure $ DatabaseConfig DatabasePostgresql "postgresql://localhost/corvus"
#else
defaultDatabase = fail "Corvus was built without database backend support"
#endif

parseDatabase :: Maybe String -> IO (Either Text DatabaseConfig)
parseDatabase Nothing = Right <$> defaultDatabase
parseDatabase (Just value)
  | isPostgresqlUrl value = pure $ Right $ DatabaseConfig DatabasePostgresql value
  | hasUriScheme value =
      pure $
        Left $
          "unsupported database URI scheme in "
            <> T.pack value
            <> "; use a PostgreSQL URL or a SQLite file path"
  | otherwise = pure $ Right $ DatabaseConfig DatabaseSqlite value

createDatabasePool :: DatabaseConfig -> IO (Pool SqlBackend)
createDatabasePool cfg =
  case dcEngine cfg of
    DatabasePostgresql -> createPostgresqlPoolChecked (dcValue cfg)
    DatabaseSqlite -> createSqlitePoolChecked (dcValue cfg)

createPostgresqlPoolChecked :: String -> IO (Pool SqlBackend)
#if defined(WITH_POSTGRESQL)
createPostgresqlPoolChecked value =
  runStdoutLoggingT $ createPostgresqlPool (pack value) 10
#else
createPostgresqlPoolChecked _ =
  ioError $ userError "this corvus binary was built without PostgreSQL support"
#endif

createSqlitePoolChecked :: String -> IO (Pool SqlBackend)
#if defined(WITH_SQLITE)
createSqlitePoolChecked value = do
  ensureSqliteParent value
  pool <- runStdoutLoggingT $ createSqlitePool (T.pack value) 1
  runSqlPool
    ( do
        rawExecute "PRAGMA foreign_keys = ON" []
        rawExecute "PRAGMA journal_mode = WAL" []
        rawExecute "PRAGMA busy_timeout = 5000" []
    )
    pool
  pure pool
#else
createSqlitePoolChecked _ =
  ioError $ userError "this corvus binary was built without SQLite support"
#endif

runDatabaseMigrations :: Pool SqlBackend -> IO ()
runDatabaseMigrations = runSqlPool (runMigration migrateAll)

isPostgresqlUrl :: String -> Bool
isPostgresqlUrl value =
  "postgresql://" `T.isPrefixOf` lowered
    || "postgres://" `T.isPrefixOf` lowered
  where
    lowered = T.toLower (T.pack value)

hasUriScheme :: String -> Bool
hasUriScheme value =
  case break (== ':') value of
    ([], _) -> False
    (scheme, ':' : _) -> all isSchemeChar scheme && any isAsciiAlpha scheme
    _ -> False
  where
    isSchemeChar c = isAlphaNum c || c == '+' || c == '-' || c == '.'
    isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

defaultSqlitePath :: IO FilePath
defaultSqlitePath = do
  mDataHome <- lookupEnv "XDG_DATA_HOME"
  case mDataHome of
    Just dir | not (null dir) -> pure $ dir </> "corvus" </> "corvus.db"
    Nothing -> do
      home <- getHomeDirectory
      pure $ home </> ".local" </> "share" </> "corvus" </> "corvus.db"
    Just _ -> do
      home <- getHomeDirectory
      pure $ home </> ".local" </> "share" </> "corvus" </> "corvus.db"

ensureSqliteParent :: FilePath -> IO ()
ensureSqliteParent ":memory:" = pure ()
ensureSqliteParent path = createDirectoryIfMissing True (takeDirectory path)
