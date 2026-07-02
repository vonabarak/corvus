{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.Database
  ( DatabaseEngine (..)
  , DatabaseConfig (..)
  , DatabaseRuntimeInfo (..)
  , defaultDatabase
  , parseDatabase
  , createDatabasePool
  , runDatabaseMigrations
  , databaseEngineLabel
  , databaseEngineId
  , getDatabaseRuntimeInfo
  , unknownDatabaseRuntimeInfo
  , readSqliteHeaderVersion
  , sqliteVersionNumberFromText
  , warnIfSqliteHeaderVersionMismatch
  )
where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logWarnN, runStdoutLoggingT)
import Corvus.Model (migrateAll)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (fromRight)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue (..))
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, runMigration, runSqlPool)
#if defined(WITH_POSTGRESQL)
import Database.Persist.Postgresql (createPostgresqlPool)
#endif
#if defined(WITH_SQLITE)
import qualified Database.Sqlite as Sqlite
import Database.Persist.Sqlite (createSqlitePool)
#endif
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
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

data DatabaseRuntimeInfo = DatabaseRuntimeInfo
  { driBackend :: !Text
  , driVersion :: !Text
  }
  deriving (Eq, Show)

databaseEngineLabel :: DatabaseEngine -> Text
databaseEngineLabel DatabasePostgresql = "PostgreSQL"
databaseEngineLabel DatabaseSqlite = "SQLite"

databaseEngineId :: DatabaseEngine -> Text
databaseEngineId DatabasePostgresql = "postgresql"
databaseEngineId DatabaseSqlite = "sqlite"

unknownDatabaseRuntimeInfo :: DatabaseRuntimeInfo
unknownDatabaseRuntimeInfo =
  DatabaseRuntimeInfo
    { driBackend = "unknown"
    , driVersion = "unknown"
    }

sqliteHeaderSize :: Int
sqliteHeaderSize = 100

sqliteHeaderVersionOffset :: Int
sqliteHeaderVersionOffset = 96

sqliteHeaderMagic :: BS.ByteString
sqliteHeaderMagic =
  BS.pack [83, 81, 76, 105, 116, 101, 32, 102, 111, 114, 109, 97, 116, 32, 51, 0]

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

getDatabaseRuntimeInfo :: DatabaseConfig -> Pool SqlBackend -> IO DatabaseRuntimeInfo
getDatabaseRuntimeInfo cfg pool = do
  version <- getDatabaseRuntimeVersion cfg pool
  pure
    DatabaseRuntimeInfo
      { driBackend = databaseEngineId (dcEngine cfg)
      , driVersion = fromRight "unknown" version
      }

getDatabaseRuntimeVersion :: DatabaseConfig -> Pool SqlBackend -> IO (Either SomeException Text)
getDatabaseRuntimeVersion cfg pool =
  try $
    case dcEngine cfg of
      DatabasePostgresql -> querySingleText "SHOW server_version"
      DatabaseSqlite -> querySingleText "SELECT sqlite_version()"
  where
    querySingleText sql = do
      rows <- runSqlPool (rawSql sql []) pool
      case rows of
        Single version : _ -> pure version
        [] -> pure "unknown"

readSqliteHeaderVersion :: FilePath -> IO (Maybe Int)
readSqliteHeaderVersion ":memory:" = pure Nothing
readSqliteHeaderVersion path = do
  result <- try readHeader :: IO (Either SomeException (Maybe Int))
  pure $ fromRight Nothing result
  where
    readHeader = do
      exists <- doesFileExist path
      if not exists
        then pure Nothing
        else withBinaryFile path ReadMode $ \handle -> do
          header <- BS.hGet handle sqliteHeaderSize
          pure $
            if BS.length header < sqliteHeaderSize || BS.take 16 header /= sqliteHeaderMagic
              then Nothing
              else Just $ readBigEndian32 $ BS.take 4 $ BS.drop sqliteHeaderVersionOffset header

readBigEndian32 :: BS.ByteString -> Int
readBigEndian32 =
  BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

sqliteVersionNumberFromText :: Text -> Maybe Int
sqliteVersionNumberFromText value =
  case T.splitOn "." value of
    [majorText, minorText, patchText] ->
      sqliteVersionNumber
        <$> parseComponent majorText
        <*> parseComponent minorText
        <*> parseComponent patchText
    _ -> Nothing
  where
    parseComponent component
      | T.null component = Nothing
      | T.any (not . isDigit) component = Nothing
      | otherwise = Just $ T.foldl' (\acc c -> acc * 10 + fromEnum c - fromEnum '0') 0 component
    sqliteVersionNumber major minor patch = major * 1000000 + minor * 1000 + patch

warnIfSqliteHeaderVersionMismatch :: DatabaseConfig -> LoggingT IO ()
warnIfSqliteHeaderVersionMismatch cfg =
  case dcEngine cfg of
    DatabasePostgresql -> pure ()
    DatabaseSqlite -> warnIfSqliteHeaderVersionMismatch' (dcValue cfg)

warnIfSqliteHeaderVersionMismatch' :: FilePath -> LoggingT IO ()
#if defined(WITH_SQLITE)
warnIfSqliteHeaderVersionMismatch' path
  | path == ":memory:" = pure ()
  | otherwise = do
      headerVersion <- liftIO $ readSqliteHeaderVersion path
      runtimeVersion <- liftIO currentSqliteRuntimeVersionNumber
      case (headerVersion, runtimeVersion) of
        (Just fileVersion, Just currentVersion) ->
          when (fileVersion /= currentVersion) $
            logWarnN $
              "SQLite database "
                <> T.pack path
                <> " was last modified by SQLite "
                <> sqliteVersionNumberToText fileVersion
                <> " ("
                <> T.pack (show fileVersion)
                <> "), but this binary uses SQLite "
                <> sqliteVersionNumberToText currentVersion
                <> " ("
                <> T.pack (show currentVersion)
                <> "); continuing startup"
        _ -> pure ()
#else
warnIfSqliteHeaderVersionMismatch' _ = pure ()
#endif

currentSqliteRuntimeVersionNumber :: IO (Maybe Int)
#if defined(WITH_SQLITE)
currentSqliteRuntimeVersionNumber = do
  result <- try queryVersion :: IO (Either SomeException (Maybe Int))
  pure $ fromRight Nothing result
  where
    queryVersion =
      bracket (Sqlite.open ":memory:") Sqlite.close $ \conn ->
        bracket (Sqlite.prepare conn "SELECT sqlite_version()") Sqlite.finalize $ \stmt -> do
          stepResult <- Sqlite.step stmt
          case stepResult of
            Sqlite.Row -> do
              versionValue <- Sqlite.column stmt 0
              case versionValue of
                PersistText versionText -> pure $ sqliteVersionNumberFromText versionText
                _ -> pure Nothing
            Sqlite.Done -> pure Nothing
#else
currentSqliteRuntimeVersionNumber = pure Nothing
#endif

sqliteVersionNumberToText :: Int -> Text
sqliteVersionNumberToText versionNumber =
  T.pack $
    show (versionNumber `div` 1000000)
      <> "."
      <> show ((versionNumber `div` 1000) `mod` 1000)
      <> "."
      <> show (versionNumber `mod` 1000)

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
