{-# LANGUAGE OverloadedStrings #-}

module Corvus.DatabaseSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..), LoggingT, runLoggingT)
import Corvus.Database
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Pool (Pool, destroyAllResources)
import qualified Data.Text as T
import Database.Persist (PersistValue (..))
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, runSqlPool)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Test.Database as TestDb
import Test.Hspec

spec :: Spec
spec = do
  describe "parseDatabase" $ do
    it "selects PostgreSQL for PostgreSQL URLs" $ do
      parsed <- parseDatabase (Just "postgresql://localhost/corvus")
      parsed
        `shouldBe` Right
          (DatabaseConfig DatabasePostgresql "postgresql://localhost/corvus")

    it "selects SQLite for file paths" $ do
      parsed <- parseDatabase (Just "/var/lib/corvus/corvus.db")
      parsed
        `shouldBe` Right
          (DatabaseConfig DatabaseSqlite "/var/lib/corvus/corvus.db")

    it "rejects unknown URI schemes" $ do
      parsed <- parseDatabase (Just "mysql://localhost/corvus")
      case parsed of
        Left msg -> msg `shouldSatisfy` T.isInfixOf "unsupported database URI scheme"
        Right cfg -> expectationFailure $ "expected parse failure, got " <> show cfg

    it "defaults to SQLite under XDG_DATA_HOME when SQLite is compiled" $
      withSystemTempDirectory "corvus-db-default" $ \dir ->
        withEnv "XDG_DATA_HOME" dir $ do
          parsed <- parseDatabase Nothing
          parsed
            `shouldBe` Right
              (DatabaseConfig DatabaseSqlite (dir </> "corvus" </> "corvus.db"))

  describe "sqliteVersionNumberFromText" $ do
    it "converts SQLite version text to SQLITE_VERSION_NUMBER" $ do
      sqliteVersionNumberFromText "3.45.1" `shouldBe` Just 3045001
      sqliteVersionNumberFromText "3.50.2" `shouldBe` Just 3050002

    it "rejects malformed version text" $ do
      sqliteVersionNumberFromText "3.45" `shouldBe` Nothing
      sqliteVersionNumberFromText "3.45.1.0" `shouldBe` Nothing
      sqliteVersionNumberFromText "3.45.x" `shouldBe` Nothing
      sqliteVersionNumberFromText "3..1" `shouldBe` Nothing

  describe "decideSchemaMigration" $ do
    it "skips migration when the stored version matches" $
      decideSchemaMigration currentSchemaVersion currentSchemaVersion
        `shouldBe` SchemaMigrationNotNeeded

    it "runs migration when the stored version is older" $
      decideSchemaMigration (currentSchemaVersion - 1) currentSchemaVersion
        `shouldBe` SchemaMigrationNeeded

    it "refuses startup when the stored version is newer" $
      decideSchemaMigration (currentSchemaVersion + 1) currentSchemaVersion
        `shouldBe` SchemaMigrationRefusedNewer

  describe "runDatabaseMigrations with SQLite" $ do
    it "treats a missing schema version as first boot and records the current version" $
      withSystemTempDirectory "corvus-db-schema-first" $ \dir -> do
        let cfg = DatabaseConfig DatabaseSqlite (dir </> "corvus.db")
        pool <- createDatabasePool cfg
        result <- runDatabaseMigrations cfg pool
        storedVersion <- runSqlPool readSchemaVersion pool
        nodeTableExists <- sqliteTableExists pool "node"
        destroyAllResources pool

        result `shouldBe` Right (SchemaMigrated 0 currentSchemaVersion)
        storedVersion `shouldBe` currentSchemaVersion
        nodeTableExists `shouldBe` True

    it "skips Persistent migrations when the stored version is already current" $
      withSystemTempDirectory "corvus-db-schema-skip" $ \dir -> do
        let cfg = DatabaseConfig DatabaseSqlite (dir </> "corvus.db")
        pool <- createDatabasePool cfg
        installSchemaVersionOnly pool currentSchemaVersion

        result <- runDatabaseMigrations cfg pool
        nodeTableExists <- sqliteTableExists pool "node"
        destroyAllResources pool

        result `shouldBe` Right (SchemaAlreadyCurrent currentSchemaVersion)
        nodeTableExists `shouldBe` False

    it "upgrades version 1 databases with the TPM columns" $
      withSystemTempDirectory "corvus-db-schema-tpm" $ \dir -> do
        let cfg = DatabaseConfig DatabaseSqlite (dir </> "corvus.db")
        pool <- createDatabasePool cfg
        _ <- runDatabaseMigrations cfg pool
        runSqlPool
          ( do
              rawExecute "ALTER TABLE \"vm\" DROP COLUMN \"tpm\"" []
              rawExecute "ALTER TABLE \"template_vm\" DROP COLUMN \"tpm\"" []
              writeSchemaVersion 1
          )
          pool

        result <- runDatabaseMigrations cfg pool
        vmHasTpm <- sqliteColumnExists pool "vm" "tpm"
        templateHasTpm <- sqliteColumnExists pool "template_vm" "tpm"
        storedVersion <- runSqlPool readSchemaVersion pool
        destroyAllResources pool

        result `shouldBe` Right (SchemaMigrated 1 currentSchemaVersion)
        vmHasTpm `shouldBe` True
        templateHasTpm `shouldBe` True
        storedVersion `shouldBe` currentSchemaVersion

    it "rejects a database created by a newer binary" $
      withSystemTempDirectory "corvus-db-schema-newer" $ \dir -> do
        let cfg = DatabaseConfig DatabaseSqlite (dir </> "corvus.db")
        pool <- createDatabasePool cfg
        installSchemaVersionOnly pool (currentSchemaVersion + 1)

        result <- runDatabaseMigrations cfg pool
        nodeTableExists <- sqliteTableExists pool "node"
        destroyAllResources pool

        result
          `shouldBe` Left
            SchemaVersionTooNew
              { sveStoredVersion = currentSchemaVersion + 1
              , sveCurrentVersion = currentSchemaVersion
              }
        nodeTableExists `shouldBe` False

  describe "readSqliteHeaderVersion" $ do
    it "skips missing, empty, short, and non-SQLite files" $
      withSystemTempDirectory "corvus-db-header" $ \dir -> do
        readSqliteHeaderVersion (dir </> "missing.db") `shouldReturn` Nothing

        let emptyPath = dir </> "empty.db"
        BS.writeFile emptyPath ""
        readSqliteHeaderVersion emptyPath `shouldReturn` Nothing

        let shortPath = dir </> "short.db"
        BS.writeFile shortPath sqliteHeaderMagic
        readSqliteHeaderVersion shortPath `shouldReturn` Nothing

        let nonSqlitePath = dir </> "not-sqlite.db"
        BS.writeFile nonSqlitePath $ BS.replicate 100 0
        readSqliteHeaderVersion nonSqlitePath `shouldReturn` Nothing

    it "reads the SQLite last-writer version from header offset 96" $
      withSystemTempDirectory "corvus-db-header" $ \dir -> do
        let path = dir </> "corvus.db"
        BS.writeFile path $ sqliteHeaderWithVersion 3045001
        readSqliteHeaderVersion path `shouldReturn` Just 3045001

  describe "getDatabaseRuntimeInfo" $ do
    it "returns SQLite backend and runtime version" $
      withSystemTempDirectory "corvus-db-runtime" $ \dir -> do
        let cfg = DatabaseConfig DatabaseSqlite (dir </> "corvus.db")
        pool <- createDatabasePool cfg
        migrationResult <- runDatabaseMigrations cfg pool
        migrationResult `shouldBe` Right (SchemaMigrated 0 currentSchemaVersion)
        info <- getDatabaseRuntimeInfo cfg pool
        sqliteMasterRows <-
          runSqlPool
            (rawSql "SELECT sql FROM sqlite_master WHERE type='table' AND name='node';" [])
            pool
        destroyAllResources pool
        driBackend info `shouldBe` "sqlite"
        driVersion info `shouldSatisfy` (not . T.null)
        case sqliteMasterRows of
          Single sql : _ -> sql `shouldSatisfy` T.isInfixOf "AUTOINCREMENT"
          [] -> expectationFailure "expected node table definition"

  describe "warnIfSqliteHeaderVersionMismatch" $ do
    it "warns when the SQLite header version differs from the runtime SQLite version" $
      withSystemTempDirectory "corvus-db-header-warning" $ \dir -> do
        let path = dir </> "corvus.db"
        BS.writeFile path $ sqliteHeaderWithVersion 1
        warningCount <- captureWarningCount $ warnIfSqliteHeaderVersionMismatch (DatabaseConfig DatabaseSqlite path)
        warningCount `shouldBe` 1

  describe "runDatabaseMigrations with the configured test backend" $ do
    it "skips when the stored version is current" $
      withIsolatedTestDb $ \env -> do
        result <- runDatabaseMigrations (envDatabaseConfig env) (TestDb.tePool env)
        result `shouldBe` Right (SchemaAlreadyCurrent currentSchemaVersion)

    it "upgrades when the stored version is older" $
      withIsolatedTestDb $ \env -> do
        runSqlPool (writeSchemaVersion (currentSchemaVersion - 1)) (TestDb.tePool env)
        result <- runDatabaseMigrations (envDatabaseConfig env) (TestDb.tePool env)
        storedVersion <- runSqlPool readSchemaVersion (TestDb.tePool env)

        result `shouldBe` Right (SchemaMigrated (currentSchemaVersion - 1) currentSchemaVersion)
        storedVersion `shouldBe` currentSchemaVersion

    it "fails when the stored version is newer" $
      withIsolatedTestDb $ \env -> do
        runSqlPool (writeSchemaVersion (currentSchemaVersion + 1)) (TestDb.tePool env)
        result <- runDatabaseMigrations (envDatabaseConfig env) (TestDb.tePool env)

        result
          `shouldBe` Left
            SchemaVersionTooNew
              { sveStoredVersion = currentSchemaVersion + 1
              , sveCurrentVersion = currentSchemaVersion
              }

withEnv :: String -> String -> IO a -> IO a
withEnv name value =
  bracket set restore . const
  where
    set = do
      old <- lookupEnv name
      setEnv name value
      pure old
    restore Nothing = unsetEnv name
    restore (Just oldValue) = setEnv name oldValue

captureWarningCount :: LoggingT IO () -> IO Int
captureWarningCount action = do
  ref <- newIORef 0
  runLoggingT action $ \_ _ level _ ->
    when (level == LevelWarn) $ modifyIORef' ref (+ 1)
  readIORef ref

sqliteHeaderWithVersion :: Int -> BS.ByteString
sqliteHeaderWithVersion version =
  sqliteHeaderMagic <> BS.replicate 80 0 <> word32be version

sqliteHeaderMagic :: BS.ByteString
sqliteHeaderMagic =
  BS.pack [83, 81, 76, 105, 116, 101, 32, 102, 111, 114, 109, 97, 116, 32, 51, 0]

word32be :: Int -> BS.ByteString
word32be value =
  BS.pack
    [ fromIntegral $ (value `shiftR` 24) .&. 0xff
    , fromIntegral $ (value `shiftR` 16) .&. 0xff
    , fromIntegral $ (value `shiftR` 8) .&. 0xff
    , fromIntegral $ value .&. 0xff
    ]

installSchemaVersionOnly :: Pool SqlBackend -> Int -> IO ()
installSchemaVersionOnly pool version =
  runSqlPool
    ( do
        rawExecute
          "CREATE TABLE IF NOT EXISTS schema_version (id INTEGER PRIMARY KEY, version INTEGER NOT NULL)"
          []
        writeSchemaVersion version
    )
    pool

sqliteTableExists :: Pool SqlBackend -> T.Text -> IO Bool
sqliteTableExists pool tableName = do
  rows <-
    runSqlPool
      ( rawSql
          "SELECT name FROM sqlite_master WHERE type='table' AND name = ?;"
          [PersistText tableName]
      )
      pool
  pure $ not (null (rows :: [Single T.Text]))

sqliteColumnExists :: Pool SqlBackend -> T.Text -> T.Text -> IO Bool
sqliteColumnExists pool tableName columnName = do
  rows <-
    runSqlPool
      ( rawSql
          "SELECT name FROM pragma_table_info(?) WHERE name = ?"
          [PersistText tableName, PersistText columnName]
      )
      pool
  pure $ not (null (rows :: [Single T.Text]))

envDatabaseConfig :: TestDb.TestEnv -> DatabaseConfig
envDatabaseConfig env =
  case TestDb.teDatabaseEngine env of
    DatabaseSqlite -> DatabaseConfig DatabaseSqlite (TestDb.teTempDir env </> T.unpack (TestDb.teDbName env) <> ".db")
    DatabasePostgresql -> DatabaseConfig DatabasePostgresql (T.unpack $ testDbConnString (TestDb.teConfig env) (TestDb.teDbName env))

withIsolatedTestDb :: (TestDb.TestEnv -> IO a) -> IO a
withIsolatedTestDb =
  bracket TestDb.setupTestDb TestDb.teardownTestDb

testDbConnString :: TestDb.TestDbConfig -> T.Text -> T.Text
testDbConnString config dbName =
  "host="
    <> TestDb.tdcHost config
    <> " port="
    <> T.pack (show (TestDb.tdcPort config))
    <> " user="
    <> TestDb.tdcUser config
    <> " password="
    <> TestDb.tdcPassword config
    <> " dbname="
    <> dbName
