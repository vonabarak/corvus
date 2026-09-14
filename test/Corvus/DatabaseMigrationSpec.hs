module Corvus.DatabaseMigrationSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Corvus.Database
import Corvus.Database.Migration
import Data.Either (isLeft)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sql (Single (..), SqlBackend, SqlPersistT, rawExecute, rawSql, runSqlPool)
import qualified Test.Database as TestDb
import Test.Hspec

spec :: Spec
spec = do
  describe "migration planning" $ do
    let step version = Migration version "test" (const $ pure ())
        targets current registry stored = map migrationVersion <$> planMigrations current registry stored
    it "orders only required steps, independently of registry ordering" $
      targets 5 [step 5, step 3, step 4] 3 `shouldBe` Right [4, 5]
    it "rejects a missing intermediate step" $
      targets 5 [step 3, step 5] 2 `shouldBe` Left (SchemaMigrationMissing 2 5 4)
    it "permits a retired prefix but refuses databases that need it" $ do
      targets 5 [step 5] 4 `shouldBe` Right [5]
      targets 5 [step 5] 3 `shouldBe` Left (SchemaMigrationMissing 3 5 4)
    it "supports an empty history only for current databases" $ do
      targets 3 [] 3 `shouldBe` Right []
      targets 3 [] 2 `shouldBe` Left (SchemaMigrationMissing 2 3 3)
    it "rejects duplicate, invalid and future targets" $
      forM_ [[step 3, step 3], [step 0], [step 4]] $ \registry ->
        targets 3 registry 3 `shouldSatisfy` isLeft
    it "rejects invalid stored versions and newer databases" $ do
      targets 3 [] 0 `shouldSatisfy` isLeft
      targets 3 [] 4 `shouldBe` Left (SchemaVersionTooNew 4 3)

  describe "database migration transactions with the configured backend" $ do
    it "upgrades the frozen version-2 schema and preserves constraints and data" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema (dcEngine cfg) >> loadVersion2 (dcEngine cfg)) pool
        case dcEngine cfg of
          DatabaseSqlite -> runSqlPool (rawExecute "UPDATE sqlite_sequence SET seq = 100 WHERE name = 'drive'" []) pool
          DatabasePostgresql -> pure ()
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaMigrated 2 3)
        runSqlPool readSchemaVersion pool `shouldReturn` Just 3
        rows <- runSqlPool (rawSql "SELECT id, disk_image_id FROM drive" []) pool
        rows `shouldBe` [(Single (1 :: Int), Single (1 :: Int))]
        let insertDrive :: T.Text -> SqlPersistT IO ()
            insertDrive image =
              rawExecute
                ("INSERT INTO drive (vm_id, disk_image_id, interface, media, cache_type) VALUES (1, " <> image <> ", 'ide', 'cdrom', 'none')")
                []
        runSqlPool (insertDrive "1") pool `shouldThrow` anyException
        runSqlPool (insertDrive "999") pool `shouldThrow` anyException
        runSqlPool (insertDrive "NULL" >> insertDrive "NULL") pool
        case dcEngine cfg of
          DatabaseSqlite -> do
            ids <- runSqlPool (rawSql "SELECT id FROM drive WHERE disk_image_id IS NULL ORDER BY id" []) pool
            ids `shouldBe` [Single (101 :: Int), Single 102]
          DatabasePostgresql -> pure ()
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaAlreadyCurrent 3)

    it "upgrades an empty historical drive table without reusing deleted IDs" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema (dcEngine cfg) >> loadVersion2 (dcEngine cfg) >> rawExecute "DELETE FROM drive" []) pool
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaMigrated 2 3)
        runSqlPool (rawExecute "INSERT INTO drive (vm_id, disk_image_id, interface, cache_type) VALUES (1, NULL, 'ide', 'none')" []) pool
        ids <- runSqlPool (rawSql "SELECT id FROM drive" []) pool
        ids `shouldBe` [Single (2 :: Int)]

    it "rolls back schema, data and version updates when a later step fails" $
      withDatabase $ \cfg pool -> do
        let first = Migration 4 "create marker" $ const $ do
              rawExecute "CREATE TABLE migration_marker (value INTEGER NOT NULL)" []
              rawExecute "INSERT INTO migration_marker VALUES (42)" []
              rawExecute "UPDATE node SET description = 'changed'" []
            second = Migration 5 "deliberate failure" $ const $ do
              version <- readSchemaVersion
              liftIO $ version `shouldBe` Just 4
              liftIO $ ioError $ userError "injected failure"
        result <- runDatabaseMigrationsWith 5 [second, first] cfg pool
        case result of
          Left (SchemaMigrationFailed 5 "deliberate failure" reason) -> reason `shouldSatisfy` T.isInfixOf "injected failure"
          _ -> expectationFailure $ show result
        runSqlPool readSchemaVersion pool `shouldReturn` Just 3
        runSqlPool (rawSql "SELECT value FROM migration_marker" [] :: SqlPersistT IO [Single Int]) pool `shouldThrow` anyException
        descriptions <- runSqlPool (rawSql "SELECT description FROM node" []) pool
        descriptions `shouldBe` [Single (Nothing :: Maybe T.Text)]

    it "checks the whole path before executing any step" $
      withDatabase $ \cfg pool -> do
        let first = Migration 4 "must not run" $ const $ liftIO $ expectationFailure "ran before validating path"
        runDatabaseMigrationsWith 5 [first] cfg pool `shouldReturn` Left (SchemaMigrationMissing 3 5 5)
        runSqlPool readSchemaVersion pool `shouldReturn` Just 3

    it "creates fresh schemas even when no historical migrations remain" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema $ dcEngine cfg) pool
        runDatabaseMigrationsWith 3 [] cfg pool `shouldReturn` Right (SchemaCreated 3)
        runSqlPool readSchemaVersion pool `shouldReturn` Just 3

    it "does not invoke migration actions when creating a fresh schema" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema $ dcEngine cfg) pool
        let step = Migration 3 "must not run" $ const $ liftIO $ expectationFailure "fresh creation invoked migration"
        runDatabaseMigrationsWith 3 [step] cfg pool `shouldReturn` Right (SchemaCreated 3)

    it "creates a fresh schema when only an empty version table exists" $
      withDatabase $ \cfg pool -> do
        runSqlPool
          (clearSchema (dcEngine cfg) >> rawExecute "CREATE TABLE schema_version (id INTEGER PRIMARY KEY, version INTEGER NOT NULL)" [])
          pool
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaCreated 3)

    it "refuses existing tables with absent metadata without creating metadata" $
      withDatabase $ \cfg pool -> do
        runSqlPool (rawExecute "DROP TABLE schema_version" []) pool
        result <- runDatabaseMigrations cfg pool
        result `shouldSatisfy` isLeft
        runSqlPool readSchemaVersion pool `shouldThrow` anyException

    it "rejects empty and malformed version records on populated databases" $
      withDatabase $ \cfg pool -> do
        forM_ ["DELETE FROM schema_version", "INSERT INTO schema_version VALUES (2, 3)"] $ \sql -> do
          runSqlPool (rawExecute sql []) pool
          result <- runDatabaseMigrations cfg pool
          result `shouldSatisfy` isLeft

withDatabase :: (DatabaseConfig -> Pool SqlBackend -> IO ()) -> IO ()
withDatabase action = bracket TestDb.setupTestDb TestDb.teardownTestDb $ \env ->
  -- Migration execution uses the supplied pool, not the connection string.
  action (DatabaseConfig (TestDb.teDatabaseEngine env) "") (TestDb.tePool env)

clearSchema :: DatabaseEngine -> SqlPersistT IO ()
clearSchema DatabasePostgresql = do
  rawExecute "DROP SCHEMA public CASCADE" []
  rawExecute "CREATE SCHEMA public" []
clearSchema DatabaseSqlite = do
  rawExecute "PRAGMA defer_foreign_keys = ON" []
  tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'" []
  forM_ tables $ \(Single name) -> rawExecute ("DROP TABLE \"" <> name <> "\"") []

loadVersion2 :: DatabaseEngine -> SqlPersistT IO ()
loadVersion2 engine = forM_ [databaseEngineId engine <> "-v2.sql", "seed.sql"] $ \filename -> do
  sql <- liftIO $ T.readFile $ "test/fixtures/database/" <> T.unpack filename
  forM_ (filter (not . T.null) $ map T.strip $ T.splitOn ";" sql) $ \statement -> rawExecute statement []
