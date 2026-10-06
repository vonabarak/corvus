module Corvus.DatabaseMigrationSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Corvus.Database
import Corvus.Database.Migration
import Corvus.Database.Migrations (migrations)
import Data.Either (isLeft)
import Data.Int (Int64)
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
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaMigrated 2 10)
        runSqlPool readSchemaVersion pool `shouldReturn` Just 10
        adapters <- runSqlPool (rawSql "SELECT graphics_adapter FROM vm" []) pool
        adapters `shouldBe` [Single ("virtio-vga" :: T.Text)]
        templateAdapters <- runSqlPool (rawSql "SELECT graphics_adapter FROM template_vm" []) pool
        templateAdapters `shouldBe` [Single ("virtio-vga" :: T.Text)]
        forM_ ["vm", "template_vm"] $ \table ->
          forM_ ["vsock", "balloon", "rng"] $ \column -> do
            values <- runSqlPool (rawSql ("SELECT " <> column <> " FROM " <> table) []) pool
            values `shouldBe` [Single True]
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
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaAlreadyCurrent 10)

    it "converts version-8 capacities exactly and preserves NULL and zero" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema (dcEngine cfg) >> loadVersion2 (dcEngine cfg)) pool
        runDatabaseMigrationsWith 8 (filter ((<= 8) . migrationVersion) migrations) cfg pool `shouldReturn` Right (SchemaMigrated 2 8)
        runSqlPool (rawExecute "UPDATE node SET ram_mb_total = 8192, ram_mb_free = NULL" [] >> rawExecute "UPDATE disk_image SET size_mb = 0" []) pool
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaMigrated 8 10)
        ram <- runSqlPool (rawSql "SELECT ram FROM vm" [] :: SqlPersistT IO [Single Int64]) pool
        ram `shouldBe` [Single 134217728]
        node <- runSqlPool (rawSql "SELECT ram_total, ram_free FROM node" [] :: SqlPersistT IO [(Single Int64, Single (Maybe Int64))]) pool
        node `shouldBe` [(Single 8589934592, Single Nothing)]
        disk <- runSqlPool (rawSql "SELECT size FROM disk_image" [] :: SqlPersistT IO [Single Int64]) pool
        disk `shouldBe` [Single 0]

    it "upgrades version-9 images and floating selectors without losing dependencies" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema (dcEngine cfg) >> loadVersion2 (dcEngine cfg)) pool
        runDatabaseMigrationsWith 9 (filter ((<= 9) . migrationVersion) migrations) cfg pool `shouldReturn` Right (SchemaMigrated 2 9)
        runSqlPool (rawExecute "INSERT INTO template_drive (template_id, disk_image_id, disk_name, interface, cache_type, clone_strategy) VALUES (1, 1, 'migration-disk', 'virtio', 'none', 'overlay')" []) pool
        case dcEngine cfg of
          DatabaseSqlite -> runSqlPool (rawExecute "UPDATE sqlite_sequence SET seq = 100 WHERE name = 'disk_image'" []) pool
          DatabasePostgresql -> pure ()
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaMigrated 9 10)
        tags <- runSqlPool (rawSql "SELECT name, tag, disk_image_id FROM disk_image_tag" [] :: SqlPersistT IO [(Single T.Text, Single T.Text, Single Int)]) pool
        tags `shouldBe` [(Single "migration-disk", Single "latest", Single 1)]
        selectors <- runSqlPool (rawSql "SELECT disk_image_id, disk_name, disk_tag FROM template_drive" [] :: SqlPersistT IO [(Single (Maybe Int), Single T.Text, Single T.Text)]) pool
        selectors `shouldBe` [(Single Nothing, Single "migration-disk", Single "latest")]
        placements <- runSqlPool (rawSql "SELECT disk_image_id, node_id, file_path FROM disk_image_node" [] :: SqlPersistT IO [(Single Int, Single Int, Single T.Text)]) pool
        placements `shouldBe` [(Single 1, Single 1, Single "/tmp/migration-disk.raw")]
        runSqlPool (rawExecute "INSERT INTO disk_image (name, format, size, created_at, backing_image_id, ephemeral) VALUES ('migration-disk', 'raw', 1048576, '2026-09-02 00:00:00', 1, false)" []) pool
        case dcEngine cfg of
          DatabaseSqlite -> do
            ids <- runSqlPool (rawSql "SELECT id FROM disk_image WHERE backing_image_id = 1" [] :: SqlPersistT IO [Single Int]) pool
            ids `shouldBe` [Single 101]
          DatabasePostgresql -> pure ()
        runSqlPool (rawExecute "INSERT INTO disk_image_tag (name, tag, disk_image_id) VALUES ('migration-disk', 'latest', 1)" []) pool `shouldThrow` anyException

    it "rolls back every converted column when a capacity overflows" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema (dcEngine cfg) >> loadVersion2 (dcEngine cfg)) pool
        runDatabaseMigrationsWith 8 (filter ((<= 8) . migrationVersion) migrations) cfg pool `shouldReturn` Right (SchemaMigrated 2 8)
        runSqlPool (rawExecute "UPDATE disk_image SET size_mb = 8796093022208" []) pool
        result <- runDatabaseMigrations cfg pool
        result `shouldSatisfy` isLeft
        runSqlPool readSchemaVersion pool `shouldReturn` Just 8
        values <- runSqlPool (rawSql "SELECT ram_mb FROM vm" [] :: SqlPersistT IO [Single Int64]) pool
        values `shouldBe` [Single 128]
        runSqlPool (rawSql "SELECT ram FROM vm" [] :: SqlPersistT IO [Single Int64]) pool `shouldThrow` anyException

    it "upgrades an empty historical drive table without reusing deleted IDs" $
      withDatabase $ \cfg pool -> do
        runSqlPool (clearSchema (dcEngine cfg) >> loadVersion2 (dcEngine cfg) >> rawExecute "DELETE FROM drive" []) pool
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaMigrated 2 10)
        runSqlPool (rawExecute "INSERT INTO drive (vm_id, disk_image_id, interface, cache_type) VALUES (1, NULL, 'ide', 'none')" []) pool
        ids <- runSqlPool (rawSql "SELECT id FROM drive" []) pool
        ids `shouldBe` [Single (2 :: Int)]

    it "rolls back schema, data and version updates when a later step fails" $
      withDatabase $ \cfg pool -> do
        let first = Migration 11 "create marker" $ const $ do
              rawExecute "CREATE TABLE migration_marker (value INTEGER NOT NULL)" []
              rawExecute "INSERT INTO migration_marker VALUES (42)" []
              rawExecute "UPDATE node SET description = 'changed'" []
            second = Migration 12 "deliberate failure" $ const $ do
              version <- readSchemaVersion
              liftIO $ version `shouldBe` Just 11
              liftIO $ ioError $ userError "injected failure"
        result <- runDatabaseMigrationsWith 12 [second, first] cfg pool
        case result of
          Left (SchemaMigrationFailed 12 "deliberate failure" reason) -> reason `shouldSatisfy` T.isInfixOf "injected failure"
          _ -> expectationFailure $ show result
        runSqlPool readSchemaVersion pool `shouldReturn` Just 10
        runSqlPool (rawSql "SELECT value FROM migration_marker" [] :: SqlPersistT IO [Single Int]) pool `shouldThrow` anyException
        descriptions <- runSqlPool (rawSql "SELECT description FROM node" []) pool
        descriptions `shouldBe` [Single (Nothing :: Maybe T.Text)]

    it "checks the whole path before executing any step" $
      withDatabase $ \cfg pool -> do
        let first = Migration 11 "must not run" $ const $ liftIO $ expectationFailure "ran before validating path"
        runDatabaseMigrationsWith 12 [first] cfg pool `shouldReturn` Left (SchemaMigrationMissing 10 12 12)
        runSqlPool readSchemaVersion pool `shouldReturn` Just 10

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
        runDatabaseMigrations cfg pool `shouldReturn` Right (SchemaCreated 10)

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
