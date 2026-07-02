{-# LANGUAGE OverloadedStrings #-}

module Corvus.DatabaseSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..), LoggingT, runLoggingT)
import Corvus.Database
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Pool (destroyAllResources)
import qualified Data.Text as T
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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
        info <- getDatabaseRuntimeInfo cfg pool
        destroyAllResources pool
        driBackend info `shouldBe` "sqlite"
        driVersion info `shouldSatisfy` (not . T.null)

  describe "warnIfSqliteHeaderVersionMismatch" $ do
    it "warns when the SQLite header version differs from the runtime SQLite version" $
      withSystemTempDirectory "corvus-db-header-warning" $ \dir -> do
        let path = dir </> "corvus.db"
        BS.writeFile path $ sqliteHeaderWithVersion 1
        warningCount <- captureWarningCount $ warnIfSqliteHeaderVersionMismatch (DatabaseConfig DatabaseSqlite path)
        warningCount `shouldBe` 1

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
