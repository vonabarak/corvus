{-# LANGUAGE OverloadedStrings #-}

module Corvus.DatabaseSpec (spec) where

import Control.Exception (bracket)
import Corvus.Database
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
