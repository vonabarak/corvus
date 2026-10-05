{-# LANGUAGE OverloadedStrings #-}

module Corvus.SizeYamlSpec (spec) where

import Control.Monad (forM_)
import Corvus.Schema.Apply (ApplyConfig)
import Corvus.Schema.Build (PipelineConfig)
import Corvus.Schema.Template (TemplateYaml)
import Data.Aeson (Result (..), Value (..), fromJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Yaml as Yaml
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec

spec :: Spec
spec = describe "repository YAML sizes" $
  it "parses every Corvus YAML example with the actual input schemas" $ do
    files <- yamlFiles "yaml"
    files `shouldSatisfy` (not . null)
    forM_ files $ \path -> do
      decoded <- Yaml.decodeFileEither path
      case decoded of
        Left err -> expectationFailure (path ++ ": " ++ show err)
        Right value@(Object o)
          | KM.member "pipeline" o -> check path (fromJSON value :: Result PipelineConfig)
          | KM.member "cpuCount" o -> check path (fromJSON value :: Result TemplateYaml)
          | any (`KM.member` o) ["vms", "templates", "disks", "sshKeys", "networks"] -> check path (fromJSON value :: Result ApplyConfig)
        Right _ -> pure () -- Guest service configurations have their own schemas.
  where
    check path (Error err) = expectationFailure (path ++ ": " ++ err)
    check _ (Success _) = pure ()

yamlFiles :: FilePath -> IO [FilePath]
yamlFiles directory = do
  names <- listDirectory directory
  concat <$> mapM visit names
  where
    visit name = do
      let path = directory </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then yamlFiles path
        else pure [path | takeExtension path `elem` [".yml", ".yaml"]]
