{-# LANGUAGE OverloadedStrings #-}

module Corvus.BuildPreprocessSpec (spec) where

import Corvus.Client.BuildVars (applyBuildVars)
import Corvus.Client.Commands.Build (expandDirectoryProvisioners, preprocessRoot)
import Corvus.Schema.Build (PipelineConfig)
import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory (createDirectory, createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, createSymbolicLink)
import Test.Hspec

pipeline :: [Value] -> Value
pipeline steps = object ["pipeline" .= [object ["build" .= object ["name" .= ("example" :: T.Text), "template" .= ("base" :: T.Text), "target" .= object [], "provisioners" .= steps]]]]

directory :: FilePath -> [Value]
directory source = [object ["directory" .= object ["from" .= source, "to" .= ("/opt/app" :: T.Text)]]]

file :: T.Text -> T.Text -> Value
file target content = object ["file" .= object ["to" .= target, "content" .= content]]

spec :: Spec
spec = describe "build directory preprocessing" $ do
  it "expands directories for validation without requiring generated file assets" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      createDirectory (base </> "assets")
      BS.writeFile (base </> "assets" </> "config") "ok"
      let generated = object ["file" .= object ["from" .= ("missing.pyz" :: T.Text), "to" .= ("/run/build.pyz" :: T.Text)]]
          script = object ["shell" .= object ["script" .= ("missing.sh" :: T.Text)]]
          input = pipeline ([generated] <> directory "assets" <> [script])
          expected = pipeline [generated, file "/opt/app/config" "b2s=", script]
      expandDirectoryProvisioners base input `shouldReturn` expected
      case fromJSON expected :: Result PipelineConfig of
        Success _ -> pure ()
        Error err -> expectationFailure err

  it "expands hidden, nested, binary and empty files in stable depth-first order" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      let root = base </> "files"
          beforeStep = object ["shell" .= ("echo before" :: T.Text)]
          afterStep = object ["shell" .= ("echo after" :: T.Text)]
          input = pipeline ([beforeStep] <> directory "files" <> [afterStep])
      createDirectoryIfMissing True (root </> "config")
      createDirectory (root </> "empty")
      BS.writeFile (root </> "z") "z"
      BS.writeFile (root </> "config" </> "app.ini") "hi"
      BS.writeFile (root </> ".hidden") (BS.pack [0, 255])
      BS.writeFile (root </> "a") ""
      result <- preprocessRoot base input
      result `shouldBe` pipeline [beforeStep, file "/opt/app/.hidden" "AP8=", file "/opt/app/a" "", file "/opt/app/config/app.ini" "aGk=", file "/opt/app/z" "eg==", afterStep]
      preprocessRoot base input `shouldReturn` result
      case fromJSON result :: Result PipelineConfig of
        Success _ -> pure ()
        Error err -> expectationFailure err

  it "supports absolute sources and applies a mode uniformly" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      BS.writeFile (base </> "a") "a"
      BS.writeFile (base </> "b") "b"
      let declaration = object ["directory" .= object ["from" .= base, "to" .= ("/opt/app/" :: T.Text), "mode" .= ("u+x" :: T.Text)]]
          output name content = object ["file" .= object ["to" .= ("/opt/app/" <> name :: T.Text), "content" .= (content :: T.Text), "mode" .= ("u+x" :: T.Text)]]
      preprocessRoot "/unrelated" (pipeline [declaration]) `shouldReturn` pipeline [output "a" "YQ==", output "b" "Yg=="]

  it "expands variables before walking and treats a null mode as omitted" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      BS.writeFile (base </> "a") "a"
      let declaration = object ["directory" .= object ["from" .= ("{{ source }}" :: T.Text), "to" .= ("/opt/app" :: T.Text), "mode" .= Null]]
      -- Substitute through the same stage used by the CLI.
      substituted <- applyBuildVars [("source", T.pack base)] [] (addVars (pipeline [declaration]))
      case substituted of
        Left err -> expectationFailure (show err)
        Right value -> preprocessRoot base value `shouldReturn` pipeline [file "/opt/app/a" "YQ=="]

  it "omits empty directories" $
    withSystemTempDirectory "corvus-directory" $ \base ->
      preprocessRoot base (pipeline (directory base)) `shouldReturn` pipeline []

  mapM_
    invalidCase
    [ ("non-object", Null)
    , ("missing source", object ["to" .= ("/x" :: T.Text)])
    , ("empty source", object ["from" .= ("" :: T.Text), "to" .= ("/x" :: T.Text)])
    , ("missing destination", object ["from" .= ("." :: T.Text)])
    , ("non-string destination", object ["from" .= ("." :: T.Text), "to" .= (1 :: Int)])
    , ("non-string mode", object ["from" .= ("." :: T.Text), "to" .= ("/x" :: T.Text), "mode" .= (644 :: Int)])
    , ("unknown field", object ["from" .= ("." :: T.Text), "to" .= ("/x" :: T.Text), "extra" .= True])
    ]

  it "rejects mixed kinds even for an empty source" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      let mixed = object ["directory" .= object ["from" .= base, "to" .= ("/x" :: T.Text)], "shell" .= ("true" :: T.Text)]
      preprocessRoot base (pipeline [mixed]) `shouldThrow` anyIOException

  it "reports missing sources and non-directory roots" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      preprocessRoot base (pipeline (directory "missing")) `shouldThrow` anyIOException
      BS.writeFile (base </> "plain") "a"
      preprocessRoot base (pipeline (directory "plain")) `shouldThrow` anyIOException

  it "rejects source-root links, file links, directory links and broken links" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      let root = base </> "files"
      createDirectory root
      BS.writeFile (base </> "plain") "a"
      createSymbolicLink root (base </> "link")
      preprocessRoot base (pipeline (directory "link")) `shouldThrow` anyIOException
      preprocessRoot base (pipeline (directory "link/")) `shouldThrow` anyIOException
      preprocessRoot base (pipeline (directory "link/.")) `shouldThrow` anyIOException
      mapM_
        ( \(target, name) -> do
            let path = root </> name
            createSymbolicLink target path
            preprocessRoot base (pipeline (directory "files")) `shouldThrow` (\err -> path `isInfixOf` show (err :: IOError))
            removeFile path
        )
        [(base </> "plain", "file-link"), (root, "dir-link"), (base </> "missing", "broken-link")]

  it "rejects FIFOs without trying to read them" $
    withSystemTempDirectory "corvus-directory" $ \base -> do
      createNamedPipe (base </> "fifo") 0o600
      preprocessRoot base (pipeline (directory base)) `shouldThrow` anyIOException
  where
    invalidCase (label, fields) =
      it ("rejects " <> label) $
        preprocessRoot "." (pipeline [object ["directory" .= fields]]) `shouldThrow` anyIOException
    addVars (Object fields) = Object (KM.insert "vars" (object ["source" .= Null]) fields)
    addVars value = value
