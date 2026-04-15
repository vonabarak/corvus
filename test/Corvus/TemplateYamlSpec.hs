{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure round-trip test for 'templateDetailsToYaml': serialise a
-- 'TemplateDetails', re-parse it via the server's 'TemplateYaml' 'FromJSON'
-- instance, and confirm the fields survive.
module Corvus.TemplateYamlSpec (spec) where

import Corvus.Client.Commands.Template.Yaml (skeletonTemplateYaml, templateDetailsToYaml)
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..), DriveMedia (..), NetInterfaceType (..), TemplateCloneStrategy (..))
import Corvus.Protocol
  ( TemplateDetails (..)
  , TemplateDriveInfo (..)
  , TemplateNetIfInfo (..)
  , TemplateSshKeyInfo (..)
  )
import Corvus.Schema.Template
  ( TemplateDriveYaml (..)
  , TemplateNetworkInterfaceYaml (..)
  , TemplateSshKeyYaml (..)
  , TemplateYaml (..)
  )
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified Data.Yaml as Yaml
import Test.Hspec

sampleDetails :: TemplateDetails
sampleDetails =
  TemplateDetails
    { tvdId = 42
    , tvdName = "sample-tpl"
    , tvdCpuCount = 2
    , tvdRamMb = 4096
    , tvdDescription = Just "A sample template"
    , tvdHeadless = False
    , tvdCloudInit = True
    , tvdGuestAgent = True
    , tvdAutostart = False
    , tvdCloudInitConfig = Nothing
    , tvdCreatedAt = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
    , tvdDrives =
        [ TemplateDriveInfo
            { tvdiDiskImageId = Just 7
            , tvdiDiskImageName = Just "base-disk"
            , tvdiInterface = InterfaceVirtio
            , tvdiMedia = Just MediaDisk
            , tvdiReadOnly = False
            , tvdiCacheType = CacheWriteback
            , tvdiDiscard = True
            , tvdiCloneStrategy = StrategyOverlay
            , tvdiSizeMb = Just 20480
            , tvdiFormat = Nothing
            }
        ]
    , tvdNetIfs =
        [ TemplateNetIfInfo
            { tvniType = NetUser
            , tvniHostDevice = Nothing
            }
        ]
    , tvdSshKeys =
        [ TemplateSshKeyInfo
            { tvskiId = 3
            , tvskiName = "admin-key"
            }
        ]
    }

spec :: Spec
spec = do
  describe "templateDetailsToYaml" $ do
    it "round-trips through TemplateYaml parser with all fields preserved" $ do
      let yamlText = templateDetailsToYaml sampleDetails
      case Yaml.decodeEither' (TE.encodeUtf8 yamlText) of
        Left err -> expectationFailure $ "parse failed: " ++ show err
        Right (ty :: TemplateYaml) -> do
          tyName ty `shouldBe` "sample-tpl"
          tyCpuCount ty `shouldBe` 2
          tyRamMb ty `shouldBe` 4096
          tyDescription ty `shouldBe` Just "A sample template"
          tyHeadless ty `shouldBe` False
          tyCloudInit ty `shouldBe` True
          tyGuestAgent ty `shouldBe` True
          tyAutostart ty `shouldBe` False
          length (tyDrives ty) `shouldBe` 1
          let [d] = tyDrives ty
          tdyDiskImageName d `shouldBe` Just "base-disk"
          tdyInterface d `shouldBe` InterfaceVirtio
          tdyMedia d `shouldBe` Just MediaDisk
          tdyReadOnly d `shouldBe` Just False
          tdyCacheType d `shouldBe` Just CacheWriteback
          tdyDiscard d `shouldBe` Just True
          tdyStrategy d `shouldBe` StrategyOverlay
          tdySizeMb d `shouldBe` Just 20480
          length (tyNetworkInterfaces ty) `shouldBe` 1
          let [n] = tyNetworkInterfaces ty
          tnyType n `shouldBe` NetUser
          tnyHostDevice n `shouldBe` Nothing
          length (tySshKeys ty) `shouldBe` 1
          let [k] = tySshKeys ty
          tkyName k `shouldBe` "admin-key"

    it "round-trips a create-strategy drive (no diskImageName)" $ do
      let createDetails =
            sampleDetails
              { tvdDrives =
                  [ TemplateDriveInfo
                      { tvdiDiskImageId = Nothing
                      , tvdiDiskImageName = Nothing
                      , tvdiInterface = InterfaceVirtio
                      , tvdiMedia = Nothing
                      , tvdiReadOnly = False
                      , tvdiCacheType = CacheNone
                      , tvdiDiscard = False
                      , tvdiCloneStrategy = StrategyCreate
                      , tvdiSizeMb = Just 10240
                      , tvdiFormat = Just FormatQcow2
                      }
                  ]
              }
          yamlText = templateDetailsToYaml createDetails
      case Yaml.decodeEither' (TE.encodeUtf8 yamlText) of
        Left err -> expectationFailure $ "parse failed: " ++ show err
        Right (ty :: TemplateYaml) -> do
          let [d] = tyDrives ty
          tdyDiskImageName d `shouldBe` Nothing
          tdyStrategy d `shouldBe` StrategyCreate
          tdyFormat d `shouldBe` Just FormatQcow2
          tdySizeMb d `shouldBe` Just 10240

    it "omits description when it is Nothing" $ do
      let details = sampleDetails {tvdDescription = Nothing}
          yamlText = templateDetailsToYaml details
      case Yaml.decodeEither' (TE.encodeUtf8 yamlText) of
        Left err -> expectationFailure $ "parse failed: " ++ show err
        Right (ty :: TemplateYaml) -> tyDescription ty `shouldBe` Nothing

  describe "skeletonTemplateYaml" $ do
    it "parses as a valid TemplateYaml" $ do
      case Yaml.decodeEither' (TE.encodeUtf8 skeletonTemplateYaml) of
        Left err -> expectationFailure $ "skeleton failed to parse: " ++ show err
        Right (ty :: TemplateYaml) -> do
          tyName ty `shouldBe` "my-template"
          length (tyDrives ty) `shouldBe` 1
