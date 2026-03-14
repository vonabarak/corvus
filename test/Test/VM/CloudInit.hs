{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cloud-init configuration generation for VM integration tests.
-- Creates NoCloud datasource ISOs for injecting SSH keys and configuration.
module Test.VM.CloudInit
  ( -- * Configuration
    CloudInitConfig (..),
    defaultCloudInitConfig,

    -- * ISO generation
    generateCloudInitIso,
    removeCloudInitIso,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Utils.Yaml (yamlQQ)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Cloud-init configuration
data CloudInitConfig = CloudInitConfig
  { -- | Username to create
    ciUser :: !Text,
    -- | Hostname for the VM
    ciHostname :: !Text,
    -- | Instance ID (should be unique per VM)
    ciInstanceId :: !Text
  }
  deriving (Show, Eq)

-- | Default cloud-init configuration
defaultCloudInitConfig :: CloudInitConfig
defaultCloudInitConfig =
  CloudInitConfig
    { ciUser = "test",
      ciHostname = "corvus-test-vm",
      ciInstanceId = "corvus-test-001"
    }

-- | Generate user-data YAML for cloud-init
generateUserData :: CloudInitConfig -> Text -> Text
generateUserData config sshPubKey =
  "#cloud-config\n"
    <> T.decodeUtf8
      ( Yaml.encode
          [yamlQQ|
            users:
              - name: #{ciUser config}
                sudo: ALL=(ALL) NOPASSWD:ALL
                shell: /bin/bash
                lock_passwd: true
                ssh_authorized_keys: [#{T.strip sshPubKey}]
            ssh_pwauth: false
            disable_root: true
            package_update: false
            package_upgrade: false
            runcmd:
              - systemctl enable ssh
              - systemctl start ssh
          |]
      )

-- | Generate meta-data for cloud-init
generateMetaData :: CloudInitConfig -> Text
generateMetaData config =
  T.decodeUtf8 $
    Yaml.encode
      [yamlQQ|
        instance-id: #{ciInstanceId config}
        local-hostname: #{ciHostname config}
      |]

-- | Generate a cloud-init NoCloud ISO image.
-- The ISO contains user-data and meta-data files.
generateCloudInitIso :: FilePath -> CloudInitConfig -> Text -> IO (Either Text FilePath)
generateCloudInitIso tmpDir config sshPubKey = do
  let userDataPath = tmpDir </> "user-data"
      metaDataPath = tmpDir </> "meta-data"
      isoPath = tmpDir </> "cloud-init.iso"

  -- Write cloud-init files
  TIO.writeFile userDataPath (generateUserData config sshPubKey)
  TIO.writeFile metaDataPath (generateMetaData config)

  -- Try genisoimage first, fall back to mkisofs
  result <- tryGenIsoImage userDataPath metaDataPath isoPath
  case result of
    Right _ -> pure $ Right isoPath
    Left _ -> do
      -- Try mkisofs as fallback
      mkResult <- tryMkIsofs userDataPath metaDataPath isoPath
      case mkResult of
        Right _ -> pure $ Right isoPath
        Left err -> pure $ Left err

-- | Try to create ISO using genisoimage
tryGenIsoImage :: FilePath -> FilePath -> FilePath -> IO (Either Text ())
tryGenIsoImage userDataPath metaDataPath isoPath = do
  result <-
    try $
      readProcessWithExitCode
        "genisoimage"
        [ "-output",
          isoPath,
          "-volid",
          "cidata",
          "-joliet",
          "-rock",
          userDataPath,
          metaDataPath
        ]
        ""
  case result of
    Left (_ :: SomeException) -> pure $ Left "genisoimage not found"
    Right (ExitSuccess, _, _) -> pure $ Right ()
    Right (ExitFailure n, _, stderr) ->
      pure $
        Left $
          "genisoimage failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Try to create ISO using mkisofs
tryMkIsofs :: FilePath -> FilePath -> FilePath -> IO (Either Text ())
tryMkIsofs userDataPath metaDataPath isoPath = do
  result <-
    try $
      readProcessWithExitCode
        "mkisofs"
        [ "-output",
          isoPath,
          "-volid",
          "cidata",
          "-joliet",
          "-rock",
          userDataPath,
          metaDataPath
        ]
        ""
  case result of
    Left (_ :: SomeException) -> pure $ Left "mkisofs not found"
    Right (ExitSuccess, _, _) -> pure $ Right ()
    Right (ExitFailure n, _, stderr) ->
      pure $
        Left $
          "mkisofs failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Remove cloud-init ISO and associated files
removeCloudInitIso :: FilePath -> IO ()
removeCloudInitIso tmpDir = do
  let files =
        [ tmpDir </> "cloud-init.iso",
          tmpDir </> "user-data",
          tmpDir </> "meta-data"
        ]
  mapM_ removeIfExists files
  where
    removeIfExists path = do
      exists <- doesFileExist path
      when exists $ removeFile path
