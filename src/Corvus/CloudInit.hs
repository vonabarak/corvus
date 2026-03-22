{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cloud-init configuration generation for VMs.
-- Creates NoCloud datasource ISOs for injecting SSH keys and configuration.
module Corvus.CloudInit
  ( -- * Configuration
    CloudInitConfig (..)
  , defaultCloudInitConfig

    -- * ISO paths
  , getCloudInitIsoPath
  , getCloudInitDir

    -- * ISO generation
  , generateCloudInitIso
  , removeCloudInitIso
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Corvus.Utils.Yaml (yamlQQ)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Cloud-init configuration
data CloudInitConfig = CloudInitConfig
  { ciUser :: !Text
  -- ^ Username to create
  , ciHostname :: !Text
  -- ^ Hostname for the VM
  , ciInstanceId :: !Text
  -- ^ Instance ID (should be unique per VM)
  }
  deriving (Show, Eq)

-- | Default cloud-init configuration
defaultCloudInitConfig :: CloudInitConfig
defaultCloudInitConfig =
  CloudInitConfig
    { ciUser = "corvus"
    , ciHostname = "corvus-vm"
    , ciInstanceId = "corvus-001"
    }

-- | Get the directory for storing cloud-init files for a VM
-- Returns: $qcBasePath/<vm-name>/
getCloudInitDir :: QemuConfig -> Text -> IO FilePath
getCloudInitDir config vmName = do
  basePath <- getEffectiveBasePath config
  let vmDir = basePath </> T.unpack vmName
  createDirectoryIfMissing True vmDir
  pure vmDir

-- | Get the path to the cloud-init ISO for a VM
-- Returns: $qcBasePath/<vm-name>/cloud-init.iso
getCloudInitIsoPath :: QemuConfig -> Text -> IO FilePath
getCloudInitIsoPath config vmName = do
  vmDir <- getCloudInitDir config vmName
  pure $ vmDir </> "cloud-init.iso"

-- | Generate user-data YAML for cloud-init with multiple SSH keys
generateUserData :: CloudInitConfig -> [Text] -> Text
generateUserData config sshPubKeys' =
  let sshPubKeys = map T.strip sshPubKeys'
      user = ciUser config
      home = "/home/" <> user
      runcmds =
        [ "chmod 700 " <> home
        , "chmod 700 " <> home <> "/.ssh"
        , "chmod 600 " <> home <> "/.ssh/authorized_keys"
        , "chown -R " <> user <> ":" <> user <> " " <> home <> "/.ssh"
        , "echo 'permit nopass " <> user <> "' > /etc/doas.d/" <> user <> ".conf 2>/dev/null || true"
        , "rc-service sshd restart || systemctl restart ssh || systemctl restart sshd || true"
        ]
   in "#cloud-config\n"
        <> T.decodeUtf8
          ( Yaml.encode
              [yamlQQ|
            ssh_genkeytypes:
              - rsa
              - ed25519
            bootcmd:
              - test -f /etc/ssh/ssh_host_rsa_key || ssh-keygen -t rsa -f /etc/ssh/ssh_host_rsa_key -N ''
              - test -f /etc/ssh/ssh_host_ed25519_key || ssh-keygen -t ed25519 -f /etc/ssh/ssh_host_ed25519_key -N ''
            chpasswd:
              expire: false
            users:
              - name: #{user}
                sudo: ALL=(ALL) NOPASSWD:ALL
                shell: /bin/sh
                lock_passwd: false
                plain_text_passwd: corvus
                ssh_authorized_keys: #{sshPubKeys}
            ssh_pwauth: true
            disable_root: true
            package_update: false
            package_upgrade: false
            runcmd: #{runcmds}
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

-- | Generate a cloud-init NoCloud ISO image with multiple SSH keys.
-- The ISO contains user-data and meta-data files.
-- Returns the path to the generated ISO, or an error message.
generateCloudInitIso :: FilePath -> CloudInitConfig -> [Text] -> IO (Either Text FilePath)
generateCloudInitIso targetDir config sshPubKeys = do
  -- Ensure target directory exists
  createDirectoryIfMissing True targetDir

  let userDataPath = targetDir </> "user-data"
      metaDataPath = targetDir </> "meta-data"
      isoPath = targetDir </> "cloud-init.iso"

  -- Write cloud-init files
  TIO.writeFile userDataPath (generateUserData config sshPubKeys)
  TIO.writeFile metaDataPath (generateMetaData config)

  -- Try genisoimage first, fall back to mkisofs
  result <- tryGenIsoImage userDataPath metaDataPath isoPath
  case result of
    Right _ -> do
      -- Clean up temp files
      removeIfExists userDataPath
      removeIfExists metaDataPath
      pure $ Right isoPath
    Left _ -> do
      -- Try mkisofs as fallback
      mkResult <- tryMkIsofs userDataPath metaDataPath isoPath
      case mkResult of
        Right _ -> do
          removeIfExists userDataPath
          removeIfExists metaDataPath
          pure $ Right isoPath
        Left err -> pure $ Left err

-- | Try to create ISO using genisoimage
tryGenIsoImage :: FilePath -> FilePath -> FilePath -> IO (Either Text ())
tryGenIsoImage userDataPath metaDataPath isoPath = do
  result <-
    try $
      readProcessWithExitCode
        "genisoimage"
        [ "-output"
        , isoPath
        , "-volid"
        , "cidata"
        , "-joliet"
        , "-rock"
        , userDataPath
        , metaDataPath
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
        [ "-output"
        , isoPath
        , "-volid"
        , "cidata"
        , "-joliet"
        , "-rock"
        , userDataPath
        , metaDataPath
        ]
        ""
  case result of
    Left (_ :: SomeException) -> pure $ Left "mkisofs not found"
    Right (ExitSuccess, _, _) -> pure $ Right ()
    Right (ExitFailure n, _, stderr) ->
      pure $
        Left $
          "mkisofs failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Remove cloud-init ISO file
removeCloudInitIso :: FilePath -> IO ()
removeCloudInitIso = removeIfExists

-- | Helper to remove file if it exists
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
