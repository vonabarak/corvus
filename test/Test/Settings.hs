{-# LANGUAGE OverloadedStrings #-}

-- | Default settings for tests.
-- Contains database and VM image configuration.
module Test.Settings
  ( -- * Database settings
    TestDbConfig (..),
    getTestDbConfig,

    -- * Image settings
    ImageConfig (..),
    getImageConfig,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------
-- Database Configuration
--------------------------------------------------------------------------------

-- | Configuration for test database connections
data TestDbConfig = TestDbConfig
  { -- | PostgreSQL host
    tdcHost :: !Text,
    -- | PostgreSQL port
    tdcPort :: !Int,
    -- | PostgreSQL user
    tdcUser :: !Text,
    -- | PostgreSQL password
    tdcPassword :: !Text,
    -- | Admin database (used to create/drop test databases)
    tdcAdminDb :: !Text
  }
  deriving (Show)

-- | Get test database configuration from environment variables
getTestDbConfig :: IO TestDbConfig
getTestDbConfig = do
  host <- maybe "localhost" T.pack <$> lookupEnv "TEST_DB_HOST"
  port <- maybe 5432 read <$> lookupEnv "TEST_DB_PORT"
  user <- maybe "corvus" T.pack <$> lookupEnv "TEST_DB_USER"
  password <- maybe "corvus" T.pack <$> lookupEnv "TEST_DB_PASSWORD"
  adminDb <- maybe "postgres" T.pack <$> lookupEnv "TEST_DB_ADMIN"
  pure
    TestDbConfig
      { tdcHost = host,
        tdcPort = port,
        tdcUser = user,
        tdcPassword = password,
        tdcAdminDb = adminDb
      }

--------------------------------------------------------------------------------
-- Image Configuration
--------------------------------------------------------------------------------

-- | Configuration for VM images
data ImageConfig = ImageConfig
  { -- | URL to download the base image from
    icImageUrl :: !Text,
    -- | Filename for the cached image
    icImageName :: !Text
  }
  deriving (Show, Eq)

-- | Mapping of short OS names to image configurations
imageConfigs :: [(Text, ImageConfig)]
imageConfigs =
  [ ( "almalinux-10",
      ImageConfig
        { icImageUrl = "https://repo.almalinux.org/almalinux/10/cloud/x86_64_v2/images/AlmaLinux-10-GenericCloud-10.1-20251125.0.x86_64_v2.qcow2",
          icImageName = "AlmaLinux-10-GenericCloud-10.1-20251125.0.x86_64_v2.qcow2"
        }
    ),
    ( "alpine-3.20-uefi",
      ImageConfig
        { icImageUrl = "https://dev.alpinelinux.org/~tomalok/alpine-cloud-images/v3.20/nocloud/x86_64/nocloud_alpine-3.20.9-x86_64-uefi-cloudinit-r0.qcow2",
          icImageName = "nocloud_alpine-3.20.9-x86_64-uefi-cloudinit-r0.qcow2"
        }
    ),
    ( "alpine-3.20-bios",
      ImageConfig
        { icImageUrl = "https://dev.alpinelinux.org/~tomalok/alpine-cloud-images/v3.20/nocloud/x86_64/nocloud_alpine-3.20.9-x86_64-bios-cloudinit-r0.qcow2",
          icImageName = "nocloud_alpine-3.20.9-x86_64-bios-cloudinit-r0.qcow2"
        }
    ),
    ( "ubuntu-24.04",
      ImageConfig
        { icImageUrl = "https://cloud-images.ubuntu.com/releases/noble/release/ubuntu-24.04-server-cloudimg-amd64.img",
          icImageName = "ubuntu-24.04-server-cloudimg-amd64.img"
        }
    ),
    ( "debian-12",
      ImageConfig
        { icImageUrl = "https://cloud.debian.org/images/cloud/bookworm/latest/debian-12-generic-amd64.qcow2",
          icImageName = "debian-12-generic-amd64.qcow2"
        }
    )
  ]

-- | Get image configuration by OS name
getImageConfig :: Text -> Maybe ImageConfig
getImageConfig name = lookup name imageConfigs
