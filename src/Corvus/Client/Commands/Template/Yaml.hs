{-# LANGUAGE OverloadedStrings #-}

-- | Convert a 'TemplateDetails' (as returned by @ReqTemplateShow@) into the
-- YAML shape accepted by 'ReqTemplateCreate' / 'ReqTemplateUpdate'. The
-- server's default @ToJSON TemplateDetails@ uses a slightly different shape
-- (it has @id@, @createdAt@, @cloneStrategy@, @diskImageId@), so we have a
-- dedicated encoder here for the round-trip case.
module Corvus.Client.Commands.Template.Yaml
  ( templateDetailsToYaml
  , skeletonTemplateYaml
  )
where

import Corvus.Protocol
  ( CloudInitInfo (..)
  , NamedRef (..)
  , TemplateAudioDeviceInfo (..)
  , TemplateDetails (..)
  , TemplateDriveInfo (..)
  , TemplateNetIfInfo (..)
  , TemplateSharedDirInfo (..)
  , TemplateSshKeyInfo (..)
  )
import Corvus.Size (formatSize)
import Data.Aeson (ToJSON, Value, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml

-- | Render a 'TemplateDetails' as a YAML document compatible with
-- @crv template create@'s parser.
templateDetailsToYaml :: TemplateDetails -> Text
templateDetailsToYaml t =
  TE.decodeUtf8 $ Yaml.encode $ templateDetailsToValue t

templateDetailsToValue :: TemplateDetails -> Value
templateDetailsToValue t =
  object $
    [ "name" .= tvdName t
    , "cpuCount" .= tvdCpuCount t
    , "ram" .= formatSize (tvdRam t)
    , "headless" .= tvdHeadless t
    , "graphicsAdapter" .= tvdGraphicsAdapter t
    , "vsock" .= tvdVsock t
    , "balloon" .= tvdBalloon t
    , "rng" .= tvdRng t
    , "cloudInit" .= tvdCloudInit t
    , "guestAgent" .= tvdGuestAgent t
    , "tpm" .= tvdTpm t
    , "autostart" .= tvdAutostart t
    , "drives" .= map driveToValue (tvdDrives t)
    , "networkInterfaces" .= map netIfToValue (tvdNetIfs t)
    , "sshKeys" .= map sshKeyToValue (tvdSshKeys t)
    , "sharedDirs" .= map sharedDirToValue (tvdSharedDirs t)
    , "audioDevices" .= map audioDeviceToValue (tvdAudioDevices t)
    ]
      ++ catMaybes
        [ optPair "description" (tvdDescription t)
        , optPair "cloudInitConfig" (fmap cloudInitInfoToValue (tvdCloudInitConfig t))
        ]

driveToValue :: TemplateDriveInfo -> Value
driveToValue d =
  object $
    [ "interface" .= tvdiInterface d
    , "readOnly" .= tvdiReadOnly d
    , "cacheType" .= tvdiCacheType d
    , "discard" .= tvdiDiscard d
    , "strategy" .= tvdiCloneStrategy d
    ]
      ++ catMaybes
        [ optPair "diskImageName" (nrName <$> tvdiDiskImage d)
        , optPair "media" (tvdiMedia d)
        , optPair "size" (formatSize <$> tvdiSize d)
        , optPair "format" (tvdiFormat d)
        , optPair "ephemeral" (tvdiEphemeral d)
        ]

netIfToValue :: TemplateNetIfInfo -> Value
netIfToValue n =
  object $
    ("type" .= tvniType n)
      : ("model" .= tvniModel n)
      : catMaybes
        [ optPair "hostDevice" (tvniHostDevice n)
        , optPair "network" (tvniNetwork n)
        ]

sshKeyToValue :: TemplateSshKeyInfo -> Value
sshKeyToValue k =
  object ["name" .= tvskiName k]

sharedDirToValue :: TemplateSharedDirInfo -> Value
sharedDirToValue sd =
  object
    [ "path" .= tvsdiPath sd
    , "tag" .= tvsdiTag sd
    , "cache" .= tvsdiCache sd
    , "readOnly" .= tvsdiReadOnly sd
    ]

audioDeviceToValue :: TemplateAudioDeviceInfo -> Value
audioDeviceToValue device = object ["backend" .= tvadiBackend device, "model" .= tvadiModel device, "options" .= tvadiOptions device]

cloudInitInfoToValue :: CloudInitInfo -> Value
cloudInitInfoToValue ci =
  object $
    ("injectSshKeys" .= ciiInjectSshKeys ci)
      : catMaybes
        [ optPair "userData" (ciiUserData ci)
        , optPair "networkConfig" (ciiNetworkConfig ci)
        ]

-- | Include a key/value pair only when the value is 'Just'.
optPair :: (ToJSON a) => Key -> Maybe a -> Maybe (Key, Value)
optPair _ Nothing = Nothing
optPair k (Just x) = Just (k, toJSON x)

-- | Sample skeleton used by @crv template create@ when no file is supplied.
-- Contains the minimum set of fields the parser requires plus comments to
-- guide the user.
skeletonTemplateYaml :: Text
skeletonTemplateYaml =
  "# Edit this file, save and exit to create the template.\n\
  \# Lines starting with '#' are comments and can be removed.\n\
  \name: my-template\n\
  \description: \"\"\n\
  \cpuCount: 1\n\
  \ram: 1024M\n\
  \headless: false\n\
  \graphicsAdapter: virtio-vga\n\
  \vsock: true\n\
  \balloon: true\n\
  \rng: true\n\
  \cloudInit: false\n\
  \guestAgent: false\n\
  \tpm: false\n\
  \autostart: false\n\
  \drives:\n\
  \  - diskImageName: disk0\n\
  \    interface: virtio\n\
  \    strategy: create  # options are create, clone, overlay, direct\n\
  \    format: qcow2     # only for create strategy\n\
  \    size: 8192M      # for create, clone, overlay strategies\n\
  \networkInterfaces: []\n\
  \sshKeys: []\n\
  \sharedDirs: []        # list of {path, tag, cache, readOnly} entries\n\
  \audioDevices: []     # list of {backend: spice|pulse|pipewire, model: virtio-sound|intel-hda|ich9-intel-hda|AC97, options: ''} entries\n"
