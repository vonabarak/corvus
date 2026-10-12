{-# LANGUAGE OverloadedStrings #-}

-- | Canonical list column catalogs. Headers and selectors share the same names.
module Corvus.Client.Output.Columns
  ( vmColumns
  , vmSnapshotColumns
  , diskColumns
  , snapshotColumns
  , networkColumns
  , nodeColumns
  , templateVmColumns
  , taskColumns
  , sshKeyColumns
  , sshKeyVmColumns
  , netIfColumns
  , sharedDirColumns
  , audioDeviceColumns
  , entityLabel
  , formatTimestamp
  , durationOf
  , durationLabel
  , formatDuration
  , isTaskPending
  ) where

import Corvus.Client.Output (Align (..), Column (..), formatBool)
import Corvus.Model (EnumText (..), TaskResult (..), VmStatus (..))
import Corvus.Protocol
import Corvus.Size (formatSize)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Text.Printf (printf)

vmColumns :: UTCTime -> [Column VmInfo]
vmColumns now =
  [ Column "ID" RightAlign (show . viId)
  , Column "NAME" LeftAlign (T.unpack . viName)
  , Column "NODE" LeftAlign (T.unpack . nrName . viNode)
  , Column "STATUS" LeftAlign (T.unpack . enumToText . viStatus)
  , Column "CPUS" RightAlign (show . viCpuCount)
  , Column "RAM" RightAlign (formatSize . viRam)
  , Column "HEALTH" LeftAlign (healthLabel now)
  , Column "GUEST_AGENT" LeftAlign (formatBool . viGuestAgent)
  , Column "CLOUD_INIT" LeftAlign (formatBool . viCloudInit)
  , Column "HEADLESS" LeftAlign (formatBool . viHeadless)
  , ExtraColumn "TPM" LeftAlign (formatBool . viTpm)
  , ExtraColumn "AUTOSTART" LeftAlign (formatBool . viAutostart)
  , ExtraColumn "HEALTHCHECK" LeftAlign (maybe "-" formatTimestamp . viHealthcheck)
  , ExtraColumn "REBOOT_QUIRK" LeftAlign (formatBool . viRebootQuirk)
  , ExtraColumn "CPU_MODEL" LeftAlign (textOrDash . viCpuModel)
  , ExtraColumn "GRAPHICS_ADAPTER" LeftAlign (T.unpack . enumToText . viGraphicsAdapter)
  , ExtraColumn "VSOCK" LeftAlign (formatBool . viVsock)
  , ExtraColumn "BALLOON" LeftAlign (formatBool . viBalloon)
  , ExtraColumn "RNG" LeftAlign (formatBool . viRng)
  , ExtraColumn "STARTUP_TIMEOUT_SEC" RightAlign (show . viStartupTimeoutSec)
  ]

vmSnapshotColumns :: [Column VmSnapshotInfo]
vmSnapshotColumns =
  [ Column "NAME" LeftAlign (T.unpack . vsiName)
  , Column "CREATED" LeftAlign (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . vsiCreatedAt)
  , Column "CARRIER" LeftAlign (T.unpack . nrName . vsiCarrierDisk)
  , Column "DISKS" RightAlign (show . vsiDiskCount)
  , Column "SIZE" RightAlign (formatSize . vsiTotalSize)
  , ExtraColumn "VM" LeftAlign (namedRef . vsiVm)
  ]

healthLabel :: UTCTime -> VmInfo -> String
healthLabel now vm
  | not (viGuestAgent vm) = "--"
  | viStatus vm /= VmRunning = "--"
  | otherwise = case viHealthcheck vm of
      Nothing -> "DOWN"
      Just t -> if diffUTCTime now t < 30 then "OK" else "DOWN"

diskColumns :: [Column DiskImageInfo]
diskColumns =
  [ Column "ID" RightAlign (show . diiId)
  , Column "NAME" LeftAlign (T.unpack . diiName)
  , Column "TAGS" LeftAlign (T.unpack . T.intercalate "," . diiTags)
  , Column "FORMAT" LeftAlign (T.unpack . enumToText . diiFormat)
  , Column "SIZE" RightAlign (maybe "-" formatSize . diiSize)
  , Column "EPHEMERAL" LeftAlign (formatBool . diiEphemeral)
  , Column "ATTACHED_TO" LeftAlign formatAttached
  , ExtraColumn "CREATED" LeftAlign (formatTimestamp . diiCreatedAt)
  , ExtraColumn "BACKING_IMAGE" LeftAlign (maybe "-" namedRef . diiBackingImage)
  , ExtraColumn "PLACEMENTS" LeftAlign (placements . diiPlacements)
  ]
  where
    formatAttached d
      | null (diiAttachedTo d) = "-"
      | otherwise = T.unpack (T.intercalate ", " (map nrName (diiAttachedTo d)))

snapshotColumns :: [Column SnapshotInfo]
snapshotColumns =
  [ Column "ID" RightAlign (show . sniId)
  , Column "NAME" LeftAlign (T.unpack . sniName)
  , Column "CREATED" LeftAlign (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . sniCreatedAt)
  , Column "SIZE" RightAlign (maybe "-" formatSize . sniSize)
  , Column "LIVE" LeftAlign (formatBool . sniLive)
  , Column "QUIESCED" LeftAlign (formatBool . sniQuiesced)
  , Column "HAS_VMSTATE" LeftAlign (formatBool . sniHasVmstate)
  ]

nodeColumns :: [Column NodeInfo]
nodeColumns =
  [ Column "ID" RightAlign (show . noiId)
  , Column "NAME" LeftAlign (T.unpack . noiName)
  , Column "HOST" LeftAlign (T.unpack . noiHost)
  , Column "STATE" LeftAlign (T.unpack . enumToText . noiAdminState)
  , Column "NETD" LeftAlign netdState
  , Column "CPUS" RightAlign (maybe "--" show . noiCpuCount)
  , Column "RAM_FREE" RightAlign (maybe "--" formatSize . noiRamFree)
  , Column "DISK_FREE" RightAlign $ \n ->
      maybe
        "--"
        formatSize
        (noiStorageBytesFree n)
  , Column "LOAD1" RightAlign (maybe "--" (formatLoad . (\x -> x :: Double)) . noiLoadAvg1)
  , ExtraColumn "NODE_AGENT_PORT" RightAlign (show . noiNodeAgentPort)
  , ExtraColumn "NET_AGENT_PORT" RightAlign (show . noiNetAgentPort)
  , ExtraColumn "CREATED" LeftAlign (formatTimestamp . noiCreatedAt)
  , ExtraColumn "RAM_TOTAL" RightAlign (maybe "-" formatSize . noiRamTotal)
  , ExtraColumn "DISK_TOTAL" RightAlign (maybe "-" formatSize . noiStorageBytesTotal)
  , ExtraColumn "LAST_NODE_AGENT_PUSH" LeftAlign (maybe "-" formatTimestamp . noiLastNodeAgentPushAt)
  , ExtraColumn "LAST_NET_AGENT_PUSH" LeftAlign (maybe "-" formatTimestamp . noiLastNetAgentPushAt)
  , ExtraColumn "NETD_DISABLED" LeftAlign (formatBool . noiNetdDisabled)
  , ExtraColumn "NETD_CONNECTED" LeftAlign (formatBool . noiNetdConnected)
  ]
  where
    formatLoad x =
      let s = (round (x * 100) :: Int)
       in show (fromIntegral s / (100 :: Double))
    netdState n
      | noiNetdDisabled n = "disabled"
      | noiNetdConnected n = "online"
      | otherwise = "offline"

networkColumns :: [Column NetworkInfo]
networkColumns =
  [ Column "ID" RightAlign (show . nwiId)
  , Column "NAME" LeftAlign (T.unpack . nwiName)
  , Column "SUBNET" LeftAlign (\i -> let s = nwiSubnet i in if T.null s then "-" else T.unpack s)
  , Column "DHCP" LeftAlign (formatBool . nwiDhcp)
  , Column "NAT" LeftAlign (formatBool . nwiNat)
  , Column "STATUS" LeftAlign (\i -> if nwiRunning i then "running" else "stopped")
  , Column "PEERS" RightAlign (show . length . nwiPeerNodeIds)
  , Column "AUTOSTART" LeftAlign (formatBool . nwiAutostart)
  , ExtraColumn "DNSMASQ_PID" RightAlign (maybe "-" show . nwiDnsmasqPid)
  , ExtraColumn "CREATED" LeftAlign (formatTimestamp . nwiCreatedAt)
  , ExtraColumn "VNI" RightAlign (maybe "-" show . nwiVni)
  , ExtraColumn "PEER_NODE_IDS" LeftAlign (commaList . map show . nwiPeerNodeIds)
  , ExtraColumn "DNS_SERVERS" LeftAlign (commaList . map T.unpack . nwiDnsServers)
  , ExtraColumn "DOMAIN" LeftAlign (textOrDash . nwiDomain)
  , ExtraColumn "HOST_DNS" LeftAlign (formatBool . nwiHostDns)
  ]

sshKeyColumns :: [Column SshKeyInfo]
sshKeyColumns =
  [ Column "ID" RightAlign (show . skiId)
  , Column "NAME" LeftAlign (T.unpack . skiName)
  , Column "PUBLIC_KEY" LeftAlign (T.unpack . skiPublicKey)
  , Column "ATTACHED_VMS" LeftAlign formatAttached
  , ExtraColumn "CREATED" LeftAlign (formatTimestamp . skiCreatedAt)
  ]
  where
    formatAttached k
      | null (skiAttachedVms k) = "-"
      | otherwise = T.unpack (T.intercalate ", " (map nrName (skiAttachedVms k)))

templateVmColumns :: [Column TemplateVmInfo]
templateVmColumns =
  [ Column "ID" RightAlign (show . tviId)
  , Column "NAME" LeftAlign (T.unpack . tviName)
  , Column "CPUS" RightAlign (show . tviCpuCount)
  , Column "RAM" RightAlign (formatSize . tviRam)
  , Column "TPM" LeftAlign (formatBool . tviTpm)
  , ExtraColumn "DESCRIPTION" LeftAlign (maybe "-" textOrDash . tviDescription)
  , ExtraColumn "HEADLESS" LeftAlign (formatBool . tviHeadless)
  , ExtraColumn "GUEST_AGENT" LeftAlign (formatBool . tviGuestAgent)
  , ExtraColumn "AUTOSTART" LeftAlign (formatBool . tviAutostart)
  , ExtraColumn "REBOOT_QUIRK" LeftAlign (formatBool . tviRebootQuirk)
  , ExtraColumn "GRAPHICS_ADAPTER" LeftAlign (T.unpack . enumToText . tviGraphicsAdapter)
  , ExtraColumn "VSOCK" LeftAlign (formatBool . tviVsock)
  , ExtraColumn "BALLOON" LeftAlign (formatBool . tviBalloon)
  , ExtraColumn "RNG" LeftAlign (formatBool . tviRng)
  , ExtraColumn "STARTUP_TIMEOUT_SEC" RightAlign (show . tviStartupTimeoutSec)
  ]

sharedDirColumns :: [Column SharedDirInfo]
sharedDirColumns =
  [ Column "ID" RightAlign (show . sdiId)
  , Column "PATH" LeftAlign (T.unpack . sdiPath)
  , Column "TAG" LeftAlign (T.unpack . sdiTag)
  , Column "CACHE" LeftAlign (T.unpack . enumToText . sdiCache)
  , Column "READ_ONLY" LeftAlign (formatBool . sdiReadOnly)
  , Column "PID" RightAlign (maybe "-" show . sdiPid)
  ]

netIfColumns :: [Column NetIfInfo]
netIfColumns =
  [ Column "ID" RightAlign (show . niId)
  , Column "MODEL" LeftAlign (T.unpack . enumToText . niModel)
  , Column "TYPE" LeftAlign (T.unpack . enumToText . niType)
  , Column "DEVICE" LeftAlign (T.unpack . niHostDevice)
  , Column "MAC" LeftAlign (T.unpack . niMacAddress)
  , Column "GUEST_IPS" LeftAlign (maybe "" T.unpack . niGuestIpAddresses)
  , ExtraColumn "NETWORK" LeftAlign (maybe "-" namedRef . niNetwork)
  , ExtraColumn "IP_ADDRESS" LeftAlign (maybe "-" textOrDash . niIpAddress)
  ]

taskColumns :: UTCTime -> [Column TaskInfo]
taskColumns now =
  [ Column "ID" RightAlign (show . tiId)
  , Column "SUBSYSTEM" LeftAlign (T.unpack . enumToText . tiSubsystem)
  , Column "COMMAND" LeftAlign (T.unpack . tiCommand)
  , Column "ENTITY" LeftAlign entityLabel
  , Column "CLIENT" LeftAlign (T.unpack . tiClientName)
  , Column "RESULT" LeftAlign (T.unpack . enumToText . tiResult)
  , Column "STARTED" LeftAlign (formatTimestamp . tiStartedAt)
  , Column "DURATION" RightAlign (durationLabel now)
  , Column "MESSAGE" LeftAlign (T.unpack . fromMaybe "-" . tiMessage)
  , ExtraColumn "PARENT_ID" RightAlign (maybe "-" show . tiParentId)
  , ExtraColumn "FINISHED" LeftAlign (maybe "-" formatTimestamp . tiFinishedAt)
  ]

entityLabel :: TaskInfo -> String
entityLabel info = case tiEntity info of
  Just e -> T.unpack (nrName e) ++ " (" ++ show (nrId e) ++ ")"
  Nothing -> "-"

formatTimestamp :: UTCTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

durationOf :: TaskInfo -> Maybe Double
durationOf info = do
  finish <- tiFinishedAt info
  pure $ realToFrac $ diffUTCTime finish (tiStartedAt info)

durationLabel :: UTCTime -> TaskInfo -> String
durationLabel now info = case tiFinishedAt info of
  Just finish -> formatDuration (realToFrac $ diffUTCTime finish (tiStartedAt info))
  Nothing ->
    if isTaskPending (tiResult info)
      then formatDuration (realToFrac $ diffUTCTime now (tiStartedAt info)) ++ "..."
      else "-"

formatDuration :: Double -> String
formatDuration d
  | d < 1.0 = printf "%.0fms" (d * 1000)
  | d < 60.0 = printf "%.1fs" d
  | d < 3600.0 = printf "%.0fm%.0fs" (d / 60 :: Double) (mod' d 60)
  | otherwise = printf "%.0fh%.0fm" (d / 3600 :: Double) (mod' (d / 60) 60)
  where
    mod' a b = a - b * fromIntegral (floor (a / b) :: Int)

isTaskPending :: TaskResult -> Bool
isTaskPending TaskRunning = True
isTaskPending TaskNotStarted = True
isTaskPending _ = False

audioDeviceColumns :: [Column AudioDeviceInfo]
audioDeviceColumns =
  [ Column "ID" RightAlign (show . adiId)
  , Column "MODEL" LeftAlign (T.unpack . enumToText . adiModel)
  , Column "BACKEND" LeftAlign (T.unpack . enumToText . adiBackend)
  , Column "OPTIONS" LeftAlign (T.unpack . adiOptions)
  ]

-- | The VM-specific SSH key view hides attachment counts by default only.
sshKeyVmColumns :: [Column SshKeyInfo]
sshKeyVmColumns = map hideAttachments sshKeyColumns
  where
    hideAttachments c | colName c == "ATTACHED_VMS" = ExtraColumn (colName c) (colAlign c) (colRender c)
    hideAttachments c = c

textOrDash :: Text -> String
textOrDash t = if T.null t then "-" else T.unpack t

namedRef :: NamedRef -> String
namedRef ref = T.unpack (nrName ref) ++ " (" ++ show (nrId ref) ++ ")"

commaList :: [String] -> String
commaList [] = "-"
commaList xs = intercalate ", " xs

placements :: [DiskImagePlacement] -> String
placements [] = "-"
placements xs = intercalate "; " [namedRef (dipNode p) ++ ": " ++ T.unpack (dipFilePath p) | p <- xs]
