{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Canonical hashing for build-step caching.
--
-- A successful step's snapshot is keyed by a hex SHA-256 chain:
--
-- > envHash       = sha256(buildEnvelopeCanonicalBytes b)
-- > stepHash i    = sha256(provisionerCanonicalBytes step_i)
-- > chainHash 0   = sha256(envHash || stepHash 0)
-- > chainHash i   = sha256(chainHash (i-1) || stepHash i)
--
-- 'chainHashes' returns the @(stepHashHex, chainHashHex)@ pair for
-- every provisioner in 'buildProvisioners', left-to-right. The
-- envelope captures every YAML field that affects what the bake
-- VM does AT BAKE TIME: template, target spec (size/format/compact/
-- path/ifExists), strategy, bake VM (cpu/ram), shell defaults, boot
-- keys, floppy. Explicitly excluded: 'buildName' (operator label
-- that already scopes the pipeline key), 'buildDescription' /
-- 'buildNode' / 'buildCleanup' / 'buildWaitForShutdownSec' / the
-- cache flags themselves (operator policy, not bake content).
--
-- For provisioner steps, the canonical form filters out the
-- runtime-injected @CORVUS_BAKEVM_ID@ / @CORVUS_BUILD_TASK_ID@ envs
-- (they change every invocation) and any 'shellScript' / 'fileFrom' /
-- 'floppyFrom' fields (always 'Nothing' post-client-inline; including
-- them risks @Maybe@-vs-@null@ wire drift).
module Corvus.Build.Cache.Hash
  ( provisionerCanonicalBytes
  , buildEnvelopeCanonicalBytes
  , envelopeHash
  , chainHashes
  , cacheSnapshotName
  , shortChainHash
  )
where

import Corvus.Model (EnumText (..))
import Corvus.Schema.Build
import qualified Crypto.Hash as Hash
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Bifunctor
import qualified Data.ByteArray.Encoding as BAEnc
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

--------------------------------------------------------------------------------
-- Public surface
--------------------------------------------------------------------------------

-- | Canonical bytes for one provisioner step.
provisionerCanonicalBytes :: Provisioner -> ByteString
provisionerCanonicalBytes = Yaml.encode . provisionerValue

-- | Canonical bytes for the build envelope (everything that affects
-- the bake but isn't a per-step input).
buildEnvelopeCanonicalBytes :: Build -> ByteString
buildEnvelopeCanonicalBytes = Yaml.encode . envelopeValue

-- | Hex SHA-256 of the canonical envelope bytes.
envelopeHash :: Build -> Text
envelopeHash = sha256Hex . buildEnvelopeCanonicalBytes

-- | One @(stepHashHex, chainHashHex)@ pair per provisioner in
-- 'buildProvisioners', folded left-to-right.
chainHashes :: Build -> [(Text, Text)]
chainHashes b =
  let env = envelopeHash b
      steps = buildProvisioners b
   in go env steps
  where
    go _ [] = []
    go prevChain (s : rest) =
      let sHashHex = sha256Hex (provisionerCanonicalBytes s)
          chain = sha256HexCat prevChain sHashHex
       in (sHashHex, chain) : go chain rest

-- | Snapshot name for a cache entry: @"cache-" <> first 16 hex chars@.
-- Per-disk uniqueness already covers collision (the qcow2 snapshot
-- table is one namespace per disk), so 64 bits is plenty.
cacheSnapshotName :: Text -> Text
cacheSnapshotName h = "cache-" <> T.take 16 h

-- | First 16 hex chars of a chain hash. Used for log lines and the
-- snapshot name.
shortChainHash :: Text -> Text
shortChainHash = T.take 16

--------------------------------------------------------------------------------
-- Canonical Aeson values
--------------------------------------------------------------------------------

-- | Build envelope. Keys are deterministic; per-list inputs ('Shell'
-- env, 'ShellDefaults' env, 'buildBootKeys', etc.) are sorted so
-- semantically-equivalent reorderings don't bust the cache.
envelopeValue :: Build -> Value
envelopeValue b =
  obj
    [ ("template", String (buildTemplate b))
    , ("target", targetValue (buildTarget b))
    , ("strategy", String (strategyText (buildStrategy b)))
    , ("vm", buildVmValue (buildVm b))
    , ("shellDefaults", shellDefaultsValue (buildShellDefaults b))
    , ("bootKeys", Array (V.fromList (map bootKeyValue (buildBootKeys b))))
    , ("floppy", maybe Null floppyValue (buildFloppy b))
    , -- The cache mode is in the envelope because switching modes
      -- swaps the on-disk artifact shape: 'CacheModeMemory' rows
      -- carry vmstate inside their qcow2; 'CacheModeDisk' rows
      -- don't. Restoring across modes is not a thing — flip the
      -- mode, the cache rebuilds from scratch.
      ("cacheMode", String (cacheModeText (buildCacheMode b)))
    ]

-- | Only the target fields that affect what the BAKE produces go
-- into the hash. The bake VM gets a target disk created with the
-- target's @format@ (qcow2 vs raw changes the QEMU drive driver
-- and snapshot support) and @sizeGb@ (the disk's virtual size),
-- so those affect bake behaviour. Everything else is operator
-- policy applied to the FINAL published artifact AFTER the bake
-- completes:
--
--   * @path@      — where on disk to publish the cloned artifact
--   * @compact@   — whether to @qemu-img -c@ the published clone
--   * @ifExists@  — what to do when an artifact with the same name
--                   already exists (error / skip / overwrite)
--
-- Treating those as cache-busting was the original behaviour and
-- bit operators in the wild: flipping @ifExists@ from @overwrite@
-- to @skip@ (or moving the published artifact to a different
-- @path@) invalidated the entire cache prefix, forcing a full
-- rebake even though nothing about the bake itself changed.
-- Those fields are now excluded — same treatment as 'buildName',
-- 'buildCleanup', 'useCache' / 'buildCache' etc.
targetValue :: BuildTarget -> Value
targetValue t =
  obj
    [ ("format", String (enumToText (btFormat t)))
    , ("sizeGb", intValue (btSizeGb t))
    ]

buildVmValue :: BuildVm -> Value
buildVmValue v =
  obj
    [ ("cpuCount", intValue (bvmCpuCount v))
    , ("ramMb", intValue (bvmRamMb v))
    ]

shellDefaultsValue :: ShellDefaults -> Value
shellDefaultsValue sd =
  obj
    [ ("preamble", maybe Null String (sdPreamble sd))
    , ("env", envObject (sdEnv sd))
    ]

bootKeyValue :: BootKey -> Value
bootKeyValue bk =
  obj
    [ ("keys", String (bkKeys bk))
    , ("delaySec", intValue (bkDelaySec bk))
    , ("repeat", intValue (bkRepeat bk))
    , ("intervalSec", intValue (bkIntervalSec bk))
    ]

floppyValue :: Floppy -> Value
floppyValue f =
  -- Strip 'floppyFrom': it should be 'Nothing' at this point (the
  -- client inlines it as 'floppyContentBase64'). Including the
  -- 'Maybe FilePath' field would let the same floppy hash differently
  -- depending on whether the client preprocessed it locally vs the
  -- daemon side parsing the raw YAML.
  obj
    [ ("contentBase64", maybe Null String (floppyContentBase64 f))
    , ("filename", maybe Null String (floppyFilename f))
    ]

--------------------------------------------------------------------------------
-- Per-provisioner canonical values
--------------------------------------------------------------------------------

provisionerValue :: Provisioner -> Value
provisionerValue = \case
  ProvShell sh -> obj [("kind", String "shell"), ("data", shellValue sh)]
  ProvFile fp -> obj [("kind", String "file"), ("data", fileProvValue fp)]
  ProvWaitFor w -> obj [("kind", String "wait-for"), ("data", waitForValue w)]
  ProvReboot r -> obj [("kind", String "reboot"), ("data", rebootValue r)]

shellValue :: Shell -> Value
shellValue sh =
  -- Strip 'shellScript' (always Nothing post-client-inline). Strip
  -- the auto-injected runtime envs so each invocation doesn't bust
  -- the cache.
  obj
    [ ("inline", maybe Null String (shellInline sh))
    , ("workdir", maybe Null String (shellWorkdir sh))
    , ("env", envObject (stripInjectedEnvs (shellEnv sh)))
    , ("timeoutSec", maybe Null intValue (shellTimeoutSec sh))
    ]

fileProvValue :: FileProv -> Value
fileProvValue fp =
  -- Strip 'fileFrom' (always Nothing post-client-inline).
  obj
    [ ("content", maybe Null String (fileContentBase64 fp))
    , ("to", String (fileTo fp))
    , ("mode", maybe Null String (fileMode fp))
    ]

waitForValue :: WaitFor -> Value
waitForValue = \case
  WaitForPing t ->
    obj [("kind", String "ping"), ("timeoutSec", intValue t)]
  WaitForFile p t ->
    obj
      [ ("kind", String "file")
      , ("path", String p)
      , ("timeoutSec", intValue t)
      ]
  WaitForPort p t ->
    obj
      [ ("kind", String "port")
      , ("port", intValue p)
      , ("timeoutSec", intValue t)
      ]

rebootValue :: Reboot -> Value
rebootValue r =
  obj [("timeoutSec", intValue (rebootTimeoutSec r))]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Build a JSON object from a sorted (by key) association list. We
-- sort here to guarantee deterministic encoding; the Aeson KeyMap is
-- ordered in practice but the caller doesn't have to think about it.
obj :: [(Text, Value)] -> Value
obj kvs =
  Object . KM.fromList $ [(Key.fromText k, v) | (k, v) <- L.sortOn fst kvs]

-- | Encode a @[(Text, Text)]@ env list as a sorted object so
-- semantically-equivalent reorderings don't bust the cache.
envObject :: [(Text, Text)] -> Value
envObject = obj . map (Data.Bifunctor.second String) . L.sortOn fst

intValue :: Int -> Value
intValue = Number . fromIntegral

strategyText :: BuildStrategy -> Text
strategyText = \case
  BuildStrategyOverlay -> "overlay"
  BuildStrategyFromScratch -> "from-scratch"
  BuildStrategyInstaller -> "installer"

cacheModeText :: BuildCacheMode -> Text
cacheModeText = \case
  CacheModeMemory -> "memory"
  CacheModeDisk -> "disk"

-- | Filter out the auto-injected runtime env vars so a step's hash
-- doesn't depend on the bake VM's identity or the parent task id —
-- both of which change every invocation and would defeat caching.
stripInjectedEnvs :: [(Text, Text)] -> [(Text, Text)]
stripInjectedEnvs = filter (\(k, _) -> k `notElem` injectedKeys)
  where
    injectedKeys =
      [ "CORVUS_BAKEVM_ID"
      , "CORVUS_BUILD_TASK_ID"
      , "CORVUS_BAKEVM"
      , "CORVUS_BAKEVM_NAME"
      , "CORVUS_BAKEVM_VSOCK_CID"
      ]

--------------------------------------------------------------------------------
-- SHA-256 plumbing
--------------------------------------------------------------------------------

sha256Hex :: ByteString -> Text
sha256Hex bs =
  TE.decodeUtf8 . BAEnc.convertToBase BAEnc.Base16 $ Hash.hashWith Hash.SHA256 bs

-- | sha256(prev_hex || step_hex) — concatenate the two hex strings as
-- ASCII bytes, then hash. Matches the description in the module
-- header.
sha256HexCat :: Text -> Text -> Text
sha256HexCat a b = sha256Hex (TE.encodeUtf8 a `BS.append` TE.encodeUtf8 b)
