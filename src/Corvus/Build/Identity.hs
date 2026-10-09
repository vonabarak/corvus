{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Canonical identities for published build artifacts.
module Corvus.Build.Identity (buildInputIdentity) where

import Corvus.Model (EnumText (..))
import Corvus.Schema.Build
import qualified Crypto.Hash as Hash
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Bifunctor
import qualified Data.ByteArray.Encoding as BAEnc
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Public surface
--------------------------------------------------------------------------------

-- | Identity of the final artifact, independent of cleanup policy and
-- allocated template IDs. Source image IDs remain: every published version
-- is immutable, and moving a floating tag must invalidate dependent builds.
-- SSH key material is supplied by the same database read as the template.
buildInputIdentity :: Build -> [Text] -> BuildIdentity
buildInputIdentity b publicKeys =
  let envelope = case envelopeValue b of
        Object fields -> Object $ KM.delete "template" $ KM.insert "resolvedTemplate" template fields
        value -> value
      template = case toJSON (buildResolvedTemplate b) of
        Object fields ->
          Object $
            KM.insert "ssh_keys" (toJSON (L.sort publicKeys)) $
              KM.mapWithKey normalizeChild $
                foldr KM.delete fields ["id", "name", "description", "created_at"]
        value -> value
      normalizeChild "drives" (Array drives) = Array (V.map normalizeDrive drives)
      normalizeChild "shared_dirs" (Array dirs) = Array (V.map (without ["id"]) dirs)
      normalizeChild "audio_devices" (Array devices) = Array (V.map (without ["id"]) devices)
      normalizeChild _ value = value
      normalizeDrive (Object fields) =
        Object $ KM.mapWithKey (\key value -> if key == "disk_image" then without ["name"] value else value) $ KM.delete "disk_selector" $ KM.delete "disk_name" fields
      normalizeDrive value = value
      without keys (Object fields) = Object (foldr KM.delete fields keys)
      without _ value = value
      manifest =
        obj
          [ ("version", intValue (1 :: Int))
          , ("envelope", envelope)
          , ("compact", Bool (btCompact (buildTarget b)))
          , ("provisioners", toJSON (map provisionerValue (buildProvisioners b)))
          ]
      bytes = LBS.toStrict $ Encoding.encodingToLazyByteString $ canonicalJson manifest
   in BuildIdentity (sha256Hex bytes) (TE.decodeUtf8 bytes)

-- | Sort every object's keys explicitly, rather than relying on KeyMap's
-- implementation order. Arrays retain their semantic order.
canonicalJson :: Value -> Encoding.Encoding
canonicalJson (Object fields) = Encoding.pairs $ foldMap (\(key, value) -> Encoding.pair key (canonicalJson value)) (L.sortOn fst (KM.toList fields))
canonicalJson (Array values) = Encoding.list canonicalJson (V.toList values)
canonicalJson value = Encoding.value value

--------------------------------------------------------------------------------
-- Canonical Aeson values
--------------------------------------------------------------------------------

-- | Build envelope. Environment keys are canonicalized; ordered inputs
-- such as boot keys retain their execution order.
envelopeValue :: Build -> Value
envelopeValue b =
  obj
    [ ("template", String (buildTemplate b))
    , ("resolvedTemplate", toJSON (buildResolvedTemplate b))
    , ("target", targetValue (buildTarget b))
    , ("strategy", String (strategyText (buildStrategy b)))
    , ("vm", buildVmValue (buildVm b))
    , ("shellDefaults", shellDefaultsValue (buildShellDefaults b))
    , ("bootKeys", Array (V.fromList (map bootKeyValue (buildBootKeys b))))
    ]

-- | Only the target fields that affect what the BAKE produces go
-- into the hash. The bake VM gets a target disk created with the
-- target's @format@ (qcow2 vs raw changes the QEMU drive driver
-- and snapshot support) and @size@ (the disk's virtual size),
-- so those affect bake behaviour. Everything else is operator
-- policy applied to the FINAL published artifact AFTER the bake
-- completes:
--
--   * @path@      — where on disk to publish the cloned artifact
--   * @compact@   — whether to @qemu-img -c@ the published clone
--   * @ifExists@  — what to do when an artifact with the same name
--                   already exists (error / skip / overwrite)
targetValue :: BuildTarget -> Value
targetValue t =
  obj
    [ ("format", String (enumToText (btFormat t)))
    , ("size", intValue (btSize t))
    ]

buildVmValue :: BuildVm -> Value
buildVmValue v =
  obj
    [ ("cpuCount", intValue (bvmCpuCount v))
    , ("ram", intValue (bvmRam v))
    ]

shellDefaultsValue :: ShellDefaults -> Value
shellDefaultsValue sd =
  obj
    [ ("preamble", maybe Null String (sdPreamble sd))
    , ("env", envObject (stripInjectedEnvs (sdEnv sd)))
    ]

bootKeyValue :: BootKey -> Value
bootKeyValue bk =
  obj
    [ ("keys", String (bkKeys bk))
    , ("delaySec", intValue (bkDelaySec bk))
    , ("repeat", intValue (bkRepeat bk))
    , ("intervalSec", intValue (bkIntervalSec bk))
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
  -- the identity.
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
-- equivalent reorderings retain the same identity.
envObject :: [(Text, Text)] -> Value
envObject = obj . map (Data.Bifunctor.second String) . L.sortOn fst

intValue :: (Integral a) => a -> Value
intValue = Number . fromIntegral

strategyText :: BuildStrategy -> Text
strategyText = \case
  BuildStrategyOverlay -> "overlay"
  BuildStrategyFromScratch -> "from-scratch"
  BuildStrategyInstaller -> "installer"

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
