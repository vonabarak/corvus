{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Per-node observation pack: CPU count, RAM, free disk, load
-- average, kernel release, agent version. The 'StatusPoller'
-- attaches a 'C.Parsed CGNA.NodeStats' to every snapshot so the
-- daemon can stamp the values into the 'Node' row's stat
-- columns + bump 'nodeAgentHealthcheck'.
--
-- Reads are best-effort: any /proc parse failure surfaces as a
-- zero / empty sentinel rather than aborting the whole poll
-- cycle. The agent always has *some* values to attach.
module Corvus.Node.NodeStats
  ( readNodeStats
  , agentVersion
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Nodeagent as CGNA
import Control.Exception (SomeException, try)
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Development.GitRev (gitHash)
import System.Process (readProcess)

-- | Read the current node observation. Never throws — all parse
-- failures land in the matching field's "unknown" sentinel.
readNodeStats :: QemuConfig -> IO (C.Parsed CGNA.NodeStats)
readNodeStats cfg = do
  basePath <- getEffectiveBasePath cfg
  cpuCount <- readCpuCount
  (ramTotal, ramFree) <- readMemInfo
  (storTotal, storFree) <- readStatvfs basePath
  (l1, l5, l15) <- readLoadAvg
  kernel <- readKernelRelease
  pure
    CGNA.NodeStats
      { CGNA.cpuCount = cpuCount
      , CGNA.ramMbTotal = ramTotal
      , CGNA.ramMbFree = ramFree
      , CGNA.storageBytesTotal = storTotal
      , CGNA.storageBytesFree = storFree
      , CGNA.loadAvg1 = l1
      , CGNA.loadAvg5 = l5
      , CGNA.loadAvg15 = l15
      , CGNA.kernelRelease = kernel
      , CGNA.agentVersion = agentVersion
      }

-- | Count online logical CPUs by reading "processor" lines in
-- @/proc/cpuinfo@. Falls back to 0 on read failure.
readCpuCount :: IO Int32
readCpuCount = do
  r <- try (readFile "/proc/cpuinfo") :: IO (Either SomeException String)
  pure $ case r of
    Right body ->
      fromIntegral $
        length [ln | ln <- lines body, "processor" `isPrefix` ln]
    Left _ -> 0
  where
    isPrefix p s = take (length p) s == p

-- | Parse @MemTotal@ and @MemAvailable@ from @/proc/meminfo@,
-- converting from KiB (linux's unit) to MiB. Returns @(0, 0)@
-- on any failure.
readMemInfo :: IO (Int32, Int32)
readMemInfo = do
  r <- try (readFile "/proc/meminfo") :: IO (Either SomeException String)
  pure $ case r of
    Right body ->
      let ls = lines body
          total = lookupKb "MemTotal:" ls
          avail = lookupKb "MemAvailable:" ls
       in (kbToMb total, kbToMb avail)
    Left _ -> (0, 0)
  where
    kbToMb kb = fromIntegral (kb `div` 1024) :: Int32
    -- Each line is "Key:    <num> kB". 'words' is fine here —
    -- 'reads' on the numeric token tolerates an empty parse
    -- without crashing the agent.
    lookupKb key ls = case [w | l <- ls, key `isPrefix` l, w <- take 1 (drop 1 (words l))] of
      [s] -> case reads s of
        [(n :: Integer, _)] -> n
        _ -> 0
      _ -> 0
    isPrefix p s = take (length p) s == p

-- | Run @df --output=size,avail -B1 <basePath>@ to get the
-- filesystem totals (bytes). statvfs(2) would be cleaner but
-- pulls in additional FFI work; @df@ is on every host the agent
-- runs on.
readStatvfs :: FilePath -> IO (Int64, Int64)
readStatvfs basePath = do
  r <-
    try
      (readProcess "df" ["--output=size,avail", "-B1", basePath] "")
      :: IO (Either SomeException String)
  pure $ case r of
    Right body -> case lines body of
      (_ : datal : _) -> case words datal of
        (totS : avS : _) ->
          ( readInt64 totS
          , readInt64 avS
          )
        _ -> (0, 0)
      _ -> (0, 0)
    Left _ -> (0, 0)
  where
    readInt64 s = case reads s of
      [(n :: Integer, _)] -> fromIntegral n
      _ -> 0

-- | Read /proc/loadavg ("1m 5m 15m running/total lastPid").
readLoadAvg :: IO (Double, Double, Double)
readLoadAvg = do
  r <- try (readFile "/proc/loadavg") :: IO (Either SomeException String)
  pure $ case r of
    Right body -> case words body of
      (a : b : c : _) -> (parseD a, parseD b, parseD c)
      _ -> (0, 0, 0)
    Left _ -> (0, 0, 0)
  where
    parseD s = case reads s of
      [(d :: Double, _)] -> d
      _ -> 0

-- | @uname -r@ via the standard syscall.
readKernelRelease :: IO T.Text
readKernelRelease = do
  r <- try (readProcess "uname" ["-r"] "") :: IO (Either SomeException String)
  pure $ case r of
    Right body -> T.strip (T.pack body)
    Left _ -> ""

-- | Baked-in agent version. The leading short git hash makes
-- daemon-side version checks (`Node.agentVersion`) actionable
-- across builds.
agentVersion :: T.Text
agentVersion = T.take 8 (T.pack $(gitHash))
