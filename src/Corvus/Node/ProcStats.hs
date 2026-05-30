{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Host-side observation helpers used by 'Corvus.Node.StatusPoller'
-- to populate 'VmStats':
--
--   * '/proc/<pid>/stat' for cumulative vCPU thread time;
--   * '/proc/<pid>/status' for the QEMU process's host RSS;
--   * '/sys/class/net/<tap>/statistics/{rx_bytes,tx_bytes}' for
--     per-TAP network throughput.
--
-- Each helper returns 'Nothing' when the underlying file does
-- not exist or fails to parse, so a transient race (the process
-- exited between the pid check and the read; the TAP was torn
-- down) just drops the sample without crashing the poller.
module Corvus.Node.ProcStats
  ( -- * Process sample (/proc/\<pid\>/{stat,status})
    ProcSample (..)
  , readProcSample

    -- * Network interface sample (/sys/class/net/\<iface\>/...)
  , TapSample (..)
  , readTapSample

    -- * Clock tick rate
  , getClkTck

    -- * Parser exports (for unit tests)
  , parseUtimeStime
  , parseVmRss
  )
where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import System.Posix.Unistd (SysVar (ClockTick), getSysVar)
import Text.Read (readMaybe)

-- | Cumulative process counters captured from /proc.
data ProcSample = ProcSample
  { psCpuJiffies :: !Word64
  -- ^ user + system jiffies from /proc/\<pid\>/stat fields 14+15.
  -- Convert to seconds via 'getClkTck'.
  , psRssBytes :: !Word64
  -- ^ VmRSS from /proc/\<pid\>/status, normalised to bytes.
  }
  deriving (Eq, Show)

-- | Sample CPU jiffies + host RSS for the given pid. Returns
-- 'Nothing' if either /proc file is missing or unparseable
-- (typically: the process exited between the pid check and the
-- read).
readProcSample :: Word32 -> IO (Maybe ProcSample)
readProcSample pid = do
  mStat <- safeReadFile ("/proc/" <> show pid <> "/stat")
  mStatus <- safeReadFile ("/proc/" <> show pid <> "/status")
  pure $ do
    statBs <- mStat
    statusBs <- mStatus
    jiff <- parseUtimeStime statBs
    rssKb <- parseVmRss statusBs
    pure
      ProcSample
        { psCpuJiffies = jiff
        , psRssBytes = rssKb * 1024
        }

-- | Parse @utime + stime@ (jiffies) from /proc/\<pid\>/stat.
--
-- The comm field (process name) can contain spaces and
-- parentheses, so we slice the line after the LAST @)@ and
-- tokenise from there. After the closing paren the tokens are
-- (man proc): state(0) ppid(1) pgrp(2) session(3) tty_nr(4)
-- tpgid(5) flags(6) minflt(7) cminflt(8) majflt(9) cmajflt(10)
-- utime(11) stime(12) … — so utime is at index 11 and stime
-- at index 12 of the post-paren tokens.
parseUtimeStime :: BS.ByteString -> Maybe Word64
parseUtimeStime bs = do
  i <- BSC.elemIndexEnd ')' bs
  let toks = BSC.words (BSC.drop (i + 1) bs)
  utimeBs <- safeIndex 11 toks
  stimeBs <- safeIndex 12 toks
  utime <- readWord utimeBs
  stime <- readWord stimeBs
  pure (utime + stime)

-- | Parse @VmRSS:@ from /proc/\<pid\>/status. Returns the value
-- in kB (the unit Linux always reports here), or 'Nothing' if
-- the line is absent.
parseVmRss :: BS.ByteString -> Maybe Word64
parseVmRss bs = go (BSC.lines bs)
  where
    go [] = Nothing
    go (line : rest) = case BSC.stripPrefix "VmRSS:" line of
      Just tail_ -> case BSC.words tail_ of
        (num : _) -> readWord num
        [] -> Nothing
      Nothing -> go rest

-- | One read of /sys/class/net/\<iface\>/statistics/{rx,tx}_bytes.
data TapSample = TapSample
  { tsRxBytes :: !Word64
  , tsTxBytes :: !Word64
  }
  deriving (Eq, Show)

-- | Sample cumulative rx/tx byte counters for a host network
-- device. Returns 'Nothing' if the sysfs path does not exist
-- (TAP gone, or a virtio-net @user@ netif with no host device).
readTapSample :: T.Text -> IO (Maybe TapSample)
readTapSample iface
  | T.null iface = pure Nothing
  | otherwise = do
      let base = "/sys/class/net/" <> T.unpack iface <> "/statistics/"
      mRx <- safeReadFile (base <> "rx_bytes")
      mTx <- safeReadFile (base <> "tx_bytes")
      pure $ do
        rxBs <- mRx
        txBs <- mTx
        rx <- readWord (BSC.takeWhile (/= '\n') rxBs)
        tx <- readWord (BSC.takeWhile (/= '\n') txBs)
        pure TapSample {tsRxBytes = rx, tsTxBytes = tx}

-- | Read @_SC_CLK_TCK@ (typically 100). The value is constant for
-- the agent's lifetime, but the syscall is cheap enough that
-- callers may invoke this once per tick rather than caching.
getClkTck :: IO Word32
getClkTck = do
  n <- getSysVar ClockTick
  pure (fromInteger n)

-- ---------------------------------------------------------------------------
-- Internal helpers

safeReadFile :: FilePath -> IO (Maybe BS.ByteString)
safeReadFile p =
  E.handle (\(_ :: E.IOException) -> pure Nothing) (Just <$> BS.readFile p)

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i < 0 = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

readWord :: BS.ByteString -> Maybe Word64
readWord bs = readMaybe (BSC.unpack bs)
