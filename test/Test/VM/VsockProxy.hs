{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Session-scoped selection of a vsock @ProxyCommand@ helper for the
-- test SSH paths. The probe runs once per test daemon startup; the
-- result is cached in a process-global 'IORef' and reused by every
-- 'sshArgsForIO' / 'scpOptsForIO' call in 'Test.VM.Ssh'.
--
-- Lives in its own module to avoid a cycle:
-- @Test.VM.Daemon@ wants to call 'initVsockProxy' at startup, and
-- @Test.VM.Ssh@ depends on @Test.VM.Types@, which already depends on
-- @Test.VM.Daemon@.
module Test.VM.VsockProxy
  ( VsockProxy (..)
  , initVsockProxy
  , getVsockProxy
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory (doesFileExist, findExecutable)
import System.IO.Unsafe (unsafePerformIO)

-- | Resolved vsock proxy. Picked once per test session by
-- 'initVsockProxy' and reused by every SSH/scp call.
data VsockProxy = VsockProxy
  { vpHost :: !(Int -> String)
  -- ^ ssh destination host token for a given CID
  , vpProxyCmd :: !(Int -> String)
  -- ^ ProxyCommand value for a given CID
  , vpExtraOpts :: ![(String, String)]
  -- ^ Extra -o key/value options
  }

vsockProxyRef :: IORef (Maybe VsockProxy)
{-# NOINLINE vsockProxyRef #-}
vsockProxyRef = unsafePerformIO $ newIORef Nothing

systemdSshProxy :: FilePath -> VsockProxy
systemdSshProxy path =
  VsockProxy
    { vpHost = \cid -> "vsock/" ++ show cid
    , vpProxyCmd = \_cid -> path ++ " %h %p"
    , vpExtraOpts = [("ProxyUseFdpass", "yes"), ("CheckHostIP", "no")]
    }

socatVsockProxy :: VsockProxy
socatVsockProxy =
  VsockProxy
    { vpHost = const "vsock"
    , vpProxyCmd = \cid -> "socat - VSOCK-CONNECT:" ++ show cid ++ ":22"
    , vpExtraOpts = [("CheckHostIP", "no")]
    }

-- | Probe the test host for a vsock-capable @ProxyCommand@ helper.
-- Prefers @systemd-ssh-proxy@ (no extra packages on systemd v256+),
-- falls back to @socat@. Fails loud if neither is present so vsock
-- tests do not silently skip the configured transport.
initVsockProxy :: IO ()
initVsockProxy = do
  let candidates = ["/usr/lib/systemd/systemd-ssh-proxy", "/lib/systemd/systemd-ssh-proxy"]
  mSystemd <- firstExisting candidates
  mSocat <- findExecutable "socat"
  proxy <- case (mSystemd, mSocat) of
    (Just path, _) -> pure (systemdSshProxy path)
    (Nothing, Just _) -> pure socatVsockProxy
    (Nothing, Nothing) ->
      fail $
        "vsock-based integration tests require either systemd-ssh-proxy "
          <> "(systemd v256+; expected at /usr/lib/systemd/systemd-ssh-proxy) "
          <> "or socat on PATH; neither was found"
  writeIORef vsockProxyRef (Just proxy)
  where
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
      e <- doesFileExist p
      if e then pure (Just p) else firstExisting ps

-- | Read the resolved vsock proxy. Fails if 'initVsockProxy' has
-- not run yet — call sites should never reach here outside a test
-- daemon session.
getVsockProxy :: IO VsockProxy
getVsockProxy = do
  m <- readIORef vsockProxyRef
  case m of
    Just p -> pure p
    Nothing ->
      fail
        "vsock proxy not initialized; call initVsockProxy in the test daemon setup"
