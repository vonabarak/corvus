{-# LANGUAGE OverloadedStrings #-}

-- | Namespace manager for running dnsmasq in an isolated network namespace
-- connected to a VDE switch. Uses Linux user namespaces for unprivileged
-- operation — no root or doas required.
module Corvus.Qemu.Vde.Namespace
  ( startNamespaceManager
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.Posix.Types (CPid (..), ProcessID)

foreign import ccall safe "corvus_vdens_start"
  c_corvus_vdens_start
    :: CString
    -> CString
    -> CString
    -> CInt
    -> CString
    -> CString
    -> CString
    -> CString
    -> CString
    -> Ptr CPid
    -> IO CInt

-- | Start a namespace manager process that runs dnsmasq in an isolated
-- network namespace connected to a VDE switch via a TAP interface.
--
-- The C code forks internally to create a single-threaded child process
-- that is safe for unshare(CLONE_NEWUSER). The child:
-- 1. Creates user + network + UTS namespaces via unshare(2)
-- 2. Sets ambient network capabilities (CAP_NET_ADMIN, etc.)
-- 3. Opens a TAP interface and connects it to the VDE switch
-- 4. Configures the TAP with the gateway IP
-- 5. Forks and execs dnsmasq
-- 6. Bridges packets between VDE and TAP until dnsmasq exits
--
-- Returns the PID of the namespace manager process.
startNamespaceManager
  :: FilePath
  -- ^ VDE switch socket path
  -> FilePath
  -- ^ dnsmasq binary path
  -> FilePath
  -- ^ Lease file path
  -> Text
  -- ^ Gateway address
  -> Text
  -- ^ Prefix length
  -> Text
  -- ^ DHCP range start
  -> Text
  -- ^ DHCP range end
  -> Text
  -- ^ Subnet mask
  -> IO (Either Text ProcessID)
startNamespaceManager vdeSock dnsmasqBin leaseFile gwAddr prefixLen rangeStart rangeEnd mask = do
  let prefixInt = read (T.unpack prefixLen) :: Int
  withCString vdeSock $ \cVdeSock ->
    withCString "vde0" $ \cTapName ->
      withCString (T.unpack gwAddr) $ \cGwAddr ->
        withCString (T.unpack rangeStart) $ \cRangeStart ->
          withCString (T.unpack rangeEnd) $ \cRangeEnd ->
            withCString (T.unpack mask) $ \cMask ->
              withCString dnsmasqBin $ \cDnsmasq ->
                withCString leaseFile $ \cLeaseFile ->
                  alloca $ \pidPtr -> do
                    rv <-
                      c_corvus_vdens_start
                        cVdeSock
                        cTapName
                        cGwAddr
                        (fromIntegral prefixInt)
                        cRangeStart
                        cRangeEnd
                        cMask
                        cDnsmasq
                        cLeaseFile
                        pidPtr
                    if rv == 0
                      then do
                        (CPid pid) <- peek pidPtr
                        pure $ Right (fromIntegral pid)
                      else pure $ Left "Namespace manager child failed to start"
