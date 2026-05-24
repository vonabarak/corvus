{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mutual-TLS plumbing for every TCP Cap'n Proto link the daemon,
-- the agents, and the CLI client speak. Unix-socket connections
-- continue to rely on filesystem permissions and never enter this
-- module.
--
-- Filename / CN convention (see @doc/security.md@):
--
-- * @ca.crt@                                    — root CA cert, shared
-- * @corvus-daemon.{crt,key}@                   — CN @corvus-daemon:<uuid>@
-- * @corvus-node.{crt,key}@                     — CN @corvus-node:<name>@
-- * @corvus-netd.{crt,key}@                     — CN @corvus-netd:<name>@
-- * @corvus-client.{crt,key}@                   — CN @corvus-client:<name>@
--
-- Cert files are searched first under @$XDG_CONFIG_HOME/corvus/@
-- (default @~/.config/corvus/@), then under @/etc/corvus/@. The
-- user-dir-first ordering lets a user-systemd daemon load its own
-- certs even when @/etc/corvus@ holds an unrelated (and possibly
-- unreadable) system-mode install. A directory whose cert files
-- exist but can't be read by the current user is skipped so the
-- search falls through cleanly. Client certs are user-owned, so
-- the CLI's resolver skips @/etc/corvus@ entirely.
--
-- After every TLS handshake the receiving side reads the peer's CN
-- and validates the prefix (and, for daemon→agent links, the node
-- name suffix). Wrong prefix = connection closed before any RPC
-- dispatch — this is what makes "stole a node's key, used it to
-- impersonate a CLI client" cryptographically impossible.
module Corvus.Tls
  ( -- * Roles + CN convention
    TlsRole (..)
  , roleFilename
  , roleCNPrefix

    -- * Search-path resolution
  , CertSearchPath (..)
  , defaultCertSearchPath
  , clientCertSearchPath
  , resolveCertDir

    -- * Configuration
  , TlsConfig (..)
  , TlsLoadError (..)
  , loadTlsConfig
  , withPeerExpectation

    -- * Connection wrapping
  , PeerCNRef
  , wrapServerSocket
  , wrapClientSocket
  , tlsTransport
  , closeTlsContext
  , readPeerCNRef

    -- * Validation
  , validatePeerCN
  , validatePeerCNAnyRole
  , checkPrefixAndName
  , peerNameFromCN
  )
where

import qualified Capnp.Bits as CapnpBits
import qualified Capnp.Convert as Conv
import qualified Capnp.Message as CM
import qualified Capnp.Rpc.Transport as CRT
import qualified Capnp.TraversalLimit as CTL
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import qualified Control.Exception as E
import Control.Monad.Trans.Class (lift)
import Data.ASN1.Types (asn1CharacterToString)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Word (Word32)
import qualified Data.X509 as X509
import qualified Data.X509.CertificateStore as X509Store
import qualified Data.X509.Validation as X509Val
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLSC
import System.Directory (Permissions, doesFileExist, getPermissions, readable)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Error (eofErrorType, mkIOError)

-- ---------------------------------------------------------------------------
-- Roles + CN convention

-- | One of the four cert-bearing roles in a Corvus deployment.
-- The filename for each role's cert pair is exactly the role's
-- 'roleFilename' string with @.crt@ / @.key@ appended, and the
-- CN in the cert starts with @roleFilename + ":"@.
data TlsRole
  = RoleDaemon
  | RoleNode
  | RoleNetd
  | RoleClient
  deriving (Eq, Show)

roleFilename :: TlsRole -> T.Text
roleFilename RoleDaemon = "corvus-daemon"
roleFilename RoleNode = "corvus-node"
roleFilename RoleNetd = "corvus-netd"
roleFilename RoleClient = "corvus-client"

-- | The CN prefix (everything up to and including the colon) for a
-- role. The remainder of the CN is the daemon UUID, the node
-- name, or the client name.
roleCNPrefix :: TlsRole -> T.Text
roleCNPrefix r = roleFilename r <> ":"

-- ---------------------------------------------------------------------------
-- Search path

-- | A resolved cert-file search path. The actual filesystem
-- lookups are done lazily by 'resolveCertDir' / 'loadTlsConfig'
-- so callers can carry the @CertSearchPath@ around without doing
-- I/O.
newtype CertSearchPath = CertSearchPath {certSearchDirs :: [FilePath]}
  deriving (Eq, Show)

-- | Default search path for every component except the CLI:
-- @$XDG_CONFIG_HOME/corvus/@ (default @~/.config/corvus/@), then
-- @/etc/corvus/@. User-dir-first so a user-systemd daemon doesn't
-- get derailed by stale or unreadable files in @/etc/corvus@.
defaultCertSearchPath :: IO CertSearchPath
defaultCertSearchPath = do
  xdg <- resolveXdgConfigDir
  pure (CertSearchPath [xdg, "/etc/corvus"])

-- | Client-side search path. Client certs are user-owned and live
-- only under @$XDG_CONFIG_HOME/corvus/@; @/etc/corvus/@ is
-- skipped to keep cert ownership unambiguous when several admins
-- share a workstation.
clientCertSearchPath :: IO CertSearchPath
clientCertSearchPath = do
  xdg <- resolveXdgConfigDir
  pure (CertSearchPath [xdg])

resolveXdgConfigDir :: IO FilePath
resolveXdgConfigDir = do
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  case mXdg of
    Just p | not (null p) -> pure (p </> "corvus")
    _ -> do
      mHome <- lookupEnv "HOME"
      case mHome of
        Just h | not (null h) -> pure (h </> ".config" </> "corvus")
        _ -> pure ("/.config" </> "corvus")

-- | Walk a 'CertSearchPath' and return the first directory in
-- which every requested filename exists AND is readable by the
-- current process. A directory whose files exist but are mode-
-- denied is treated as a miss so the search falls through to the
-- next candidate — that's what lets a user-systemd daemon recover
-- when @/etc/corvus@ holds a root-owned cert set it can't open.
-- The caller's diagnostic in the 'Nothing' branch should mention
-- every path tried.
resolveCertDir :: CertSearchPath -> [FilePath] -> IO (Maybe FilePath)
resolveCertDir (CertSearchPath dirs) needed = go dirs
  where
    go [] = pure Nothing
    go (d : rest) = do
      oks <- mapM (isAccessible . (d </>)) needed
      if and oks
        then pure (Just d)
        else go rest
    isAccessible f = do
      ex <- doesFileExist f
      if not ex
        then pure False
        else do
          eperms <- E.try (getPermissions f) :: IO (Either E.IOException Permissions)
          pure $ case eperms of
            Right p -> readable p
            Left _ -> False

-- ---------------------------------------------------------------------------
-- Configuration

-- | Loaded TLS material plus everything the wrapping side needs
-- to validate the peer. Lives for as long as the listener / dial
-- loop that produced it; sockets get their own throwaway
-- 'TLS.Context'.
data TlsConfig = TlsConfig
  { tcOwnRole :: !TlsRole
  -- ^ Which slot the local component is filling (the CN prefix
  -- of the cert we present to the peer).
  , tcOwnCN :: !T.Text
  -- ^ Full CN read out of the loaded cert. Kept for log lines
  -- and for the unit tests; not consulted on every accept.
  , tcCertDir :: !FilePath
  -- ^ The directory the cert/key/ca trio was loaded from.
  -- Reported in startup logs so operators see which file won
  -- when both @$XDG_CONFIG_HOME/corvus@ and @/etc/corvus@ have
  -- entries (the user dir wins).
  , tcCredentials :: !TLS.Credentials
  -- ^ Own cert chain + private key, packed for 'TLS.Shared'.
  , tcCAStore :: !X509Store.CertificateStore
  -- ^ Trust anchors used to verify the peer's chain.
  , tcExpectedPeerPrefix :: !TlsRole
  -- ^ The CN prefix the peer MUST present. Mismatch closes the
  -- connection before any RPC frame is exchanged.
  , tcExpectedPeerName :: !(Maybe T.Text)
  -- ^ For daemon→agent links: the @<name>@ portion of the peer's
  -- CN must equal this. Set 'Nothing' for the CLI-server cases
  -- where any client of the right kind is acceptable.
  }

-- | Things that can go wrong while resolving / loading TLS
-- material. Surface to the operator with both the candidate
-- paths and a pointer at @corvus-admin@.
data TlsLoadError
  = -- | Required filenames not found anywhere in the search path.
    -- The first list is the filenames; the second is the dirs
    -- searched.
    TlsFilesNotFound !T.Text ![FilePath] ![FilePath]
  | -- | @credentialLoadX509@ returned 'Left'.
    TlsCertParseFailed !T.Text
  | -- | 'readCertificateStore' returned 'Nothing' or an empty
    -- store for @ca.crt@.
    TlsCaParseFailed !T.Text
  | -- | The CN in the loaded cert (first) doesn't start with the
    -- expected prefix (second).
    TlsOwnCNMismatch !T.Text !T.Text
  | -- | A cert in the chain didn't have a CN at all. The
    -- 'T.Text' names which file.
    TlsCertMissingCN !T.Text
  deriving (Eq, Show)

instance E.Exception TlsLoadError

-- | Resolve the cert directory for the given role and load every
-- file we need. Caller passes a search path (typically
-- 'defaultCertSearchPath' or 'clientCertSearchPath'); the caller
-- supplies the peer expectations directly because they depend on
-- which endpoint we're standing up.
loadTlsConfig
  :: CertSearchPath
  -> TlsRole
  -- ^ own role (determines which cert filenames to load and
  -- which CN prefix the loaded cert must carry)
  -> TlsRole
  -- ^ expected peer role (the CN prefix the peer must present)
  -> Maybe T.Text
  -- ^ expected peer @<name>@, when the peer's CN is fully
  -- determined (daemon→agent links)
  -> IO (Either TlsLoadError TlsConfig)
loadTlsConfig sp ownRole peerRole mPeerName = do
  let certName = T.unpack (roleFilename ownRole) <> ".crt"
      keyName = T.unpack (roleFilename ownRole) <> ".key"
      caName = "ca.crt"
      needed = [caName, certName, keyName]
  mDir <- resolveCertDir sp needed
  case mDir of
    Nothing ->
      pure (Left (TlsFilesNotFound (roleFilename ownRole) needed (certSearchDirs sp)))
    Just dir -> do
      eCred <- TLS.credentialLoadX509 (dir </> certName) (dir </> keyName)
      case eCred of
        Left e -> pure (Left (TlsCertParseFailed (T.pack e)))
        Right cred@(chain, _key) -> do
          mStore <- X509Store.readCertificateStore (dir </> caName)
          case mStore of
            Nothing ->
              pure (Left (TlsCaParseFailed (T.pack (dir </> caName))))
            Just store ->
              case extractCNFromChain chain of
                Nothing ->
                  pure (Left (TlsCertMissingCN (T.pack (dir </> certName))))
                Just cn ->
                  if roleCNPrefix ownRole `T.isPrefixOf` cn
                    then
                      pure
                        ( Right
                            TlsConfig
                              { tcOwnRole = ownRole
                              , tcOwnCN = cn
                              , tcCertDir = dir
                              , tcCredentials = TLS.Credentials [cred]
                              , tcCAStore = store
                              , tcExpectedPeerPrefix = peerRole
                              , tcExpectedPeerName = mPeerName
                              }
                        )
                    else
                      pure (Left (TlsOwnCNMismatch cn (roleCNPrefix ownRole)))

-- | Return a new 'TlsConfig' with the peer expectations replaced.
-- The own-role / credentials / CA store stay the same. Used by
-- the daemon's per-node supervisor when reusing the daemon's
-- credentials to dial a specific nodeagent / netd peer.
withPeerExpectation :: TlsRole -> Maybe T.Text -> TlsConfig -> TlsConfig
withPeerExpectation peerRole mPeerName cfg =
  cfg
    { tcExpectedPeerPrefix = peerRole
    , tcExpectedPeerName = mPeerName
    }

-- ---------------------------------------------------------------------------
-- Wrapping a socket
--
-- After every successful handshake the peer's CN is captured into
-- the returned IORef so callers can read it back via 'peerCN' /
-- 'validatePeerCN'. Storing it in the context itself isn't
-- enough on the client side: TLS 2.x doesn't expose
-- @getServerCertificateChain@, so we capture it in the validation
-- hook.

-- | The captured-peer-CN handle returned by the socket-wrap
-- functions. Read via 'peerCN' / 'validatePeerCN'.
newtype PeerCNRef = PeerCNRef (IORef (Maybe T.Text))

newPeerCNRef :: IO PeerCNRef
newPeerCNRef = PeerCNRef <$> newIORef Nothing

readPeerCNRef :: PeerCNRef -> IO (Maybe T.Text)
readPeerCNRef (PeerCNRef ref) = readIORef ref

-- | Strip the role's CN prefix and return the @<name>@ suffix.
-- For a CN of @corvus-client:alice@ with 'RoleClient' this returns
-- @alice@. If the CN doesn't actually start with the role's prefix
-- the full CN is returned unchanged — the caller has already
-- validated the prefix at handshake time, so the input is trusted.
peerNameFromCN :: TlsRole -> T.Text -> T.Text
peerNameFromCN role cn =
  let prefix = roleCNPrefix role
   in if prefix `T.isPrefixOf` cn
        then T.drop (T.length prefix) cn
        else cn

writePeerCNRef :: PeerCNRef -> T.Text -> IO ()
writePeerCNRef (PeerCNRef ref) cn = writeIORef ref (Just cn)

-- | Run the server-side TLS handshake on an accepted socket. The
-- returned 'TLS.Context' is in the post-handshake state; the
-- peer's CN is also captured in the IORef so 'peerCN' /
-- 'validatePeerCN' can read it back. The caller is responsible
-- for closing the context (via 'closeTlsContext') and the
-- underlying socket.
wrapServerSocket :: TlsConfig -> NS.Socket -> IO (TLS.Context, PeerCNRef)
wrapServerSocket cfg sock = do
  ref <- newPeerCNRef
  ctx <- TLS.contextNew sock (serverParams cfg ref)
  TLS.handshake ctx
  pure (ctx, ref)

-- | Run the client-side TLS handshake on a freshly @connect()@'d
-- socket. Symmetric to 'wrapServerSocket'.
wrapClientSocket :: TlsConfig -> NS.Socket -> IO (TLS.Context, PeerCNRef)
wrapClientSocket cfg sock = do
  ref <- newPeerCNRef
  ctx <- TLS.contextNew sock (clientParams cfg ref)
  TLS.handshake ctx
  pure (ctx, ref)

-- | Send TLS @close_notify@ and tear down the context. Idempotent
-- on already-closed contexts: any failure is swallowed because
-- the caller is already closing the underlying socket.
closeTlsContext :: TLS.Context -> IO ()
closeTlsContext ctx =
  E.handle (\(_ :: E.SomeException) -> pure ()) (TLS.bye ctx)

-- ---------------------------------------------------------------------------
-- TLS parameters

serverParams :: TlsConfig -> PeerCNRef -> TLS.ServerParams
serverParams cfg ref =
  def
    { TLS.serverWantClientCert = True
    , TLS.serverCACertificates =
        X509Store.listCertificates (tcCAStore cfg)
    , TLS.serverShared =
        def
          { TLS.sharedCredentials = tcCredentials cfg
          , TLS.sharedCAStore = tcCAStore cfg
          }
    , TLS.serverHooks =
        def
          { TLS.onClientCertificate = onClientCert cfg ref
          }
    , TLS.serverSupported = corvusSupported
    }

clientParams :: TlsConfig -> PeerCNRef -> TLS.ClientParams
clientParams cfg ref =
  (TLS.defaultParamsClient "" "")
    { TLS.clientShared =
        def
          { TLS.sharedCredentials = tcCredentials cfg
          , TLS.sharedCAStore = tcCAStore cfg
          }
    , TLS.clientHooks =
        def
          { -- Always present our credential when the server asks.
            -- The default would return Nothing.
            TLS.onCertificateRequest = \_ -> pure (firstCredential cfg)
          , TLS.onServerCertificate = onServerCert cfg ref
          }
    , TLS.clientSupported = corvusSupported
    , -- We don't run HTTPS; we authenticate via CN-prefix
      -- matching after the handshake, not via hostname.
      TLS.clientUseServerNameIndication = False
    }

firstCredential :: TlsConfig -> Maybe (X509.CertificateChain, X509.PrivKey)
firstCredential cfg = case tcCredentials cfg of
  TLS.Credentials (c : _) -> Just c
  _ -> Nothing

corvusSupported :: TLS.Supported
corvusSupported =
  def
    { TLS.supportedVersions = [TLS.TLS13, TLS.TLS12]
    , TLS.supportedCiphers = TLSC.ciphersuite_default
    }

onClientCert
  :: TlsConfig
  -> PeerCNRef
  -> X509.CertificateChain
  -> IO TLS.CertificateUsage
onClientCert cfg ref chain = do
  reasons <-
    X509Val.validate
      X509.HashSHA256
      X509Val.defaultHooks
      relaxedChecks
      (tcCAStore cfg)
      (TLS.exceptionValidationCache [])
      ("", "")
      chain
  case reasons of
    [] -> case extractCNFromChain chain of
      Just cn -> do
        writePeerCNRef ref cn
        pure TLS.CertificateUsageAccept
      Nothing ->
        pure
          ( TLS.CertificateUsageReject
              (TLS.CertificateRejectOther "peer cert has no CN")
          )
    rs ->
      pure
        ( TLS.CertificateUsageReject
            (TLS.CertificateRejectOther (show rs))
        )

onServerCert
  :: TlsConfig
  -> PeerCNRef
  -> X509Store.CertificateStore
  -> X509Val.ValidationCache
  -> X509Val.ServiceID
  -> X509.CertificateChain
  -> IO [X509Val.FailedReason]
onServerCert cfg ref _ vcache _ chain = do
  reasons <-
    X509Val.validate
      X509.HashSHA256
      X509Val.defaultHooks
      relaxedChecks
      (tcCAStore cfg)
      vcache
      ("", "")
      chain
  case reasons of
    [] -> case extractCNFromChain chain of
      Just cn -> do
        writePeerCNRef ref cn
        pure []
      Nothing ->
        pure [X509Val.NameMismatch "peer cert has no CN"]
    rs -> pure rs

-- | We do our own subject-name validation via CN prefix matching,
-- so we relax the FQHN check the default hooks would apply
-- (they expect the cert SAN to match a hostname, which we don't
-- thread through). Time and chain-validity checks remain on.
relaxedChecks :: X509Val.ValidationChecks
relaxedChecks = def {X509Val.checkFQHN = False}

-- ---------------------------------------------------------------------------
-- Peer-CN extraction + validation

-- | Pull the CN out of the leaf certificate in a chain, decoded as
-- UTF-8 (the @asn1@ library already takes care of charset
-- decoding). Returns 'Nothing' when the leaf has no CN element.
extractCNFromChain :: X509.CertificateChain -> Maybe T.Text
extractCNFromChain (X509.CertificateChain []) = Nothing
extractCNFromChain (X509.CertificateChain (signed : _)) =
  extractCNFromCert (X509.getCertificate signed)

-- | Pull the CN out of a parsed 'X509.Certificate'.
extractCNFromCert :: X509.Certificate -> Maybe T.Text
extractCNFromCert cert = do
  cs <-
    X509.getDnElement
      X509.DnCommonName
      (X509.certSubjectDN cert)
  T.pack <$> asn1CharacterToString cs

-- | Check a CN against the prefix + optional exact-name expectation
-- the receiving side wants. Returns 'Left' with a
-- human-readable diagnostic on failure.
checkPrefixAndName :: TlsRole -> Maybe T.Text -> T.Text -> Either T.Text ()
checkPrefixAndName expectedRole mExpectedName cn =
  let prefix = roleCNPrefix expectedRole
   in if not (prefix `T.isPrefixOf` cn)
        then
          Left
            ( "peer CN "
                <> cn
                <> " does not start with required prefix "
                <> prefix
            )
        else case mExpectedName of
          Nothing -> Right ()
          Just expected ->
            let suffix = T.drop (T.length prefix) cn
             in if suffix == expected
                  then Right ()
                  else
                    Left
                      ( "peer CN "
                          <> cn
                          <> " has name "
                          <> suffix
                          <> ", expected "
                          <> expected
                      )

-- | Validate the captured peer CN against the config. Returns
-- 'Left' with a diagnostic when the CN is missing, doesn't carry
-- the right prefix, or carries the wrong @<name>@ suffix.
validatePeerCN :: TlsConfig -> PeerCNRef -> IO (Either T.Text ())
validatePeerCN cfg ref = do
  mCN <- readPeerCNRef ref
  pure $ case mCN of
    Nothing -> Left "no peer CN captured after handshake"
    Just cn ->
      checkPrefixAndName
        (tcExpectedPeerPrefix cfg)
        (tcExpectedPeerName cfg)
        cn

-- | Like 'validatePeerCN' but accepts any of the listed roles.
-- The CN must start with one of the role prefixes; the peer-name
-- suffix is ignored (used by the nodeagent server, which accepts
-- both the orchestrating daemon and other agents during
-- inter-agent disk transfer).
validatePeerCNAnyRole :: [TlsRole] -> PeerCNRef -> IO (Either T.Text ())
validatePeerCNAnyRole roles ref = do
  mCN <- readPeerCNRef ref
  pure $ case mCN of
    Nothing -> Left "no peer CN captured after handshake"
    Just cn ->
      let prefixes = map roleCNPrefix roles
       in if any (`T.isPrefixOf` cn) prefixes
            then Right ()
            else
              Left
                ( "peer CN "
                    <> cn
                    <> " does not match any accepted prefix: "
                    <> T.intercalate ", " prefixes
                )

-- ---------------------------------------------------------------------------
-- Cap'n Proto transport adapter
--
-- 'CRT.Transport' is just a pair of (send, recv) callbacks
-- exchanging @'CM.Message' 'CM.Const@. We rebuild it on top of a
-- TLS context by sending whole messages as one lazy bytestring
-- and driving 'CM.readMessage' with our own 4-byte little-endian
-- word32 reader and segment-bytes reader. Surplus bytes between
-- 'TLS.recvData' chunks live in a per-connection MVar buffer.

-- | Build a Cap'n Proto 'CRT.Transport' that reads/writes over a
-- live TLS context. The limit is the same word-count cap
-- @socketTransport defaultLimit@ uses for plain TCP.
tlsTransport :: TLS.Context -> CapnpBits.WordCount -> IO CRT.Transport
tlsTransport ctx limit = do
  buf <- newMVar BS.empty
  pure
    CRT.Transport
      { CRT.sendMsg = TLS.sendData ctx . Conv.msgToLBS
      , CRT.recvMsg =
          CTL.evalLimitT limit $
            CM.readMessage
              (lift (read32 ctx buf))
              (lift . readSegment ctx buf)
      }

read32 :: TLS.Context -> MVar BS.ByteString -> IO Word32
read32 ctx buf = do
  bs <- readExactly ctx buf 4
  pure $!
    (fromIntegral (BS.index bs 0) `shiftL` 0)
      .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
      .|. (fromIntegral (BS.index bs 2) `shiftL` 16)
      .|. (fromIntegral (BS.index bs 3) `shiftL` 24)

readSegment
  :: TLS.Context
  -> MVar BS.ByteString
  -> CapnpBits.WordCount
  -> IO (CM.Segment 'CM.Const)
readSegment ctx buf words_ = do
  let nbytes = fromIntegral (CapnpBits.wordsToBytes words_) :: Int
  bs <- readExactly ctx buf nbytes
  pure (CM.fromByteString bs)

-- | Pull exactly @n@ bytes off the TLS context, buffering any
-- residual chunk for the next call.
--
-- EOF is interpreted in two ways:
--
-- * Clean close at a message boundary (no buffered or partial bytes
--   accumulated yet) — throws an 'IOError' of 'eofErrorType' so
--   callers (and our connection log) can recognise it as a normal
--   end-of-session.
-- * EOF mid-message (partial bytes already read) — throws a
--   'userError' describing the truncation; this is an actual
--   protocol error worth surfacing.
readExactly :: TLS.Context -> MVar BS.ByteString -> Int -> IO BS.ByteString
readExactly ctx buf n = do
  stash <- readMVar buf
  go stash
  where
    go !acc
      | BS.length acc >= n = do
          let (out, rest) = BS.splitAt n acc
          modifyMVar_ buf (\_ -> pure rest)
          pure out
      | otherwise = do
          chunk <- TLS.recvData ctx
          if BS.null chunk
            then
              E.throwIO $
                if BS.null acc
                  then mkIOError eofErrorType "Corvus.Tls.readExactly" Nothing Nothing
                  else
                    userError $
                      "Corvus.Tls.readExactly: TLS peer closed before "
                        <> show n
                        <> " bytes were available"
            else go (acc <> chunk)
