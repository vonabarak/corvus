{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Corvus.Tls' — covers the pure helpers
-- (filename / CN prefix per role, CN prefix-and-name validation)
-- and the search-path file resolver. The handshake-side helpers
-- ('wrapServerSocket' / 'wrapClientSocket') need real cert
-- material and are exercised end-to-end by the integration test
-- suite instead.
module Corvus.TlsSpec (spec) where

import Corvus.Tls
  ( CertSearchPath (..)
  , TlsRole (..)
  , checkPrefixAndName
  , resolveCertDir
  , roleCNPrefix
  , roleFilename
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.User (getRealUserID)
import Test.Hspec

spec :: Spec
spec = do
  describe "roleFilename / roleCNPrefix" $ do
    it "agrees with the documented filename ↔ CN convention" $ do
      roleFilename RoleDaemon `shouldBe` "corvus-daemon"
      roleFilename RoleNode `shouldBe` "corvus-node"
      roleFilename RoleNetd `shouldBe` "corvus-netd"
      roleFilename RoleClient `shouldBe` "corvus-client"
      roleCNPrefix RoleDaemon `shouldBe` "corvus-daemon:"
      roleCNPrefix RoleNode `shouldBe` "corvus-node:"
      roleCNPrefix RoleNetd `shouldBe` "corvus-netd:"
      roleCNPrefix RoleClient `shouldBe` "corvus-client:"

  describe "checkPrefixAndName" $ do
    it "accepts a CN whose prefix matches the expected role" $
      checkPrefixAndName RoleDaemon Nothing "corvus-daemon:abc123"
        `shouldBe` Right ()
    it "rejects a CN with the wrong role prefix" $
      checkPrefixAndName RoleDaemon Nothing "corvus-client:alice"
        `shouldSatisfy` isLeft
    it "rejects a CN with no prefix at all" $
      checkPrefixAndName RoleDaemon Nothing "corvus-daemon"
        `shouldSatisfy` isLeft
    it "accepts an exact <name> suffix when one is required" $
      checkPrefixAndName RoleNode (Just "alpha") "corvus-node:alpha"
        `shouldBe` Right ()
    it "rejects a mismatched <name> suffix when one is required" $
      checkPrefixAndName RoleNode (Just "alpha") "corvus-node:beta"
        `shouldSatisfy` isLeft
    it "treats an empty suffix as not-a-match when a name was required" $
      checkPrefixAndName RoleNode (Just "alpha") "corvus-node:"
        `shouldSatisfy` isLeft

  describe "resolveCertDir" $ do
    it "returns the first directory that has every required file" $
      withSystemTempDirectory "corvus-tls-spec" $ \root -> do
        let dirA = root </> "a"
            dirB = root </> "b"
        -- dirA is missing 'corvus-daemon.key'; dirB has all three.
        mkDirWithFiles dirA ["ca.crt", "corvus-daemon.crt"]
        mkDirWithFiles dirB ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
        let sp = CertSearchPath [dirA, dirB]
        resolved <-
          resolveCertDir
            sp
            ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
        resolved `shouldBe` Just dirB
    it "returns Nothing when no directory has every required file" $
      withSystemTempDirectory "corvus-tls-spec" $ \root -> do
        let dirA = root </> "only-ca"
        mkDirWithFiles dirA ["ca.crt"]
        let sp = CertSearchPath [dirA]
        resolved <-
          resolveCertDir
            sp
            ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
        resolved `shouldBe` Nothing
    it "prefers the earliest directory in the search path" $
      -- The loader picks whichever directory comes first in the
      -- path *and* has every required file. Default ordering
      -- (see defaultCertSearchPath) puts \$XDG_CONFIG_HOME/corvus
      -- ahead of /etc/corvus so a user-systemd daemon doesn't
      -- get sidetracked by stale system-mode files.
      withSystemTempDirectory "corvus-tls-spec" $ \root -> do
        let dirA = root </> "first"
            dirB = root </> "second"
        mkDirWithFiles dirA ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
        mkDirWithFiles dirB ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
        let sp = CertSearchPath [dirA, dirB]
        resolved <-
          resolveCertDir
            sp
            ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
        resolved `shouldBe` Just dirA
    it "falls through past a directory whose files exist but aren't readable" $
      -- The system-mode → user-mode fallback case: \/etc\/corvus
      -- has a root-owned cert set the user-systemd daemon can't
      -- read; the loader must skip past it instead of erroring.
      -- The test runs as a non-root user; if we ARE root,
      -- chmod 000 still keeps root readable, so skip then.
      withSystemTempDirectory "corvus-tls-spec" $ \root -> do
        uid <- getRealUserID
        if uid == 0
          then pendingWith "permission check is a no-op as root"
          else do
            let dirA = root </> "unreadable"
                dirB = root </> "readable"
            mkDirWithFiles dirA ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
            mkDirWithFiles dirB ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
            setFileMode (dirA </> "corvus-daemon.key") 0o000
            let sp = CertSearchPath [dirA, dirB]
            resolved <-
              resolveCertDir
                sp
                ["ca.crt", "corvus-daemon.crt", "corvus-daemon.key"]
            resolved `shouldBe` Just dirB

-- | Make a directory and touch each named file inside it.
mkDirWithFiles :: FilePath -> [FilePath] -> IO ()
mkDirWithFiles dir names = do
  createDirectoryIfMissing True dir
  mapM_ (\n -> writeFile (dir </> n) "") names

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
