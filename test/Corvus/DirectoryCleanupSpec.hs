module Corvus.DirectoryCleanupSpec (spec) where

import Corvus.Node.DirectoryCleanup (removeDirectoryIfEmpty)
import System.Directory (createDirectory, createDirectoryLink, doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "removeDirectoryIfEmpty" $ do
  it "removes only the empty immediate directory" $
    withSystemTempDirectory "corvus-cleanup" $ \parent -> do
      let child = parent </> "vm"
      createDirectory child
      removeDirectoryIfEmpty [] child
      doesDirectoryExist child `shouldReturn` False
      doesDirectoryExist parent `shouldReturn` True

  it "tolerates a missing directory" $
    withSystemTempDirectory "corvus-cleanup" $ \parent ->
      removeDirectoryIfEmpty [] (parent </> "missing")

  it "preserves ordinary and hidden files" $
    withSystemTempDirectory "corvus-cleanup" $ \parent -> do
      let ordinary = parent </> "state.qemu.zst"
          hidden = parent </> ".state"
      writeFile ordinary "saved memory"
      writeFile hidden "state"
      removeDirectoryIfEmpty [] parent
      doesFileExist ordinary `shouldReturn` True
      doesFileExist hidden `shouldReturn` True

  it "preserves empty subdirectories such as TPM state directories" $
    withSystemTempDirectory "corvus-cleanup" $ \parent -> do
      let child = parent </> "tpm2"
      createDirectory child
      removeDirectoryIfEmpty [] parent
      doesDirectoryExist child `shouldReturn` True

  it "preserves directory symlinks and their targets" $
    withSystemTempDirectory "corvus-cleanup" $ \parent -> do
      let target = parent </> "target"
          link = parent </> "link"
      createDirectory target
      createDirectoryLink target link
      removeDirectoryIfEmpty [] (link ++ "/")
      pathIsSymbolicLink link `shouldReturn` True
      doesDirectoryExist target `shouldReturn` True

  it "preserves a protected root supplied through a symlink alias" $
    withSystemTempDirectory "corvus-cleanup" $ \parent -> do
      let target = parent </> "storage"
          link = parent </> "alias"
      createDirectory target
      createDirectoryLink target link
      removeDirectoryIfEmpty [link ++ "/"] (target ++ "/")
      doesDirectoryExist target `shouldReturn` True

  it "preserves the filesystem root" $
    removeDirectoryIfEmpty [] "/"

  it "reports unexpected filesystem errors" $
    withSystemTempDirectory "corvus-cleanup" $ \parent -> do
      let file = parent </> "disk"
      writeFile file "disk"
      removeDirectoryIfEmpty [] file `shouldThrow` anyIOException
