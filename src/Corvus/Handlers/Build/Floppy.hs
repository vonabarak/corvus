{-# LANGUAGE OverloadedStrings #-}

-- | Build a 1.44 MB FAT12 floppy image containing a single file.
--
-- Used by the @installer@ build strategy to attach an autounattend
-- (Windows), kickstart (RHEL/Alma), preseed (Debian), or
-- @setup-alpine@ answers file to the bake VM. The floppy interface
-- is the only universally-discovered "removable disk" Windows Setup
-- looks at for @autounattend.xml@; for other installers a CDROM is
-- often equivalent, but a floppy is the lowest-common-denominator
-- and what every example we ship uses.
--
-- Shells out to @mkfs.fat@ (from @dosfstools@) and @mcopy@ (from
-- @mtools@); both are universally available on Linux and required
-- on the daemon host.
module Corvus.Handlers.Build.Floppy
  ( buildFloppyImage
  )
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)

-- | Create a 1.44 MB FAT12 image at @dest@ holding a single file
-- @filename@ with the supplied raw @bytes@. Overwrites @dest@ if it
-- exists. Returns @Right ()@ on success or @Left stderr@ if either
-- @mkfs.fat@ or @mcopy@ fails.
buildFloppyImage
  :: FilePath
  -- ^ destination .img path
  -> Text
  -- ^ filename on the floppy (e.g. @autounattend.xml@)
  -> BS.ByteString
  -- ^ raw file bytes
  -> IO (Either Text ())
buildFloppyImage dest filename bytes = do
  -- mkfs.fat -C <file> <kbytes>  creates AND formats the image.
  (rc1, _, err1) <- readProcessWithExitCode "mkfs.fat" ["-C", dest, "1440"] ""
  case rc1 of
    ExitFailure c ->
      pure $
        Left $
          "mkfs.fat exited "
            <> T.pack (show c)
            <> ": "
            <> T.pack err1
    ExitSuccess ->
      withSystemTempFile "corvus-floppy-" $ \tmp h -> do
        BS.hPut h bytes
        hClose h
        let dst = "::" ++ T.unpack filename
        (rc2, _, err2) <- readProcessWithExitCode "mcopy" ["-i", dest, tmp, dst] ""
        pure $ case rc2 of
          ExitSuccess -> Right ()
          ExitFailure c ->
            Left $
              "mcopy exited "
                <> T.pack (show c)
                <> ": "
                <> T.pack err2
