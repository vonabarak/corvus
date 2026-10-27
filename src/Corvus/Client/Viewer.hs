{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Discover native SPICE viewers and manage their private connection files.
module Corvus.Client.Viewer
  ( ViewerLaunch (..)
  , prepareViewer
  , runViewer
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Corvus.Client.Config (ClientConfig (..))
import Corvus.Client.Types (ViewOptions (..), Viewer (..))
import Corvus.Wire.Common (ViewGrant (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Either (fromRight)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import System.Directory (doesDirectoryExist, findExecutable, makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hClose, hFlush, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Posix.Files (setFileMode)
import System.Process (CreateProcess (..), callProcess, proc, readCreateProcessWithExitCode)
import System.Random (randomIO)
import System.Timeout (timeout)

-- | A viewer whose executable and optional absolute folder have been validated.
data ViewerLaunch = ViewerLaunch !Viewer !FilePath !(Maybe FilePath) ![String]
  deriving stock (Eq, Show)

-- | Discover before requesting a short-lived grant. Explicit choices never fall
-- back, and server-only/structured-output callers do not invoke this function.
prepareViewer :: ClientConfig -> ViewOptions -> IO (Either Text ViewerLaunch)
prepareViewer config options = do
  result <- try @IOException $ do
    selected <- case voViewer options of
      Just viewer -> lookupViewer viewer
      Nothing -> do
        virtViewer <- lookupViewer VirtViewer
        case virtViewer of
          Just found -> pure (Just found)
          Nothing -> lookupViewer Remmina
    case selected of
      Nothing -> pure . Left $ case voViewer options of
        Just Remmina -> "Remmina is not installed. Install remmina with SPICE and encryption support, or use --viewer virt-viewer."
        Just VirtViewer -> "remote-viewer is not installed. Install virt-viewer with SPICE support, or use --viewer remmina."
        Nothing -> "No SPICE viewer found. Install remmina with SPICE and encryption support or virt-viewer with SPICE support."
      Just (viewer, executable) -> do
        folder <- traverse makeAbsolute (voShareFolder options)
        valid <- maybe (pure True) doesDirectoryExist folder
        pure $ if valid then Right (ViewerLaunch viewer executable folder (voViewerArgs options)) else Left "--share-folder must name an existing client-local directory."
  pure $ fromRight (Left "Unable to inspect the viewer executable or shared folder.") result
  where
    lookupViewer viewer = do
      executable <- findExecutable (case viewer of Remmina -> ccRemmina config; VirtViewer -> ccRemoteViewer config)
      pure (fmap (viewer,) executable)

-- | Keep credentials in a private temporary file for the entire viewer lifetime.
-- IOException deliberately excludes asynchronous cancellation: the bracketed
-- temporary file is still cleaned up and the interruption propagates normally.
runViewer :: ViewerLaunch -> Text -> ViewGrant -> IO Bool
runViewer (ViewerLaunch viewer executable folder extraArgs) vmName grant = do
  result <- try @IOException $ case viewer of
    VirtViewer -> do
      let args = maybe [] (\path -> ["--spice-shared-dir=" ++ path]) folder
      when (isJust folder) $ do
        TIO.hPutStrLn stderr "In virt-viewer, open File → Preferences and enable Share folder to activate the selected folder."
        hFlush stderr
      withProfile "corvus-spice-.vv" (virtViewerProfile grant) $ \path -> callProcess executable (args ++ extraArgs ++ [path])
    Remmina -> do
      token <- show <$> (randomIO :: IO Word64)
      let appId = "org.corvus.Viewer.c" ++ token
      password <- encryptPassword executable (appId ++ ".encrypt") (vgPassword grant)
      withProfile "corvus-spice-.remmina" (remminaProfile vmName grant password folder) $ \path ->
        callProcess executable (["--gapplication-app-id", appId ++ ".view", "--no-tray-icon"] ++ extraArgs ++ ["--connect", path])
  case result of
    Left _ -> do
      TIO.hPutStrLn stderr $ case viewer of
        VirtViewer -> "Failed to run remote-viewer. Check its installation and SPICE support."
        Remmina -> "Failed to run Remmina. Check its installation, SPICE and encryption support, and graphical display."
      pure False
    Right () -> pure True

withProfile :: String -> Text -> (FilePath -> IO a) -> IO a
withProfile template body action = withSystemTempFile template $ \path handle -> do
  setFileMode path 0o600
  TIO.hPutStr handle body
  hClose handle
  action path

virtViewerProfile :: ViewGrant -> Text
virtViewerProfile grant =
  T.unlines
    [ "[virt-viewer]"
    , "type=spice"
    , "host=" <> escapeIni (vgHost grant)
    , "port=" <> T.pack (show (vgPort grant))
    , "password=" <> escapeIni (vgPassword grant)
    ]

remminaProfile :: Text -> ViewGrant -> Text -> Maybe FilePath -> Text
remminaProfile vmName grant password folder =
  T.unlines $
    [ "[remmina]"
    , "protocol=SPICE"
    , "name=" <> escapeIni vmName
    , "server=" <> escapeIni endpoint
    , "password=" <> password
    , "enableaudio=1"
    , "disableclipboard=0"
    , "scale=2"
    , "disablepasswordstoring=1"
    ]
      ++ maybe [] (\path -> ["sharefolder=" <> escapeIni (T.pack path)]) folder
  where
    host = vgHost grant
    endpoint = (if ":" `T.isInfixOf` host && not ("[" `T.isPrefixOf` host) then "[" <> host <> "]" else host) <> ":" <> T.pack (show (vgPort grant))

-- | GLib GKeyFile escaping for string values, including leading whitespace.
escapeIni :: Text -> Text
escapeIni = T.concatMap $ \char -> case char of
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  ' ' -> "\\s"
  _ -> T.singleton char

-- | Remmina's encryption command prints usage examples as well as its result,
-- and 1.4.43 exits with status 1 even on success. Never expose captured output:
-- it includes encrypted credentials, which are reusable by the same account.
encryptPassword :: FilePath -> String -> Text -> IO Text
encryptPassword executable appId password = do
  environment <- getEnvironment
  result <-
    timeout 10000000 $
      readCreateProcessWithExitCode
        ( (proc executable ["--gapplication-app-id", appId, "--no-tray-icon", "--encrypt-password"])
            { env = Just (("LC_ALL", "C") : filter ((/= "LC_ALL") . fst) environment)
            }
        )
        (T.unpack password ++ "\n")
  case result of
    Just (code, output, _) -> do
      unless (code == ExitSuccess || code == ExitFailure 1) (ioError (userError "Remmina encryption failed"))
      case mapMaybe (T.stripPrefix "Encrypted password: ") (T.lines (T.pack output)) of
        [encrypted] -> case Base64.decode (TE.encodeUtf8 encrypted) of
          Right bytes | not (BS.null bytes) && BS.length bytes `mod` 8 == 0 -> pure encrypted
          _ -> invalid
        _ -> invalid
    Nothing -> ioError (userError "Remmina encryption timed out")
  where
    invalid = ioError (userError "Invalid Remmina encryption result")
