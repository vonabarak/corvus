{-# LANGUAGE ScopedTypeVariables #-}

-- | Client-local destination selection and screenshot output.
module Corvus.Client.Commands.Screenshot (handleVmScreenshot) where

import Control.Exception (IOException, SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import Corvus.Client.Capnp.Rpc (rpcVmScreenshot)
import Corvus.Client.Output (emitOkWith, emitRpcError)
import Corvus.Client.Types (OutputFormat)
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, (</>))
import System.Process (readProcessWithExitCode)
import System.Random (randomIO)

handleVmScreenshot :: OutputFormat -> CapnpConnection -> Text -> Maybe FilePath -> IO Bool
handleVmScreenshot fmt conn vm requested = do
  result <- try $ do
    destination <- maybe (defaultDestination vm) pure requested >>= makeAbsolute
    count <- rpcVmScreenshot conn (entityRefFromText vm) destination
    pure (destination, count)
  case result of
    Right (destination, count) -> do
      emitOkWith fmt [("path", toJSON destination), ("bytes_transferred", toJSON count)] $
        putStrLn ("Screenshot saved to " <> destination)
      pure True
    Left (err :: SomeException) -> do
      emitRpcError fmt err $ putStrLn ("Screenshot failed: " <> show err)
      pure False

defaultDestination :: Text -> IO FilePath
defaultDestination vm = do
  home <- getHomeDirectory
  xdg <- try (readProcessWithExitCode "xdg-user-dir" ["PICTURES"] "") :: IO (Either IOException (ExitCode, String, String))
  let directory = case xdg of
        Right (ExitSuccess, output, _) | let path = T.unpack (T.strip (T.pack output)), isAbsolute path -> path
        _ -> home </> "Pictures"
      safe = T.unpack $ T.map (\c -> if isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_" :: String) then c else '_') vm
  createDirectoryIfMissing True directory
  now <- getCurrentTime
  suffix <- randomIO :: IO Word64
  pure (directory </> safe <> "-" <> formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now <> "-" <> show suffix <> ".png")
