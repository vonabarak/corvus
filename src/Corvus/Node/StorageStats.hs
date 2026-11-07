{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bounded, read-only filesystem observations with transition diagnostics.
module Corvus.Node.StorageStats (StorageStatsReader, newStorageStatsReader, readStorageStats, parseStorageStats, existingStoragePath) where

import Control.Exception (IOException, try)
import Control.Monad.Logger (LogLevel (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getFileStatus, isDirectory)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

newtype StorageStatsReader = StorageStatsReader (IORef (Map.Map FilePath Bool))

newStorageStatsReader :: IO StorageStatsReader
newStorageStatsReader = StorageStatsReader <$> newIORef Map.empty

-- | Only ENOENT permits walking upward; permissions and other errors remain failures.
existingStoragePath :: FilePath -> IO FilePath
existingStoragePath path = do
  result <- try (getFileStatus path)
  case result of
    Right status | isDirectory status -> pure path
    Right _ -> ioError (userError "storage path is not a directory")
    Left (err :: IOException)
      | isDoesNotExistError err && takeDirectory path /= path -> existingStoragePath (takeDirectory path)
      | otherwise -> ioError err

parseStorageStats :: String -> Maybe (Int64, Int64)
parseStorageStats body = case lines body of
  [_, row] -> case words row of
    [totalText, freeText] -> do
      total <- readMaybe totalText :: Maybe Integer
      free <- readMaybe freeText :: Maybe Integer
      if total > 0 && total <= toInteger (maxBound :: Int64) && free >= 0 && free <= total
        then Just (fromInteger total, fromInteger free)
        else Nothing
    _ -> Nothing
  _ -> Nothing

readStorageStats :: StorageStatsReader -> (LogLevel -> Text -> IO ()) -> FilePath -> IO (Int64, Int64)
readStorageStats (StorageStatsReader state) logger path = do
  result <- try $ timeout 5000000 $ do
    observed <- existingStoragePath path
    (code, output, _) <- readProcessWithExitCode "df" ["--output=size,avail", "-B1", "--", observed] ""
    if observed == path then pure () else logger LevelDebug ("storage_stats path=" <> T.pack path <> " ancestor=" <> T.pack observed)
    pure $ if code == ExitSuccess then parseStorageStats output else Nothing
  let sample = case result of
        Right (Just (Just value)) -> Just value
        _ -> Nothing
      failed = case sample of Nothing -> True; Just _ -> False
      reason = case result of
        Left (_ :: IOException) -> "io_error"
        Right Nothing -> "timeout"
        _ -> "invalid_or_failed_df"
  prior <- atomicModifyIORef' state $ \states -> (Map.insert path failed states, Map.lookup path states)
  case sample of
    Nothing -> logger (if prior == Just True then LevelDebug else LevelWarn) ("storage_stats unavailable path=" <> T.pack path <> " reason=" <> reason)
    Just _ | prior == Just True -> logger LevelInfo ("storage_stats recovered path=" <> T.pack path)
    _ -> pure ()
  pure $ fromMaybe (0, 0) sample
