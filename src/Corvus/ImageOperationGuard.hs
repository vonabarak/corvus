-- | Atomic, fair leases for image versions and publication families.
module Corvus.ImageOperationGuard
  ( ImageOperationGuard
  , ImageResource (..)
  , ImageAccess (..)
  , ImageLease
  , ImageLeaseTimeout (..)
  , newImageOperationGuard
  , withImageLeases
  ) where

import Control.Concurrent.STM
import Control.Exception (Exception, bracket, mask, onException, throwIO)
import Control.Monad (when)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data ImageResource = ImageVersion Int64 | ImageFamily Text
  deriving stock (Eq, Ord, Show)
data ImageAccess = SharedImage | ExclusiveImage
  deriving stock (Eq, Ord, Show)
data ImageLease = ImageLease Int (Map.Map ImageResource ImageAccess)
data Entry = Entry Int Text (Map.Map ImageResource ImageAccess)
data Registry = Registry Int [Entry] [Entry]
newtype ImageOperationGuard = ImageOperationGuard (TVar Registry)
newtype ImageLeaseTimeout = ImageLeaseTimeout Text
instance Show ImageLeaseTimeout where
  show (ImageLeaseTimeout message) = T.unpack message
instance Exception ImageLeaseTimeout

newImageOperationGuard :: IO ImageOperationGuard
newImageOperationGuard = ImageOperationGuard <$> newTVarIO (Registry 0 [] [])

conflicts :: Map.Map ImageResource ImageAccess -> Map.Map ImageResource ImageAccess -> Bool
conflicts a b = or $ Map.intersectionWith (\x y -> x == ExclusiveImage || y == ExclusiveImage) a b

-- | The deadline covers the complete set. Earlier conflicting waiters have
-- priority, while disjoint operations can proceed. Inherited leases are explicit
-- capabilities; sharing a task id alone never bypasses contention.
withImageLeases :: ImageOperationGuard -> Int -> Text -> [ImageLease] -> [(ImageResource, ImageAccess)] -> ([ImageLease] -> IO a) -> IO a
withImageLeases (ImageOperationGuard var) timeoutMicros owner inherited resources operation = mask $ \restore -> do
  Registry _ live _ <- readTVarIO var
  let liveIds = [i | Entry i _ _ <- live]
      validInherited = [lease | lease@(ImageLease i _) <- inherited, i `elem` liveIds]
      held = Map.unionsWith max [rs | ImageLease _ rs <- validInherited]
      requested = Map.filterWithKey (\key mode -> maybe True (< mode) (Map.lookup key held)) (Map.fromListWith max resources)
      inheritedIds = [i | ImageLease i _ <- validInherited]
  when (any (\(key, mode) -> Map.lookup key held == Just SharedImage && mode == ExclusiveImage) (Map.toList requested)) $
    throwIO (ImageLeaseTimeout "Cannot upgrade a shared image lease; request exclusive access together with the initial resources")
  if Map.null requested
    then restore (operation validInherited)
    else do
      expired <- registerDelay timeoutMicros
      ident <- atomically $ do
        Registry next active waiting <- readTVar var
        writeTVar var (Registry (next + 1) active (waiting ++ [Entry next owner requested]))
        pure next
      let remove = atomically $ do
            Registry next active waiting <- readTVar var
            writeTVar var (Registry next (filter (\(Entry i _ _) -> i /= ident) active) (filter (\(Entry i _ _) -> i /= ident) waiting))
          acquire = do
            outcome <- atomically $ do
              Registry next active waiting <- readTVar var
              let earlier = takeWhile (\(Entry i _ _) -> i /= ident) waiting
                  blockers = [label | Entry i label rs <- active, i `notElem` inheritedIds, conflicts requested rs]
                  queued = any (\(Entry _ _ rs) -> conflicts requested rs) earlier
              elapsed <- readTVar expired
              if elapsed
                then pure $ Just $ "Timed out after " <> T.pack (show (timeoutMicros `div` 1000000)) <> " seconds waiting for " <> T.intercalate ", " [describe mode key | (key, mode) <- Map.toList requested] <> "; held by " <> T.intercalate ", " (if null blockers then ["an earlier waiting task"] else blockers)
                else
                  if null blockers && not queued
                    then do
                      writeTVar var (Registry next (Entry ident owner requested : active) (filter (\(Entry i _ _) -> i /= ident) waiting))
                      pure Nothing
                    else retry

            maybe (pure ()) (throwIO . ImageLeaseTimeout) outcome
      bracket (acquire `onException` remove) (const remove) $ \() -> restore (operation (ImageLease ident requested : validInherited))

describe :: ImageAccess -> ImageResource -> Text
describe mode resource =
  (if mode == ExclusiveImage then "exclusive access to " else "shared access to ") <> case resource of
    ImageVersion key -> "image version (ID " <> T.pack (show key) <> ")"
    ImageFamily name -> "image family '" <> name <> "'"
