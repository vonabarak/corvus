-- | Atomic, fair leases for database identities and image resources.
module Corvus.OperationGuard
  ( OperationGuard
  , ObjectKind (..)
  , OperationResource (..)
  , ResourceAccess (..)
  , OperationLease
  , OperationLeaseTimeout (..)
  , newOperationGuard
  , withOperationLeases
  , withOperationLeasesWithBudget
  ) where

import Control.Concurrent.STM
import Control.Exception (Exception, bracket, mask, onException, throwIO)
import Control.Monad (when)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data ObjectKind = TemplateObject | SshKeyObject | VmObject | NetworkObject | NodeObject
  deriving stock (Eq, Ord, Show)

data OperationResource
  = ImageVersion Int64
  | ImageFamily Text
  | ObjectName ObjectKind (Maybe Int64) Text
  | ObjectId ObjectKind Int64
  | NodeEndpoint Text Int
  | ChildIdentity Text Int64 Text
  deriving stock (Eq, Ord, Show)
data ResourceAccess = SharedAccess | ExclusiveAccess
  deriving stock (Eq, Ord, Show)
data OperationLease = OperationLease Int (Map.Map OperationResource ResourceAccess)
data Entry = Entry Int Text (Map.Map OperationResource ResourceAccess)
data Registry = Registry Int [Entry] [Entry]
newtype OperationGuard = OperationGuard (TVar Registry)
newtype OperationLeaseTimeout = OperationLeaseTimeout Text
instance Show OperationLeaseTimeout where
  show (OperationLeaseTimeout message) = T.unpack message
instance Exception OperationLeaseTimeout

newOperationGuard :: IO OperationGuard
newOperationGuard = OperationGuard <$> newTVarIO (Registry 0 [] [])

conflicts :: Map.Map OperationResource ResourceAccess -> Map.Map OperationResource ResourceAccess -> Bool
conflicts a b = or $ Map.intersectionWith (\x y -> x == ExclusiveAccess || y == ExclusiveAccess) a b

-- | The deadline covers the complete set. Earlier conflicting waiters have
-- priority, while disjoint operations can proceed. Inherited leases are explicit
-- capabilities; sharing a task id alone never bypasses contention.
withOperationLeases :: OperationGuard -> Int -> Text -> [OperationLease] -> [(OperationResource, ResourceAccess)] -> ([OperationLease] -> IO a) -> IO a
withOperationLeases guard timeoutMicros = withOperationLeasesWithBudget guard timeoutMicros timeoutMicros

-- | Keep the original operation budget in diagnostics when preparation or a
-- stale-resource retry has already consumed part of it. Both durations are in
-- microseconds; only the remaining duration controls the actual wait.
withOperationLeasesWithBudget :: OperationGuard -> Int -> Int -> Text -> [OperationLease] -> [(OperationResource, ResourceAccess)] -> ([OperationLease] -> IO a) -> IO a
withOperationLeasesWithBudget (OperationGuard var) budgetMicros remainingMicros owner inherited resources operation = mask $ \restore -> do
  Registry _ live _ <- readTVarIO var
  let liveIds = [i | Entry i _ _ <- live]
      validInherited = [lease | lease@(OperationLease i _) <- inherited, i `elem` liveIds]
      held = Map.unionsWith max [rs | OperationLease _ rs <- validInherited]
      requested = Map.filterWithKey (\key mode -> maybe True (< mode) (Map.lookup key held)) (Map.fromListWith max resources)
      inheritedIds = [i | OperationLease i _ <- validInherited]
  when (any (\(key, mode) -> Map.lookup key held == Just SharedAccess && mode == ExclusiveAccess) (Map.toList requested)) $
    throwIO (OperationLeaseTimeout "Cannot upgrade a shared resource lease; request exclusive access together with the initial resources")
  if Map.null requested
    then restore (operation validInherited)
    else do
      expired <- registerDelay remainingMicros
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
                  queued = any (\(Entry _ _ rs) -> conflicts requested rs && not (conflicts held rs)) earlier
              elapsed <- readTVar expired
              if elapsed
                then pure $ Just $ "Timed out after " <> T.pack (show (budgetMicros `div` 1000000)) <> " seconds waiting for " <> T.intercalate ", " [describe mode key | (key, mode) <- Map.toList requested] <> "; held by " <> T.intercalate ", " (if null blockers then ["an earlier waiting task"] else blockers)
                else
                  if null blockers && not queued
                    then do
                      writeTVar var (Registry next (Entry ident owner requested : active) (filter (\(Entry i _ _) -> i /= ident) waiting))
                      pure Nothing
                    else retry

            maybe (pure ()) (throwIO . OperationLeaseTimeout) outcome
      bracket (acquire `onException` remove) (const remove) $ \() -> restore (operation (OperationLease ident requested : validInherited))

describe :: ResourceAccess -> OperationResource -> Text
describe mode resource =
  (if mode == ExclusiveAccess then "exclusive access to " else "shared access to ") <> case resource of
    ImageVersion key -> "image version (ID " <> T.pack (show key) <> ")"
    ImageFamily name -> "image family '" <> name <> "'"
    ObjectName kind scope name -> T.pack (show kind) <> " name '" <> name <> "'" <> maybe "" (\node -> " on node " <> T.pack (show node)) scope
    ObjectId kind key -> T.pack (show kind) <> " ID " <> T.pack (show key)
    NodeEndpoint host port -> "node endpoint " <> host <> ":" <> T.pack (show port)
    ChildIdentity kind parent key -> kind <> " on " <> T.pack (show parent) <> ": " <> key
