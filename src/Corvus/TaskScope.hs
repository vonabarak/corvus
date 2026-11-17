{-# LANGUAGE LambdaCase #-}

-- | Task-tree ownership and emergency VM admission. Cancellation is a request;
-- a scope remains registered until its worker actually leaves the bracket.
module Corvus.TaskScope
  ( TaskRegistry
  , VmRecoveryException (..)
  , newTaskRegistry
  , taskToken
  , taskThread
  , registerTask
  , registerThread
  , finishTask
  , withTaskScope
  , declarePlannedTaskVms
  , associateTaskVms
  , beginVmReset
  , finishVmReset
  , recordVmResetRevision
  , vmResetRevisionMatches
  , confirmVmRevisionDrain
  , withVmResetFinalization
  , restoreVmRecovery
  , confirmVmDrain
  , vmIsRecovering
  , recordTaskNode
  , vmOperationNodes
  , restoreVmNodeHistory
  , taskVms
  , ownsVmReset
  , waitVmTasks
  , waitVmResetDrained
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Exception (Exception, bracket, bracket_)
import Control.Monad (forM_, when)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

-- These are task/VM database identities, not runtime fencing tokens.
data Root = Root (TVar Bool) (Maybe ThreadId) Int Bool (Set.Set Int64)
data Recovery = Recovery Int64 Bool Bool (Maybe Int64)

-- Recovery owner, reset currently executing, nodeagent drain confirmed.
data Registry = Registry (Map.Map Int64 Root) (Map.Map Int64 Recovery)
data TaskRegistry = TaskRegistry (TVar Registry) (TVar (Map.Map Int64 (Maybe (Set.Set Int64))))
newtype VmRecoveryException = VmRecoveryException Text deriving stock (Show)
instance Exception VmRecoveryException

newTaskRegistry :: IO TaskRegistry
newTaskRegistry = TaskRegistry <$> newTVarIO (Registry Map.empty Map.empty) <*> newTVarIO Map.empty

registerTask :: TaskRegistry -> Int64 -> STM (TVar Bool)
registerTask (TaskRegistry var _) root = do
  Registry roots gates <- readTVar var
  case Map.lookup root roots of
    Just (Root token _ _ _ _) -> pure token
    Nothing -> do
      token <- newTVar False
      writeTVar var (Registry (Map.insert root (Root token Nothing 0 False Set.empty) roots) gates)
      pure token

registerThread :: TaskRegistry -> Int64 -> ThreadId -> STM ()
registerThread registry@(TaskRegistry var _) root thread = do
  _ <- registerTask registry root
  Registry roots gates <- readTVar var
  writeTVar var (Registry (Map.adjust (\(Root token _ n done vms) -> Root token (Just thread) n done vms) root roots) gates)

taskToken :: TaskRegistry -> Int64 -> STM (Maybe (TVar Bool))
taskToken (TaskRegistry var _) root = do
  Registry roots _ <- readTVar var
  pure $ (\(Root token _ _ _ _) -> token) <$> Map.lookup root roots

taskThread :: TaskRegistry -> Int64 -> STM (Maybe ThreadId)
taskThread (TaskRegistry var _) root = do
  Registry roots _ <- readTVar var
  pure $ Map.lookup root roots >>= \(Root _ thread _ _ _) -> thread

-- Retain dependencies and cancellation tokens until the last child scope exits.
finishTask :: TaskRegistry -> Int64 -> STM ()
finishTask (TaskRegistry var _) root = do
  Registry roots gates <- readTVar var
  let completed = Map.update (\(Root token thread n _ vms) -> if n == 0 then Nothing else Just (Root token thread n True vms)) root roots
  writeTVar var (settle completed gates)

associateTaskVms :: TaskRegistry -> Int64 -> [Int64] -> STM ()
associateTaskVms registry@(TaskRegistry var _) root vms = do
  _ <- registerTask registry root
  Registry roots gates <- readTVar var
  forM_ vms $ \vm -> case Map.lookup vm gates of
    Just (Recovery owner active _ _) | owner /= root || not active -> throwSTM (VmRecoveryException "VM reset or recovery is in progress")
    _ -> pure ()
  writeTVar var (Registry (Map.adjust (\(Root token thread n done deps) -> Root token thread n done (Set.union deps (Set.fromList vms))) root roots) gates)

withTaskScope :: TaskRegistry -> Int64 -> IO a -> IO a
withTaskScope registry@(TaskRegistry var _) root = bracket_ enter leave
  where
    enter = atomically $ do
      _ <- registerTask registry root
      Registry roots gates <- readTVar var
      writeTVar var (Registry (Map.adjust (\(Root token thread n done deps) -> Root token thread (n + 1) done deps) root roots) gates)
    leave = atomically $ do
      Registry roots gates <- readTVar var
      let remaining = Map.update (\(Root token thread n done deps) -> if n == 1 && done then Nothing else Just (Root token thread (n - 1) done deps)) root roots
      writeTVar var (settle remaining gates)

-- Atomic with dependency registration: work is either cancelled by this reset
-- or rejected at admission. Never wait for a lease or a worker to acknowledge.
beginVmReset :: TaskRegistry -> Int64 -> Int64 -> STM ()
beginVmReset registry@(TaskRegistry var _) root vm = do
  _ <- registerTask registry root
  Registry roots gates <- readTVar var
  case Map.lookup vm gates of
    Just (Recovery _ True _ _) -> throwSTM (VmRecoveryException "A reset is already in progress for this VM")
    _ -> pure ()
  forM_ (Map.toList roots) $ \(other, Root token _ _ _ deps) ->
    when (other /= root && Set.member vm deps) (writeTVar token True)
  writeTVar var (Registry roots (Map.insert vm (Recovery root True False Nothing) gates))

-- Failure leaves a retryable recovery gate. Successful termination alone is
-- insufficient: confirmVmDrain must also observe the nodeagent workers gone.
finishVmReset :: TaskRegistry -> Int64 -> Int64 -> STM ()
finishVmReset (TaskRegistry var _) root vm = do
  Registry roots gates <- readTVar var
  let ended = Map.adjust (\r@(Recovery owner _ drained revision) -> if owner == root then Recovery owner False drained revision else r) vm gates
  writeTVar var (settle roots ended)

confirmVmDrain :: TaskRegistry -> Int64 -> Int64 -> STM ()
confirmVmDrain (TaskRegistry var _) root vm = do
  Registry roots gates <- readTVar var
  let drained = Map.adjust (\r@(Recovery owner active _ revision) -> if owner == root then Recovery owner active True revision else r) vm gates
  writeTVar var (settle roots drained)

vmIsRecovering :: TaskRegistry -> Int64 -> STM Bool
vmIsRecovering (TaskRegistry var _) vm = do
  Registry _ gates <- readTVar var
  pure (Map.member vm gates)

settle :: Map.Map Int64 Root -> Map.Map Int64 Recovery -> Registry
settle roots gates = Registry roots (Map.filterWithKey keep gates)
  where
    keep vm (Recovery owner active drained _) = active || not drained || any (blocks vm owner) (Map.toList roots)
    blocks vm owner (root, Root _ _ _ _ deps) = root /= owner && Set.member vm deps

-- | Snapshot dependencies while retaining the original root cancellation flag.
taskVms :: TaskRegistry -> Int64 -> STM ([Int64], Bool)
taskVms (TaskRegistry var _) root = do
  Registry roots _ <- readTVar var
  case Map.lookup root roots of
    Nothing -> pure ([], True)
    Just (Root token _ _ _ deps) -> do
      cancelled <- readTVar token
      pure (Set.toList deps, cancelled)

ownsVmReset :: TaskRegistry -> Int64 -> Int64 -> STM Bool
ownsVmReset (TaskRegistry var _) root vm = do
  Registry _ gates <- readTVar var
  pure $ case Map.lookup vm gates of
    Just (Recovery owner True _ _) -> owner == root
    _ -> False

-- | Retain recovery even when leases were released but another root is still
-- unwinding cleanup or has an outstanding RPC.
waitVmTasks :: TaskRegistry -> Int64 -> Int64 -> STM ()
waitVmTasks (TaskRegistry var _) owner vm = do
  Registry roots _ <- readTVar var
  check $ all (\(root, Root _ _ _ _ deps) -> root == owner || Set.notMember vm deps) (Map.toList roots)

waitVmResetDrained :: TaskRegistry -> Int64 -> Int64 -> STM ()
waitVmResetDrained registry@(TaskRegistry var _) root vm = do
  Registry _ gates <- readTVar var
  check $ case Map.lookup vm gates of
    Just (Recovery owner _ drained _) -> owner == root && drained
    Nothing -> True
  waitVmTasks registry root vm

-- The existing lifecycle revision also identifies asynchronous drain results.
-- A task tree can reset the same VM more than once; task ownership alone
-- cannot distinguish a late result from the first attempt.
recordVmResetRevision :: TaskRegistry -> Int64 -> Int64 -> Int64 -> STM ()
recordVmResetRevision (TaskRegistry var _) root vm revision = do
  Registry roots gates <- readTVar var
  let record r@(Recovery owner active drained _) =
        if owner == root && active then Recovery owner active drained (Just revision) else r
  writeTVar var (Registry roots (Map.adjust record vm gates))

vmResetRevisionMatches :: TaskRegistry -> Int64 -> Int64 -> Int64 -> STM Bool
vmResetRevisionMatches (TaskRegistry var _) root vm revision = do
  Registry _ gates <- readTVar var
  pure $ case Map.lookup vm gates of
    Just (Recovery owner _ _ recorded) -> owner == root && recorded == Just revision
    Nothing -> False

confirmVmRevisionDrain :: TaskRegistry -> Int64 -> Int64 -> Int64 -> STM ()
confirmVmRevisionDrain registry@(TaskRegistry _ nodesVar) root vm revision = do
  matches <- vmResetRevisionMatches registry root vm revision
  when matches $ do
    confirmVmDrain registry root vm
    modifyTVar' nodesVar (Map.insert vm (Just Set.empty))

-- Reconstructed from the persistent stopping state after daemon restart.
-- Only an explicit reset can replace this unproven recovery gate.
restoreVmRecovery :: TaskRegistry -> Int64 -> STM ()
restoreVmRecovery (TaskRegistry var _) vm = do
  Registry roots gates <- readTVar var
  writeTVar var (Registry roots (Map.insert vm (Recovery 0 False False Nothing) gates))

-- Publish dependencies before a root worker is forked. Execution still checks
-- recovery admission; this declaration itself does not authorize mutation.
declarePlannedTaskVms :: TaskRegistry -> Int64 -> [Int64] -> STM ()
declarePlannedTaskVms registry@(TaskRegistry var _) root vms = do
  _ <- registerTask registry root
  Registry roots gates <- readTVar var
  writeTVar var (Registry (Map.adjust (\(Root token thread n done deps) -> Root token thread n done (Set.union deps (Set.fromList vms))) root roots) gates)

-- Retain touched hosts beyond individual task lifetimes: exported child
-- capabilities can outlive their parent RPC. A reset fences every such host.
recordTaskNode :: TaskRegistry -> Int64 -> Int64 -> STM ()
recordTaskNode registry@(TaskRegistry _ nodesVar) root node = do
  (vms, _) <- taskVms registry root
  modifyTVar' nodesVar $ \nodes -> foldr (\vm -> Map.insertWith (\new old -> Set.union <$> new <*> old) vm (Just (Set.singleton node))) nodes vms

vmOperationNodes :: TaskRegistry -> Int64 -> STM (Maybe [Int64])
vmOperationNodes (TaskRegistry _ nodesVar) vm =
  fmap Set.toList . Map.findWithDefault (Just Set.empty) vm <$> readTVar nodesVar

-- After restart, new dispatches cannot reconstruct hosts holding older caps.
-- Only a successful reset fencing all nodes can establish known history again.
restoreVmNodeHistory :: TaskRegistry -> Int64 -> STM ()
restoreVmNodeHistory (TaskRegistry _ nodesVar) vm =
  modifyTVar' nodesVar (Map.insert vm Nothing)

-- Serialize the final cleanup with explicit reset retries. Preserve an active
-- forced-delete owner's gate until its outer action finishes deleting resources.
withVmResetFinalization :: TaskRegistry -> Int64 -> Int64 -> Int64 -> IO () -> IO ()
withVmResetFinalization registry@(TaskRegistry var _) root vm revision action =
  bracket enter leave $ \case
    Nothing -> pure ()
    Just _ -> action
  where
    enter = atomically $ do
      matches <- vmResetRevisionMatches registry root vm revision
      Registry roots gates <- readTVar var
      case Map.lookup vm gates of
        Just (Recovery owner active drained recorded) | matches -> do
          writeTVar var (Registry roots (Map.insert vm (Recovery owner True drained recorded) gates))
          pure (Just active)
        _ -> pure Nothing
    leave (Just False) = atomically $ do
      matches <- vmResetRevisionMatches registry root vm revision
      when matches $ finishVmReset registry root vm
    leave _ = pure ()
