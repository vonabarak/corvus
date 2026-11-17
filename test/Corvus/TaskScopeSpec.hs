module Corvus.TaskScopeSpec (spec) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Exception (try)
import Corvus.TaskScope
import Data.Either (isLeft)
import Test.Hspec

spec :: Spec
spec = describe "VM task-tree admission" $ do
  it "preserves an existing cancellation token on repeated root registration" $ do
    registry <- newTaskRegistry
    token <- atomically $ registerTask registry 1
    atomically $ writeTVar token True
    again <- atomically $ registerTask registry 1
    readTVarIO again `shouldReturn` True
    thread <- myThreadId
    atomically $ registerThread registry 1 thread
    atomically (taskThread registry 1) `shouldReturn` Just thread
    atomically (finishTask registry 1)
    atomically ((== Nothing) <$> taskToken registry 1) `shouldReturn` True
    atomically (taskThread registry 1) `shouldReturn` Nothing
  it "retains a finished root's token until its last worker scope leaves" $ do
    registry <- newTaskRegistry
    token <- atomically $ registerTask registry 1
    withTaskScope registry 1 $ withTaskScope registry 1 $ do
      atomically $ finishTask registry 1
      atomically ((== Just token) <$> taskToken registry 1) `shouldReturn` True
    atomically ((== Nothing) <$> taskToken registry 1) `shouldReturn` True
  it "cancels whole associated roots while preserving the reset root and unrelated work" $ do
    registry <- newTaskRegistry
    tokens <- mapM (atomically . registerTask registry) [1, 2, 3]
    atomically $ do
      associateTaskVms registry 1 [10, 20]
      associateTaskVms registry 2 [10]
      associateTaskVms registry 3 [30]
      beginVmReset registry 2 10
    mapM readTVarIO tokens `shouldReturn` [True, False, False]
    atomically (associateTaskVms registry 2 [10])
    denied <- try $ atomically (associateTaskVms registry 3 [10])
    (denied :: Either VmRecoveryException ()) `shouldSatisfy` isLeft
    atomically $ do
      confirmVmDrain registry 2 10
      finishVmReset registry 2 10
    atomically (vmIsRecovering registry 10) `shouldReturn` True
    atomically $ finishTask registry 1
    atomically (vmIsRecovering registry 10) `shouldReturn` False
    atomically (associateTaskVms registry 3 [10])
  it "keeps recovery protection after failure and permits an explicit retry" $ do
    registry <- newTaskRegistry
    atomically $ beginVmReset registry 1 10
    concurrent <- try $ atomically (beginVmReset registry 2 10)
    (concurrent :: Either VmRecoveryException ()) `shouldSatisfy` isLeft
    atomically $ finishVmReset registry 1 10
    atomically (vmIsRecovering registry 10) `shouldReturn` True
    atomically $ beginVmReset registry 2 10
    -- A delayed success from the first attempt cannot clear the retry's gate.
    atomically $ confirmVmDrain registry 1 10
    atomically $ finishVmReset registry 2 10
    atomically (vmIsRecovering registry 10) `shouldReturn` True
    atomically $ confirmVmDrain registry 2 10
    atomically (vmIsRecovering registry 10) `shouldReturn` False
  it "waits for scopes even after both reset completion and remote drain" $ do
    registry <- newTaskRegistry
    atomically $ associateTaskVms registry 1 [10]
    withTaskScope registry 1 $ do
      atomically $ do
        beginVmReset registry 2 10
        finishTask registry 1
        finishVmReset registry 2 10
        confirmVmDrain registry 2 10
      atomically (vmIsRecovering registry 10) `shouldReturn` True
    atomically (vmIsRecovering registry 10) `shouldReturn` False
  it "fences delayed drain results when the same task tree retries reset" $ do
    registry <- newTaskRegistry
    atomically $ do
      beginVmReset registry 1 10
      recordVmResetRevision registry 1 10 41
      finishVmReset registry 1 10
      beginVmReset registry 1 10
      recordVmResetRevision registry 1 10 42
      confirmVmRevisionDrain registry 1 10 41
      finishVmReset registry 1 10
    atomically (vmIsRecovering registry 10) `shouldReturn` True
    denied <- try $ atomically (associateTaskVms registry 1 [10])
    (denied :: Either VmRecoveryException ()) `shouldSatisfy` isLeft
    atomically $ confirmVmRevisionDrain registry 1 10 42
    atomically (vmIsRecovering registry 10) `shouldReturn` False
  it "cancels planned work before its worker acquires any resource" $ do
    registry <- newTaskRegistry
    atomically $ declarePlannedTaskVms registry 1 [10, 20]
    atomically $ beginVmReset registry 2 20
    atomically (taskVms registry 1) `shouldReturn` ([10, 20], True)
  it "requires explicit reset after restoring unproven recovery on restart" $ do
    registry <- newTaskRegistry
    atomically $ restoreVmRecovery registry 10
    atomically (vmIsRecovering registry 10) `shouldReturn` True
    denied <- try $ atomically (associateTaskVms registry 1 [10])
    (denied :: Either VmRecoveryException ()) `shouldSatisfy` isLeft
    atomically $ do
      beginVmReset registry 2 10
      recordVmResetRevision registry 2 10 9
      confirmVmRevisionDrain registry 2 10 9
      finishVmReset registry 2 10
    atomically (vmIsRecovering registry 10) `shouldReturn` False
  it "retains participating hosts after the task and its capabilities separate" $ do
    registry <- newTaskRegistry
    atomically $ do
      declarePlannedTaskVms registry 1 [10]
      recordTaskNode registry 1 100
      recordTaskNode registry 1 200
      finishTask registry 1
    atomically (vmOperationNodes registry 10) `shouldReturn` Just [100, 200]
  it "keeps host history unknown across new work until a fenced reset drains" $ do
    registry <- newTaskRegistry
    atomically $ do
      restoreVmNodeHistory registry 10
      declarePlannedTaskVms registry 1 [10]
      recordTaskNode registry 1 100
    atomically (vmOperationNodes registry 10) `shouldReturn` Nothing
    atomically $ do
      beginVmReset registry 2 10
      recordVmResetRevision registry 2 10 9
      confirmVmRevisionDrain registry 2 10 8
    atomically (vmOperationNodes registry 10) `shouldReturn` Nothing
    atomically $ confirmVmRevisionDrain registry 2 10 9
    atomically (vmOperationNodes registry 10) `shouldReturn` Just []
  it "excludes retries during final cleanup and preserves a forced-delete gate" $ do
    registry <- newTaskRegistry
    atomically $ do
      beginVmReset registry 1 10
      recordVmResetRevision registry 1 10 9
      finishVmReset registry 1 10
    withVmResetFinalization registry 1 10 9 $ do
      denied <- try $ atomically (beginVmReset registry 2 10)
      (denied :: Either VmRecoveryException ()) `shouldSatisfy` isLeft
      atomically $ confirmVmRevisionDrain registry 1 10 9
      atomically (vmIsRecovering registry 10) `shouldReturn` True
    atomically (vmIsRecovering registry 10) `shouldReturn` False
    atomically $ do
      beginVmReset registry 2 10
      recordVmResetRevision registry 2 10 10
    withVmResetFinalization registry 2 10 10 $
      atomically $
        confirmVmRevisionDrain registry 2 10 10
    atomically (vmIsRecovering registry 10) `shouldReturn` True
    atomically $ finishVmReset registry 2 10
    atomically (vmIsRecovering registry 10) `shouldReturn` False
