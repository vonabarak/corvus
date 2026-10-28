-- | Read-only display capture, routed to the VM's owning node.
module Corvus.Handlers.Vm.Screenshot (captureVmScreenshot) where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as N
import qualified Capnp.Gen.Streams as S
import Corvus.Model (Vm (..), VmStatus (..))
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Rpc.Common (throwWireError)
import Corvus.Types (ServerState, ssDbPool)
import Corvus.Wire.Error (ErrorCode (..))
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word64)
import Database.Persist (get)
import Database.Persist.Sql (runSqlPool, toSqlKey)

captureVmScreenshot :: ServerState -> Int64 -> C.Client S.ByteSink -> IO Word64
captureVmScreenshot state vmId sink = do
  row <- runSqlPool (get (toSqlKey vmId)) (ssDbPool state)
  case row of
    Nothing -> throwWireError VmNotFound "VM not found"
    Just vm
      | vmHeadless vm -> throwWireError VmHeadless "VM has no graphical display"
      | vmStatus vm `notElem` [VmRunning, VmPaused, VmStarting, VmStopping, VmError] -> throwWireError VmNotRunning "VM is not running"
      | otherwise -> do
          result <- withVmNodeAgent state vmId $ \nac -> NOA.vmScreenshot nac vmId sink
          case result of
            Left err -> throwWireError InternalError ("Screenshot failed: " <> err)
            Right (Left err) -> throwWireError InternalError ("Screenshot failed: " <> T.pack (show err))
            Right (Right (N.Session'vmScreenshot'results status message count)) -> case status of
              N.ScreenshotStatus'success -> pure count
              N.ScreenshotStatus'notRunning -> throwWireError VmNotRunning message
              N.ScreenshotStatus'headless -> throwWireError VmHeadless message
              _ -> throwWireError InternalError ("Screenshot failed: " <> message)
