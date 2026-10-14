{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.GuestFile (GuestFileUpload (..), downloadGuestFile) where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as S
import Corvus.Action
import Corvus.Model
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Rpc.FileTransfer (withSource)
import Corvus.Types
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Database.Persist (get)
import Database.Persist.Sql (runSqlPool)

-- | Source callbacks are scoped to the Action's node RPC. Build steps invoke
-- this Action in their existing context rather than creating duplicate tasks.
data GuestFileUpload = GuestFileUpload
  { gfuVmId :: Int64
  , gfuPath :: Text
  , gfuSource :: Int -> IO (BS.ByteString, Bool)
  }

instance Action GuestFileUpload where
  actionSubsystem _ = SubVm
  actionCommand _ = "guest-upload"
  actionEntityId = Just . fromIntegral . gfuVmId
  actionExecute ctx a = transfer (acState ctx) (gfuVmId a) (gfuPath a) $ \nac ->
    withSource (NOA.nacSupervisor nac) (gfuSource a) (NOA.vmGuestWriteFile nac (gfuVmId a) (gfuPath a))

downloadGuestFile :: ServerState -> Int64 -> Text -> C.Client S.ByteSink -> IO Response
downloadGuestFile state vmId path sink = transfer state vmId path $ \nac -> NOA.vmGuestReadFile nac vmId path sink

transfer :: ServerState -> Int64 -> Text -> (NOA.NodeAgentClient -> IO (Either NOA.NodeAgentError Word64)) -> IO Response
transfer state vmId path operation
  | T.null path || T.any (== '\0') path = pure (RespGuestAgentError "guest file path must be nonempty and contain no NUL")
  | otherwise = do
      vm <- runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
      case vm of
        Nothing -> pure RespVmNotFound
        Just row
          | vmStatus row /= VmRunning -> pure (RespInvalidTransition (vmStatus row) "VM must be running for guest file transfer")
          | not (vmGuestAgent row) -> pure RespGuestAgentNotEnabled
          | otherwise -> do
              result <- withVmNodeAgent state vmId operation
              pure $ case result of
                Left err -> RespGuestAgentError err
                Right (Left err) -> RespGuestAgentError (T.pack (show err))
                Right (Right count) -> RespGuestFileTransferred count
