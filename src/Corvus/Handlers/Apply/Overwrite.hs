{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Apply.Overwrite (Overwrite (..), preflightDiskOverwrite, preflightNetworkOverwrite, preflightVmOverwrite, preflightSshKeyOverwrite) where

import Corvus.Model
import Corvus.Protocol (Response)
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool, toSqlKey)

data Overwrite = Overwrite {oPreflight :: IO (Either Text ()), oDelete :: IO Response}
preflightDiskOverwrite state name eid = attachedMessage state (vmsAttachedToDisk $ toSqlKey eid) ("cannot overwrite disk '" <> name <> "': attached to VM(s) ") "; detach or delete those VMs first"
preflightNetworkOverwrite state name eid = attachedMessage state (vmsAttachedToNetwork $ toSqlKey eid) ("cannot overwrite network '" <> name <> "': in use by VM(s) ") "; remove their network interfaces first"
attachedMessage state query prefix suffix = do
  attached <- runSqlPool query (ssDbPool state)
  pure $ if null attached then Right () else Left $ prefix <> T.intercalate ", " attached <> suffix
preflightVmOverwrite state name eid = do
  mVm <- runSqlPool (get $ toSqlKey eid :: SqlPersistT IO (Maybe Vm)) (ssDbPool state)
  pure $ case mVm of
    Nothing -> Right ()
    Just vm
      | vmStatus vm == VmStopped || vmStatus vm == VmError -> Right ()
      | otherwise -> Left $ "cannot overwrite VM '" <> name <> "': currently in status " <> enumToText (vmStatus vm) <> "; stop it first"
preflightSshKeyOverwrite state eid = do
  let keyId = toSqlKey eid :: SshKeyId; pool = ssDbPool state
  vmRefs <- runSqlPool (selectList [VmSshKeySshKeyId ==. keyId] []) pool
  tmplRefs <- runSqlPool (selectList [TemplateSshKeySshKeyId ==. keyId] []) pool
  vmNames <- runSqlPool (names vmName $ map (vmSshKeyVmId . entityVal) vmRefs) pool
  tmplNames <- runSqlPool (names templateVmName $ map (templateSshKeyTemplateId . entityVal) tmplRefs) pool
  pure $ case (vmNames, tmplNames) of
    ([], []) -> Right ()
    (vs, ts) -> Left $ "cannot overwrite SSH key: attached to " <> describe "VM" vs <> (if null ts || null vs then "" else " and ") <> describe "template" ts <> "; detach first"
  where
    describe _ [] = ""; describe kind xs = kind <> "(s) " <> T.intercalate ", " xs
    names nameOf ks = do es <- mapM get ks; pure [nameOf e | Just e <- es]
vmsAttachedToDisk diskId = do
  drives <- selectList [DriveDiskImageId ==. Just diskId] []
  vms <- mapM get $ map (driveVmId . entityVal) drives
  pure [vmName v | Just v <- vms]
vmsAttachedToNetwork nid = do
  nis <- selectList [NetworkInterfaceNetworkId ==. Just nid] []
  vms <- mapM get $ map (networkInterfaceVmId . entityVal) nis
  pure [vmName v | Just v <- vms]
