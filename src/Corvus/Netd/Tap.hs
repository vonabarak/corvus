{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | TAP state machine for `corvus-netd`.
--
-- Same idempotent shape as 'Corvus.Netd.Network':
--
--   * 'applyTap' — create-if-missing, update-if-changed.
--   * 'deleteTap' — remove TAP from kernel and ledger.
--   * 'listTaps' — enumerate ledger.
--
-- A TAP refers to a bridge by name; we don't validate that the
-- bridge currently exists in the network ledger (cross-module
-- read), but the kernel's @ip link set master@ will fail
-- loudly if it doesn't.
module Corvus.Netd.Tap
  ( TapSpec (..)
  , TapInfo (..)
  , TapError (..)
  , Ledger
  , applyTap
  , deleteTap
  , listTaps
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad (void)
import Corvus.Netd.Cleanup (corvusTapPrefix)
import Corvus.Netd.IpLink
  ( IpLinkError (..)
  , linkSetMaster
  , linkSetUp
  , tapAdd
  , tapDel
  )
import qualified Corvus.Netd.Ledger as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word (Word32)

-- | The 'L.TapLedger' specialised for our spec.
type Ledger = L.TapLedger TapSpec

data TapSpec = TapSpec
  { tsName :: !T.Text
  , tsBridge :: !T.Text
  , tsUid :: !Word32
  , tsGid :: !Word32
  }
  deriving (Eq, Show)

data TapInfo = TapInfo
  { tiSpec :: !TapSpec
  , tiUpState :: !T.Text
  }
  deriving (Show)

data TapError
  = InvalidTapName !T.Text
  | KernelFailure !T.Text
  deriving (Show)

instance E.Exception TapError

-- ---------------------------------------------------------------------------
-- Public API

applyTap :: Ledger -> TapSpec -> IO (Either TapError TapInfo)
applyTap ledger spec = E.try @TapError $ do
  validateName (tsName spec)
  current <- atomically $ Map.lookup (tsName spec) <$> L.readTaps ledger
  case current of
    Nothing -> createFromScratch ledger spec
    Just old -> reconcile ledger old spec

deleteTap :: Ledger -> T.Text -> IO (Either TapError ())
deleteTap ledger name = E.try @TapError $ do
  mEntry <-
    atomically $ do
      m <- L.readTaps ledger
      case Map.lookup name m of
        Nothing -> pure Nothing
        Just entry -> do
          L.removeTap ledger name
          pure (Just entry)
  case mEntry of
    Nothing -> pure ()
    Just _ -> do
      _ <- tapDel name
      pure ()

listTaps :: Ledger -> IO [TapInfo]
listTaps ledger = do
  m <- atomically (L.readTaps ledger)
  pure
    [ TapInfo {tiSpec = spec, tiUpState = "up"}
    | spec <- Map.elems m
    ]

-- ---------------------------------------------------------------------------
-- Validation

validateName :: T.Text -> IO ()
validateName name = do
  whenE (T.null name) (InvalidTapName "name must be non-empty")
  whenE (not (corvusTapPrefix `T.isPrefixOf` name)) $
    InvalidTapName (name <> ": must start with \"" <> corvusTapPrefix <> "\"")
  whenE (T.length name > 15) $
    InvalidTapName (name <> ": exceeds IFNAMSIZ-1 (15 chars)")

whenE :: Bool -> TapError -> IO ()
whenE True err = E.throwIO err
whenE False _ = pure ()

-- ---------------------------------------------------------------------------
-- Fresh-create + reconcile

createFromScratch :: Ledger -> TapSpec -> IO TapInfo
createFromScratch ledger spec = do
  let name = tsName spec
  unwrapIp =<< tapAdd name (tsUid spec) (tsGid spec)
  let rollback = void (tapDel name)
  withRollback rollback $ do
    unwrapIp =<< linkSetMaster name (tsBridge spec)
    unwrapIp =<< linkSetUp name
  atomically $ L.insertTap ledger name spec
  pure (TapInfo {tiSpec = spec, tiUpState = "up"})

reconcile :: Ledger -> TapSpec -> TapSpec -> IO TapInfo
reconcile ledger old new
  | old == new = pure (TapInfo {tiSpec = new, tiUpState = "up"})
  | tsUid old /= tsUid new || tsGid old /= tsGid new = do
      -- kernel doesn't support changing TUNSETOWNER live —
      -- recreate.
      _ <- tapDel (tsName old)
      atomically $ L.removeTap ledger (tsName old)
      createFromScratch ledger new
  | tsBridge old /= tsBridge new = do
      -- swap master.
      unwrapIp =<< linkSetMaster (tsName new) (tsBridge new)
      atomically $ L.insertTap ledger (tsName new) new
      pure (TapInfo {tiSpec = new, tiUpState = "up"})
  | otherwise = do
      -- name same, uid/gid/bridge unchanged — only e.g.
      -- comparison-irrelevant difference (none today). No-op.
      atomically $ L.insertTap ledger (tsName new) new
      pure (TapInfo {tiSpec = new, tiUpState = "up"})

-- ---------------------------------------------------------------------------
-- Helpers

unwrapIp :: Either IpLinkError a -> IO a
unwrapIp (Right a) = pure a
unwrapIp (Left e) = E.throwIO (KernelFailure (ileStderr e))

withRollback :: IO () -> IO a -> IO a
withRollback rollback =
  E.handle (\(e :: TapError) -> rollback >> E.throwIO e)
