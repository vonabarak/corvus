{-# LANGUAGE OverloadedStrings #-}

-- | Thin wrapper around the @nft@ CLI for the network agent's NAT
-- and firewall ops.
--
-- Phase 2 ships per-call masquerade rules — one rule per network
-- the daemon asks NAT for. Each rule is tagged with its kernel
-- handle (parsed from @nft -ae add rule@'s stdout) so teardown is
-- precise: @nft delete rule ... handle <N>@, not a fragile
-- comment-grep.
--
-- The agent owns its own @inet corvus_fw@ table; admin / firewalld
-- rules in other tables won't collide. Future Phase 2 slices fill
-- in the FORWARD-chain isolation policy from the plan.
module Corvus.Netd.Nftables
  ( NftError (..)
  , RuleHandle (..)
  , ensureBaseTable
  , addMasquerade
  , deleteRule
  )
where

import qualified Control.Exception as E
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

-- | Captured @nft@ failure. ArgV + stderr is enough context for a
-- Cap'n Proto error envelope.
data NftError = NftError
  { neArgs :: ![T.Text]
  , neExitCode :: !Int
  , neStderr :: !T.Text
  }
  deriving (Show)

instance E.Exception NftError

-- | Kernel handle for one nftables rule. Opaque to callers; the
-- only operation that consumes it is 'deleteRule'.
newtype RuleHandle = RuleHandle Int
  deriving (Eq, Show)

-- | Create the @inet corvus_fw@ table + @postrouting@ chain, both
-- idempotently. Phase 2 first cut: just the postrouting chain for
-- NAT. The forward / input chains for cross-bridge isolation land
-- in a follow-up slice.
--
-- Idempotency strategy: we send the whole script via @nft -f -@.
-- If the table / chain already exist, nft errors with "File
-- exists"; we detect that string in stderr and treat it as
-- success. This avoids a check-then-create race.
ensureBaseTable :: IO (Either NftError ())
ensureBaseTable = do
  let script =
        unlines
          [ "add table inet corvus_fw"
          , "add chain inet corvus_fw postrouting "
              <> "{ type nat hook postrouting priority srcnat; policy accept; }"
          ]
  (code, _stdout, stderr) <-
    readProcessWithExitCode "nft" ["-f", "-"] script
  case code of
    ExitSuccess -> pure (Right ())
    ExitFailure n ->
      if "File exists" `T.isInfixOf` T.pack stderr
        then pure (Right ())
        else
          pure $
            Left
              NftError
                { neArgs = ["nft", "-f", "-"]
                , neExitCode = n
                , neStderr = T.strip (T.pack stderr)
                }

-- | Insert a masquerade rule for the given source subnet leaving
-- via the given uplink interface, into @inet corvus_fw
-- postrouting@. Returns the handle the kernel assigned so it can
-- be deleted later.
--
-- @uplinkIf@ may be empty — that means "match any outbound
-- interface", and we drop the @oifname@ clause from the rule.
addMasquerade :: T.Text -> T.Text -> IO (Either NftError RuleHandle)
addMasquerade subnet uplinkIf = do
  let oifClause =
        if T.null uplinkIf
          then []
          else ["oifname", T.unpack uplinkIf]
      args =
        [ "-ae"
        , "add"
        , "rule"
        , "inet"
        , "corvus_fw"
        , "postrouting"
        , "ip"
        , "saddr"
        , T.unpack subnet
        ]
          <> oifClause
          <> ["masquerade"]
  (code, stdout, stderr) <- readProcessWithExitCode "nft" args ""
  case code of
    ExitFailure n ->
      pure $
        Left
          NftError
            { neArgs = T.pack <$> ("nft" : args)
            , neExitCode = n
            , neStderr = T.strip (T.pack stderr)
            }
    ExitSuccess ->
      case parseHandle stdout of
        Just h -> pure (Right (RuleHandle h))
        Nothing ->
          pure $
            Left
              NftError
                { neArgs = T.pack <$> ("nft" : args)
                , neExitCode = 0
                , neStderr =
                    "nft add rule: handle not found in stdout: "
                      <> T.pack stdout
                }

-- | Remove the rule at the given handle from @inet corvus_fw
-- postrouting@. Tolerant of "rule doesn't exist": returns success
-- since the desired state (rule absent) is achieved either way.
deleteRule :: RuleHandle -> IO (Either NftError ())
deleteRule (RuleHandle h) = do
  let args =
        [ "delete"
        , "rule"
        , "inet"
        , "corvus_fw"
        , "postrouting"
        , "handle"
        , show h
        ]
  (code, _stdout, stderr) <- readProcessWithExitCode "nft" args ""
  case code of
    ExitSuccess -> pure (Right ())
    ExitFailure n ->
      if "No such file" `T.isInfixOf` T.pack stderr
        || "does not exist" `T.isInfixOf` T.pack stderr
        then pure (Right ())
        else
          pure $
            Left
              NftError
                { neArgs = T.pack <$> ("nft" : args)
                , neExitCode = n
                , neStderr = T.strip (T.pack stderr)
                }

-- ---------------------------------------------------------------------------
-- Parsing helper

-- | Extract the @# handle N@ trailer from @nft -ae add rule@'s
-- stdout. The first line of the output echoes the rule with its
-- handle as a trailing comment:
--
-- @
-- add rule inet corvus_fw postrouting ip saddr 10.0.0.0/24 ... masquerade # handle 7
-- # new generation 3 by process 569 (nft)
-- @
--
-- We scan each line for a tail that ends in @… handle N@ and
-- return the first match. The "new generation" trailer that
-- nft 1.1+ emits on the next line is ignored.
parseHandle :: String -> Maybe Int
parseHandle s = firstJust (map parseLine (lines s))
  where
    parseLine ln = case reverse (words ln) of
      (lastTok : "handle" : _) -> readMaybe lastTok
      _ -> Nothing
    firstJust = foldr (\m acc -> maybe acc Just m) Nothing
