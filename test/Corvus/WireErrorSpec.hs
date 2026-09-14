{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the structured wire error format
-- (@<code> :: <message>@) and the 'Response' → (code, message)
-- mapping that is the single source of truth for wire error texts.
module Corvus.WireErrorSpec (spec) where

import Control.Monad (forM_)
import Corvus.Model (VmStatus (VmStopped))
import Corvus.Protocol
import Corvus.Rpc.Error (responseError)
import Corvus.Wire.Error
import Data.List (nub)
import qualified Data.Text as T
import Test.Hspec

-- Responses whose wire message is hard-coded in 'responseError'
-- (as opposed to free-form pass-throughs like 'RespError' whose
-- text comes from elsewhere).
staticResponses :: [Response]
staticResponses =
  [ RespVmNotFound
  , RespDiskNotFound
  , RespSnapshotNotFound
  , RespDriveNotFound
  , RespNetIfNotFound
  , RespSshKeyNotFound
  , RespSshKeyInUse [(1, "vm")]
  , RespSharedDirNotFound
  , RespTemplateNotFound
  , RespTaskNotFound
  , RespNodeNotFound
  , RespDiskInUse [NamedRef 1 "vm"]
  , RespDiskHasOverlays [NamedRef 1 "vm"]
  , RespVmMustBeStopped
  , RespVmNotRunning
  , RespVmHeadless
  , RespNetworkNotFound
  , RespNetworkInUse
  , RespNetworkAlreadyRunning
  , RespNetworkNotRunning
  , RespGuestAgentNotEnabled
  ]

spec :: Spec
spec = do
  describe "allErrorCodes" $ do
    it "has exactly 28 codes, matching the schema enum" $
      length allErrorCodes `shouldBe` 28
    it "produces 28 distinct snake_case tokens" $
      length (nub [errorCodeText code | code <- allErrorCodes])
        `shouldBe` length allErrorCodes

  describe "renderWireError / parseWireError" $ do
    it "round-trips every code" $
      forM_ allErrorCodes $ \code ->
        parseWireError (renderWireError (WireErrorInfo code "some message"))
          `shouldBe` Just (code, "some message")
    it "keeps a message that itself contains the delimiter" $
      let info = WireErrorInfo VmNotFound "a :: b :: c"
       in parseWireError (renderWireError info)
            `shouldBe` Just (VmNotFound, "a :: b :: c")

  describe "parseWireError" $ do
    it "rejects a code-less (legacy) message" $
      parseWireError "VM not found" `shouldBe` Nothing
    it "rejects an unknown code" $
      parseWireError "totally_unknown :: VM not found" `shouldBe` Nothing
    it "rejects a code with the wrong delimiter" $
      parseWireError "vm_not_found: VM not found" `shouldBe` Nothing
    it "rejects a bare code with no message" $
      parseWireError "vm_not_found" `shouldBe` Nothing
    it "matches a token that shares a prefix with no other" $
      parseWireError "network_in_use :: in use"
        `shouldBe` Just (NetworkInUse, "in use")

  describe "parseWireError edge cases" $ do
    it "rejects empty string" $
      parseWireError "" `shouldBe` Nothing
    it "rejects only delimiter" $
      parseWireError " :: " `shouldBe` Nothing
    it "rejects delimiter at start" $
      parseWireError " :: message" `shouldBe` Nothing
    it "rejects delimiter at end" $
      parseWireError "vm_not_found :: " `shouldBe` Just (VmNotFound, "")
    it "rejects partial code prefix" $
      parseWireError "vm_no :: message" `shouldBe` Nothing
    it "handles message containing delimiter after code" $
      parseWireError "vm_not_found :: a :: b :: c"
        `shouldBe` Just (VmNotFound, "a :: b :: c")
    it "rejects code with trailing underscore" $
      parseWireError "vm_not_found_ :: message" `shouldBe` Nothing
    it "rejects code with leading underscore" $
      parseWireError "_vm_not_found :: message" `shouldBe` Nothing

  describe "errorCodeText" $
    -- parseWireError matches in declaration order, which is sound
    -- only while no token is a prefix of another.
    it "keeps the token set prefix-free" $
      [ errorCodeText a
      | a <- allErrorCodes
      , b <- allErrorCodes
      , a /= b
      , T.isPrefixOf (errorCodeText a) (errorCodeText b)
      ]
        `shouldBe` []

  describe "responseError" $ do
    it "maps not-found errors to their own codes and historical text" $ do
      responseError RespVmNotFound `shouldBe` (VmNotFound, "VM not found")
      responseError RespDiskNotFound `shouldBe` (DiskNotFound, "Disk not found")
      responseError RespSnapshotNotFound
        `shouldBe` (SnapshotNotFound, "Snapshot not found")
      responseError RespDriveNotFound `shouldBe` (DriveNotFound, "Drive not found")
      responseError RespNetworkNotFound
        `shouldBe` (NetworkNotFound, "Network not found")
      responseError RespNetIfNotFound
        `shouldBe` (NetIfNotFound, "Net-if not found")
      responseError RespSshKeyNotFound `shouldBe` (SshKeyNotFound, "SSH key not found")
      responseError RespSharedDirNotFound
        `shouldBe` (SharedDirNotFound, "Shared directory not found")
      responseError RespTemplateNotFound
        `shouldBe` (TemplateNotFound, "Template not found")
      responseError RespTaskNotFound `shouldBe` (TaskNotFound, "Task not found")
      responseError RespNodeNotFound `shouldBe` (NodeNotFound, "Node not found")

    it "maps state-conflict errors" $ do
      responseError (RespDiskInUse []) `shouldBe` (DiskInUse, "Disk in use")
      responseError (RespDiskHasOverlays [])
        `shouldBe` (DiskHasOverlays, "Disk has overlays")
      responseError RespVmMustBeStopped
        `shouldBe` (VmMustBeStopped, "VM must be stopped")
      responseError RespVmNotRunning `shouldBe` (VmNotRunning, "VM not running")
      responseError RespVmHeadless
        `shouldBe` (VmHeadless, "VM has no SPICE display")
      responseError RespNetworkInUse `shouldBe` (NetworkInUse, "Network in use")
      responseError RespNetworkAlreadyRunning
        `shouldBe` (NetworkAlreadyRunning, "Network already running")
      responseError RespNetworkNotRunning
        `shouldBe` (NetworkNotRunning, "Network not running")
      responseError (RespSshKeyInUse [(1, "vm")])
        `shouldBe` (SshKeyInUse, "SSH key in use")
      responseError RespGuestAgentNotEnabled
        `shouldBe` (GuestAgentNotEnabled, "Guest agent not enabled")

    it "renders invalid transitions in the statusOrThrow format" $
      responseError (RespInvalidTransition VmStopped "cannot pause")
        `shouldBe` ( InvalidTransition
                   , "invalid transition from stopped: cannot pause"
                   )

    it "passes through free-form messages with the right code" $ do
      responseError (RespFormatNotSupported "raw not supported here")
        `shouldBe` (FormatNotSupported, "raw not supported here")
      responseError (RespGuestAgentError "freeze failed")
        `shouldBe` (GuestAgentError, "freeze failed")
      responseError (RespNodeInUse "node is draining")
        `shouldBe` (NodeInUse, "node is draining")

    it "maps an ambiguous ref to its own code, keeping the message" $
      let msg = "VM 'shared' is ambiguous: 2 matches across nodes; use the numeric id"
       in responseError (RespAmbiguousRef msg)
            `shouldBe` (AmbiguousRef, msg)

    it "maps the generic error constructors to internal_error" $ do
      let resp = RespError "VM is not headless — use SPICE viewer instead"
       in responseError resp
            `shouldBe` (InternalError, "VM is not headless — use SPICE viewer instead")
      responseError (RespNetworkError "peer attach failed")
        `shouldBe` (InternalError, "peer attach failed")

    it "reports success constructors as internal errors (daemon bug)" $
      case responseError RespPong of
        (code, _) -> code `shouldBe` InternalError

    it "never embeds the wire delimiter in a static message" $
      -- The wire form is "<code> :: <message>" and the client
      -- splits on the first delimiter, so a message containing
      -- " :: " would be mis-split.
      [ msg
      | resp <- staticResponses
      , let (_, msg) = responseError resp
      , T.isInfixOf " :: " msg
      ]
        `shouldBe` []

    it "never embeds the wire delimiter in any error message" $
      -- This extends the static message check to ALL error responses,
      -- including free-form pass-throughs (RespError, RespNodeInUse, etc.).
      -- A message containing " :: " would break client parsing.
      let allErrorResponses =
            [ RespError "some error"
            , RespAmbiguousRef "ambiguous"
            , RespFormatNotSupported "format issue"
            , RespNetworkError "network error"
            , RespGuestAgentError "agent error"
            , RespNodeInUse "node in use"
            , RespVmNotFound
            , RespDiskNotFound
            , RespSnapshotNotFound
            , RespDriveNotFound
            , RespNetIfNotFound
            , RespSshKeyNotFound
            , RespSharedDirNotFound
            , RespTemplateNotFound
            , RespTaskNotFound
            , RespNodeNotFound
            , RespDiskInUse []
            , RespDiskHasOverlays []
            , RespVmMustBeStopped
            , RespVmNotRunning
            , RespVmHeadless
            , RespNetworkInUse
            , RespNetworkAlreadyRunning
            , RespNetworkNotRunning
            , RespSshKeyInUse [(1, "vm")]
            , RespGuestAgentNotEnabled
            , RespInvalidTransition VmStopped "cannot pause"
            ]
       in [ msg
          | resp <- allErrorResponses
          , let (_, msg) = responseError resp
          , T.isInfixOf " :: " msg
          ]
            `shouldBe` []

  describe "renderWireError" $
    it "uses the fixed delimiter" $
      renderWireError (WireErrorInfo VmNotFound "VM not found")
        `shouldBe` "vm_not_found :: VM not found"
