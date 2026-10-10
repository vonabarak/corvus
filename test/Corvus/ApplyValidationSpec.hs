{-# LANGUAGE OverloadedStrings #-}

module Corvus.ApplyValidationSpec (spec) where

import Corvus.Handlers.Apply.Validation (checksumSpecToImport, validateConfig)
import Corvus.Schema.Apply
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Yaml (decodeEither')
import Test.Hspec

spec :: Spec
spec = describe "apply validation boundaries" $ do
  mapM_
    ( \(label, yaml, expected) -> it label $ case decodeEither' (T.encodeUtf8 yaml) of
        Left err -> expectationFailure (show err)
        Right config -> case expected of
          Nothing -> validateConfig config `shouldBe` Right ()
          Just fragment -> validateConfig config `shouldSatisfy` either (T.isInfixOf fragment) (const False)
    )
    cases
  mapM_
    ( \(algorithm, size, name) -> it ("validates " <> T.unpack name <> " and normalizes its import representation") $ do
        let checksum = T.replicate size "A"
            source = "disks: [{name: base, import: 'https://image', checksum: {algorithm: " <> name <> ", value: " <> checksum <> ", target: final}}]"
        case decodeEither' (T.encodeUtf8 source) of
          Left err -> expectationFailure (show err)
          Right config -> validateConfig config `shouldBe` Right ()
        checksumSpecToImport (ChecksumSpec algorithm checksum ChecksumFinal) `shouldBe` (name, T.toLower checksum, "final")
    )
    [(ChecksumMd5, 32, "md5"), (ChecksumSha1, 40, "sha1"), (ChecksumSha256, 64, "sha256"), (ChecksumSha512, 128, "sha512"), (ChecksumBlake2b, 128, "blake2b")]

cases :: [(String, T.Text, Maybe T.Text)]
cases =
  [ ("duplicate SSH keys", "sshKeys: [{name: key, publicKey: AAA}, {name: key, publicKey: BBB}]", Just "Duplicate SSH key")
  , ("canonical duplicate disk selectors", "disks: [{name: base, register: /a}, {name: 'base:latest', register: /b}]", Just "Duplicate disk")
  , ("duplicate VMs on one node", "vms: [{name: web, cpuCount: 1, ram: 1G}, {name: web, cpuCount: 1, ram: 1G}]", Just "Duplicate VM")
  , ("same VM name on different nodes", "vms: [{name: web, cpuCount: 1, ram: 1G, node: a}, {name: web, cpuCount: 1, ram: 1G, node: b}]", Nothing)
  , ("duplicate networks on one node", "networks: [{name: net}, {name: net}]", Just "Duplicate network")
  , ("same network name on different nodes", "networks: [{name: net, node: a}, {name: net, node: b}]", Nothing)
  , ("duplicate templates", "templates: [{name: base, cpuCount: 1, ram: 1G, drives: []}, {name: base, cpuCount: 1, ram: 1G, drives: []}]", Just "Duplicate template")
  , ("numeric SSH key name", "sshKeys: [{name: '123', publicKey: AAA}]", Just "all digits")
  , ("numeric VM name", "vms: [{name: '123', cpuCount: 1, ram: 1G}]", Just "all digits")
  , ("empty network name", "networks: [{name: ''}]", Just "cannot be empty")
  , ("empty template name", "templates: [{name: '', cpuCount: 1, ram: 1G, drives: []}]", Just "cannot be empty")
  , ("numeric publication name", "disks: [{name: '123', register: /a}]", Just "requires a name")
  , ("exclusive import and clone", "disks: [{name: base, import: /a, clone: other}]", Just "more than one")
  , ("exclusive overlay and register", "disks: [{name: base, overlay: other, register: /a}]", Just "more than one")
  , ("missing disk strategy", "disks: [{name: base}]", Just "must specify")
  , ("incomplete create", "disks: [{name: base, format: qcow2}]", Just "must specify")
  , ("complete create with path", "disks: [{name: base, format: qcow2, size: 1G, path: /a}]", Nothing)
  , ("register with forbidden path", "disks: [{name: base, register: /a, path: /b}]", Just "'path' can only")
  , ("overlay with path", "disks: [{name: base, overlay: other, path: /a}]", Nothing)
  , ("clone with path", "disks: [{name: base, clone: other, path: /a}]", Nothing)
  , ("import with forbidden backing", "disks: [{name: base, import: /a, backing: other}]", Just "'backing' can only")
  , ("register with backing", "disks: [{name: base, register: /a, backing: other}]", Nothing)
  , ("checksum without import", "disks: [{name: base, register: /a, checksum: {algorithm: md5, value: abcd}}]", Just "'checksum' can only")
  , ("checksum with local import", "disks: [{name: base, import: /a, checksum: {algorithm: md5, value: abcd}}]", Just "HTTP/HTTPS")
  , ("nonhex checksum", "disks: [{name: base, import: 'https://image', checksum: {algorithm: md5, value: zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz}}]", Just "32 hex")
  , ("update without checksum", "disks: [{name: base, import: 'https://image', ifExists: update}]", Just "update requires")
  , ("SSH keys enable cloud-init by default", "vms: [{name: web, cpuCount: 1, ram: 1G, sshKeys: [key]}]", Nothing)
  , ("explicit disabled cloud-init rejects SSH keys", "vms: [{name: web, cpuCount: 1, ram: 1G, cloudInit: false, sshKeys: [key]}]", Just "cloud-init is not enabled")
  , ("explicit cloud-init with SSH keys", "vms: [{name: web, cpuCount: 1, ram: 1G, cloudInit: true, sshKeys: [key]}]", Nothing)
  ]
