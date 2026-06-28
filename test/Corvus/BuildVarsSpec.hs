{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Unit tests for 'Corvus.Client.BuildVars.applyBuildVars'.
--
-- Covers the substitution syntax (@{{ name }}@), literal delimiter
-- escapes, the @vars:@-declares-the-allowed-set rule, the CLI / file
-- precedence order, the required-no-default contract, and the guarantee
-- that the top-level @vars:@ key is stripped from the output (so the
-- daemon never sees it).
module Corvus.BuildVarsSpec (spec) where

import Corvus.Client.BuildVars (VarError (..), applyBuildVars)
import Corvus.Utils.Yaml (yamlQQ)
import Test.Hspec

spec :: Spec
spec = describe "Corvus.Client.BuildVars.applyBuildVars" $ do
  it "leaves a document without a vars: block untouched" $ do
    let doc =
          [yamlQQ|
pipeline:
  - apply:
      disks:
        - name: foo
          import: "https://example.com/foo.qcow2"
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right doc

  it "strips the vars: key from the output even when empty" $ do
    let doc =
          [yamlQQ|
vars: {}
pipeline:
  - apply:
      disks:
        - name: foo
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      disks:
        - name: foo
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected

  it "expands a single {{ name }} reference from --var" $ do
    let doc =
          [yamlQQ|
vars:
  base_image_url: ~
pipeline:
  - apply:
      disks:
        - name: foo
          import: "{{ base_image_url }}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      disks:
        - name: foo
          import: "https://example.com/foo.qcow2"
|]
    r <-
      applyBuildVars
        [("base_image_url", "https://example.com/foo.qcow2")]
        []
        doc
    r `shouldBe` Right expected

  it "expands checksum values inside apply disk definitions" $ do
    let doc =
          [yamlQQ|
vars:
  base_image_url: ~
  base_image_sha256: ~
pipeline:
  - apply:
      disks:
        - name: foo
          import: "{{ base_image_url }}"
          checksum:
            algorithm: sha256
            value: "{{ base_image_sha256 }}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      disks:
        - name: foo
          import: "https://example.com/foo.qcow2"
          checksum:
            algorithm: sha256
            value: "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
|]
    r <-
      applyBuildVars
        [ ("base_image_url", "https://example.com/foo.qcow2")
        , ("base_image_sha256", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
        ]
        []
        doc
    r `shouldBe` Right expected

  it "uses a YAML default when no --var is supplied" $ do
    let doc =
          [yamlQQ|
vars:
  monitor_ip: 192.168.72.1
pipeline:
  - apply:
      vms:
        - name: m
          notes: "{{ monitor_ip }}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      vms:
        - name: m
          notes: "192.168.72.1"
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected

  it "lets --var override the YAML default" $ do
    let doc =
          [yamlQQ|
vars:
  monitor_ip: 192.168.72.1
pipeline:
  - apply:
      vms:
        - name: m
          notes: "{{ monitor_ip }}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      vms:
        - name: m
          notes: "10.0.0.5"
|]
    r <- applyBuildVars [("monitor_ip", "10.0.0.5")] [] doc
    r `shouldBe` Right expected

  it "errors when a required (null-defaulted) variable is unset" $ do
    let doc =
          [yamlQQ|
vars:
  base_image_url: ~
pipeline:
  - apply: {disks: [{name: f, import: "{{ base_image_url }}"}]}
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Left (VarUnsetRequired "base_image_url")

  it "errors on {{ unknown }} reference" $ do
    let doc =
          [yamlQQ|
vars: {x: "1"}
pipeline:
  - apply: {vms: [{name: m, notes: "{{ typo }}"}]}
|]
    r <- applyBuildVars [] [] doc
    case r of
      Left (VarUnknownRef "typo" _) -> pure ()
      other -> expectationFailure ("expected VarUnknownRef typo, got " <> show other)

  it "errors when --var names a variable not in vars:" $ do
    let doc =
          [yamlQQ|
vars: {x: "1"}
pipeline:
  - apply: {vms: [{name: m, notes: "{{ x }}"}]}
|]
    r <- applyBuildVars [("y", "2")] [] doc
    r `shouldBe` Left (VarUndeclaredCli "y")

  it "errors on malformed {{ ref with no closing braces" $ do
    let doc =
          [yamlQQ|
vars: {x: "1"}
pipeline:
  - apply: {vms: [{name: m, notes: "{{ x is broken"}]}
|]
    r <- applyBuildVars [] [] doc
    case r of
      Left (VarMalformedRef _) -> pure ()
      other -> expectationFailure ("expected VarMalformedRef, got " <> show other)

  it "expands compact {{name}} references without spaces" $ do
    let doc =
          [yamlQQ|
vars: {x: "1"}
pipeline:
  - apply:
      vms:
        - name: m
          notes: "templated {{x}}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      vms:
        - name: m
          notes: "templated 1"
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected

  it "leaves shell variables untouched" $ do
    let doc =
          [yamlQQ|
vars: {x: "1"}
pipeline:
  - apply:
      vms:
        - name: m
          notes: "$5 and $HOME and ${HOME} and ${DEV}2"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      vms:
        - name: m
          notes: "$5 and $HOME and ${HOME} and ${DEV}2"
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected

  it "leaves old ${name} syntax untouched" $ do
    let doc =
          [yamlQQ|
vars: {x: "1"}
pipeline:
  - apply:
      vms:
        - name: m
          notes: "${x}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      vms:
        - name: m
          notes: "${x}"
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected

  it "escapes literal template delimiters" $ do
    let doc =
          [yamlQQ|
vars: {x: "1"}
pipeline:
  - apply:
      vms:
        - name: m
          notes: "literal {{{{ x }}}} and templated {{ x }}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      vms:
        - name: m
          notes: "literal {{ x }} and templated 1"
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected

  it "expands multiple {{ ... }} refs in one string" $ do
    let doc =
          [yamlQQ|
vars:
  base: "https://example.com"
  file: "foo.qcow2"
pipeline:
  - apply:
      disks:
        - name: f
          import: "{{ base }}/path/{{ file }}"
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      disks:
        - name: f
          import: "https://example.com/path/foo.qcow2"
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected

  it "walks nested arrays and objects" $ do
    let doc =
          [yamlQQ|
vars: {host: example.com, port: "8080"}
pipeline:
  - apply:
      vms:
        - name: m
          notes: "{{ host }}:{{ port }}"
          tags: ["{{ host }}", "static", "{{ port }}"]
|]
        expected =
          [yamlQQ|
pipeline:
  - apply:
      vms:
        - name: m
          notes: "example.com:8080"
          tags: ["example.com", "static", "8080"]
|]
    r <- applyBuildVars [] [] doc
    r `shouldBe` Right expected
