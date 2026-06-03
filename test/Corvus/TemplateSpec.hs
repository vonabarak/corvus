{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | VM-template CRUD + instantiation guards.
--
-- The handler under test is `Corvus.Handlers.Template`. The
-- create / update path goes through `Corvus.Schema.Template`'s
-- YAML parser; we feed it minimal-but-valid YAML for the happy
-- paths and a malformed string for the parse-error path.
-- Instantiation needs at least one backing disk image on the
-- target node (`DiskImageNode` row) — when missing we expect a
-- `RespError`, not a panic.
module Corvus.TemplateSpec (spec) where

import Corvus.Protocol (TemplateDetails (..), TemplateSharedDirInfo (..))
import Test.Prelude

-- A minimal template YAML accepted by `Corvus.Schema.Template`'s
-- `TemplateYaml` parser. `drives:` is required (no default), so
-- we include an empty list explicitly.
minimalYaml :: Text -> Text
minimalYaml name =
  "name: "
    <> name
    <> "\n\
       \cpuCount: 2\n\
       \ramMb: 1024\n\
       \drives: []\n"

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenTemplateList" $ do
    testCase "returns an empty list when no templates exist" $ do
      when_ whenTemplateList
      then_ $ responseIs $ \case
        RespTemplateList [] -> True
        _ -> False

  describe "whenTemplateCreate" $ do
    testCase "writes a template row from valid YAML" $ do
      when_ $ whenTemplateCreate (minimalYaml "t1")
      then_ $ responseIs $ \case
        RespTemplateCreated _ -> True
        _ -> False

    testCase "rejects malformed YAML" $ do
      when_ $ whenTemplateCreate "not: a: valid template"
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "rejects a duplicate template name" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "dup")
      when_ $ whenTemplateCreate (minimalYaml "dup")
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

  describe "whenTemplateShow" $ do
    testCase "returns TemplateNotFound for unknown id" $ do
      when_ $ whenTemplateShow 999
      then_ $ responseIs $ \case
        RespTemplateNotFound -> True
        _ -> False

    testCase "returns details for an existing template" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "showable")
      when_ $ whenTemplateShow 1
      then_ $ responseIs $ \case
        RespTemplateInfo _ -> True
        _ -> False

  describe "whenTemplateUpdate" $ do
    testCase "returns a clean error for unknown id" $ do
      -- handleTemplateUpdate surfaces missing-template as
      -- `RespError "Template not found"` rather than the
      -- dedicated RespTemplateNotFound constructor — TemplateShow
      -- uses the constructor; update uses the error string.
      when_ $ whenTemplateUpdate 999 (minimalYaml "ghost")
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "rewrites the row when both id and YAML are valid" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "orig")
      when_ $ whenTemplateUpdate 1 (minimalYaml "renamed")
      then_ $ responseIs $ \case
        RespTemplateUpdated _ -> True
        _ -> False

  describe "whenTemplateDelete" $ do
    testCase "deletes an existing template" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "doomed")
      when_ $ whenTemplateDelete 1
      then_ $ responseIs (== RespTemplateDeleted)

  describe "template sharedDirs round-trip" $ do
    testCase "create + show preserves sharedDirs fields" $ do
      let yaml =
            "name: tpl-sd\n\
            \cpuCount: 1\n\
            \ramMb: 512\n\
            \drives: []\n\
            \sharedDirs:\n\
            \  - path: /srv/data\n\
            \    tag: data\n\
            \  - path: /etc/ssl\n\
            \    tag: certs\n\
            \    cache: never\n\
            \    readOnly: true\n"
      _ <- when_ $ whenTemplateCreate yaml
      when_ $ whenTemplateShow 1
      then_ $ responseIs $ \case
        RespTemplateInfo details ->
          let sds = tvdSharedDirs details
              byTag t = filter (\sd -> tvsdiTag sd == t) sds
              [d] = byTag "data"
              [c] = byTag "certs"
           in length sds == 2
                && tvsdiPath d == "/srv/data"
                && tvsdiCache d == CacheAuto
                && not (tvsdiReadOnly d)
                && tvsdiPath c == "/etc/ssl"
                && tvsdiCache c == CacheNever
                && tvsdiReadOnly c
        _ -> False

    testCase "rejects a template with a duplicate sharedDirs tag" $ do
      let yaml =
            "name: tpl-dup-tag\n\
            \cpuCount: 1\n\
            \ramMb: 512\n\
            \drives: []\n\
            \sharedDirs:\n\
            \  - path: /a\n\
            \    tag: same\n\
            \  - path: /b\n\
            \    tag: same\n"
      when_ $ whenTemplateCreate yaml
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False
