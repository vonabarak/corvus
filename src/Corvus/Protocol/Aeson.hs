{-# LANGUAGE OverloadedStrings #-}

-- | Shared Aeson options for protocol JSON encoding.
--
-- One source of truth so the Python bridge, any future TS/Rust client,
-- and the Haskell-side tests all see the same shape. The shape:
--
-- * Constructor names are stripped of their subsystem prefix
--   (@Req@ / @Resp@) and converted to @snake_case@.
-- * Record field names are converted to @snake_case@ as-is (we rely on
--   @NoFieldSelectors@ so field names don't need per-constructor
--   disambiguation prefixes).
-- * Sum types use a tag field alongside the record fields
--   (@TaggedObject@).
-- * @Nothing@ fields are omitted rather than serialised as @null@.
module Corvus.Protocol.Aeson
  ( requestOptions
  , responseOptions
  , innerOptions
  , camelToSnake
  , dropLowerPrefix
  )
where

import Data.Aeson (Options (..), SumEncoding (..), defaultOptions)
import Data.Char (isLower, isUpper, toLower)

-- | Aeson options for the 'Request' sum type.
--
-- Constructor tag goes under the @op@ field; record fields are inlined
-- at the top level. Example shapes produced:
--
-- > ReqPing                         → {"op": "ping"}
-- > ReqVmShow {ref = ...}           → {"op": "vm_show", "ref": "..."}
-- > ReqVmCreate {name = ..., ...}   → {"op": "vm_create", "name": "...", "cpu_count": 2, ...}
requestOptions :: Options
requestOptions =
  defaultOptions
    { sumEncoding = TaggedObject {tagFieldName = "op", contentsFieldName = "args"}
    , constructorTagModifier = camelToSnake . dropPrefix "Req"
    , fieldLabelModifier = camelToSnake
    , omitNothingFields = True
    }

-- | Aeson options for the 'Response' sum type.
--
-- Tagged object keyed by @tag@; record fields inlined:
--
-- > RespPong                        → {"tag": "pong"}
-- > RespVmCreated {id = 42}         → {"tag": "vm_created", "id": 42}
-- > RespInvalidTransition {...}     → {"tag": "invalid_transition", "status": "running", "reason": "..."}
responseOptions :: Options
responseOptions =
  defaultOptions
    { sumEncoding = TaggedObject {tagFieldName = "tag", contentsFieldName = "contents"}
    , constructorTagModifier = camelToSnake . dropPrefix "Resp"
    , fieldLabelModifier = camelToSnake
    , omitNothingFields = True
    }

-- | Aeson options for inner response records (e.g. 'VmInfo',
-- 'StatusInfo', 'TemplateDetails'). These types are plain records, not
-- sum types, so no tag/constructor modifier is needed — only a field
-- name rewrite to keep the wire shape uniform with 'Request' /
-- 'Response'.
--
-- Field names are stripped of their lowercase prefix (@viId@ → @Id@,
-- @sdiReadOnly@ → @ReadOnly@) and then converted to @snake_case@. Each
-- inner record type uses its own prefix, but because the rule strips
-- all leading lowercase characters it handles every prefix uniformly.
innerOptions :: Options
innerOptions =
  defaultOptions
    { fieldLabelModifier = camelToSnake . dropLowerPrefix
    , omitNothingFields = True
    }

-- | Drop a leading prefix if present; otherwise return the string unchanged.
dropPrefix :: String -> String -> String
dropPrefix p s
  | take n s == p = drop n s
  | otherwise = s
  where
    n = length p

-- | Strip the leading run of lowercase characters. @"viId"@ → @"Id"@,
-- @"sniCreatedAt"@ → @"CreatedAt"@. Used by 'innerOptions' to remove
-- per-type field prefixes before 'camelToSnake'.
dropLowerPrefix :: String -> String
dropLowerPrefix = dropWhile isLower

-- | @camelCase@ / @PascalCase@ → @snake_case@.
--
-- Each capital letter (except a leading one) becomes "_<lowercase>"
-- so both first-character lowercase and first-character uppercase
-- inputs produce the same output.
camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (c : cs) = toLower c : go cs
  where
    go [] = []
    go (x : xs)
      | isUpper x = '_' : toLower x : go xs
      | otherwise = x : go xs
