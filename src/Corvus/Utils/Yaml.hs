{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Corvus.Utils.Yaml
  ( yamlQQ
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))

-- | Quasi-quoter for YAML with #{expression} interpolation.
-- Example: [yamlQQ|foo: #{bar}|]
-- This supports YAML syntax and produces an Aeson Value.
-- Interpolation only works for entire values (not inside strings).
yamlQQ :: QuasiQuoter
yamlQQ =
  QuasiQuoter
    { quoteExp = parseInterpolatedYaml
    , quotePat = const $ fail "yamlQQ: patterns not supported"
    , quoteType = const $ fail "yamlQQ: types not supported"
    , quoteDec = const $ fail "yamlQQ: declarations not supported"
    }

parseInterpolatedYaml :: String -> ExpQ
parseInterpolatedYaml s = do
  -- Replace #{expr} with placeholders
  let (replacedStr, interpolations) = extractInterpolations s
  -- Parse as YAML
  case Y.decodeEither' (T.encodeUtf8 $ T.pack replacedStr) of
    Left err -> fail $ "yamlQQ: failed to parse YAML: " ++ show err
    Right (val :: A.Value) ->
      -- Convert Value to Exp, re-inserting interpolations
      valueToExp interpolations val

extractInterpolations :: String -> (String, [(String, String)])
extractInterpolations = go 0
  where
    go :: Int -> String -> (String, [(String, String)])
    go _ [] = ([], [])
    go n str =
      let (before, rest) = break (== '#') str
       in case rest of
            ('#' : '{' : tailRest) ->
              let (expr, after) = break (== '}') tailRest
               in case after of
                    ('}' : restAfter) ->
                      let placeholder = "__CORVUS_INTERP_" ++ show n ++ "__"
                          (s', i') = go (n + 1) restAfter
                       in (before ++ placeholder ++ s', (placeholder, expr) : i')
                    _ -> (str, []) -- Unterminated
            ('#' : tailRest) ->
              let (s', i') = go n tailRest
               in (before ++ "#" ++ s', i')
            _ -> (str, [])

valueToExp :: [(String, String)] -> A.Value -> ExpQ
valueToExp interps = \case
  A.Object obj ->
    [|A.Object (KM.fromList $(listE $ map (pairToExp interps) (KM.toList obj)))|]
  A.Array arr ->
    [|A.Array (V.fromList $(listE $ map (valueToExp interps) (V.toList arr)))|]
  A.String t ->
    case lookup (T.unpack t) interps of
      Just exprStr ->
        case parseExp exprStr of
          Left err -> fail $ "yamlQQ: failed to parse expression " ++ exprStr ++ ": " ++ err
          Right e -> [|A.toJSON $(pure e)|]
      Nothing -> [|A.String t|]
  A.Number n -> [|A.Number n|]
  A.Bool b -> [|A.Bool b|]
  A.Null -> [|A.Null|]

pairToExp :: [(String, String)] -> (AK.Key, A.Value) -> ExpQ
pairToExp interps (k, v) = [|($(lift k), $(valueToExp interps v))|]
