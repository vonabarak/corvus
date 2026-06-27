{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build-time variable substitution for @crv build@ YAML.
--
-- The build YAML may carry a top-level @vars:@ block that declares
-- the variables the document expects, with optional defaults.
-- References elsewhere in the document use Jinja-style @{{ name }}@.
-- The complete substitution pipeline is:
--
-- @
-- decoded Value
--   ─▶ extractVarsBlock   (lift the top-level vars: map, returning
--                          the declared set and the document sans vars:)
--   ─▶ resolveVars        (merge declarations with --var / --var-file;
--                          enforce required, reject undeclared)
--   ─▶ substituteValue    (walk strings and expand {{ name }})
-- @
--
-- The pass is **client-side only**: the document handed off to the
-- daemon is already fully substituted, with @vars:@ stripped, so the
-- daemon stays unaware of variables.
module Corvus.Client.BuildVars
  ( applyBuildVars
  , VarError (..)
  , renderVarError
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value (..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

-- ---------------------------------------------------------------------------
-- Errors

-- | All the ways variable resolution / substitution can fail.
-- 'renderVarError' formats one for an operator.
data VarError
  = -- | The top-level @vars:@ block isn't an object.
    VarsBlockNotObject
  | -- | A @vars:@ entry's value isn't a string, null, or omitted.
    VarsBlockBadValue Text
  | -- | A variable was declared with no default and not supplied on CLI.
    VarUnsetRequired Text
  | -- | A @{{ ref }}@ in the document names a variable not declared in @vars:@.
    VarUnknownRef
      Text
      Text
      -- ^ name, ~40-char excerpt
  | -- | A @{{@ in the document is missing its closing @}}@.
    VarMalformedRef
      Text
      -- ^ ~40-char excerpt
  | -- | A CLI @--var KEY=VALUE@ names a variable not declared in @vars:@.
    VarUndeclaredCli Text
  | -- | A @--var-file@ couldn't be parsed.
    VarFileParse FilePath Text
  | -- | A @--var-file@ couldn't be read.
    VarFileRead FilePath Text
  deriving (Show, Eq)

renderVarError :: VarError -> Text
renderVarError = \case
  VarsBlockNotObject ->
    "build vars: the top-level `vars:` key must be a mapping"
  VarsBlockBadValue k ->
    "build vars: `vars." <> k <> "` must be a string or null (use null for required)"
  VarUnsetRequired k ->
    "build vars: `" <> k <> "` is required (declared in `vars:` with no default); set it with --var " <> k <> "=VALUE"
  VarUnknownRef k ctx ->
    "build vars: `{{ " <> k <> " }}` references an undeclared variable (near \"" <> ctx <> "\"); add it to the YAML's `vars:` block"
  VarMalformedRef ctx ->
    "build vars: malformed `{{ ... }}` reference (no closing braces) near \"" <> ctx <> "\""
  VarUndeclaredCli k ->
    "build vars: --var " <> k <> "=... is not declared in the YAML's `vars:` block"
  VarFileParse p msg ->
    "build vars: failed to parse --var-file " <> T.pack p <> ": " <> msg
  VarFileRead p msg ->
    "build vars: failed to read --var-file " <> T.pack p <> ": " <> msg

-- ---------------------------------------------------------------------------
-- Public entry point

-- | Apply variable substitution to a parsed YAML root.
--
-- The output 'Value' has its @vars:@ key removed (if present); only
-- variables declared in that block are accepted from the CLI, and
-- only @{{ name }}@ references to declared variables are accepted in
-- the document. Returns 'Right' iff every reference resolves.
applyBuildVars
  :: [(Text, Text)]
  -- ^ CLI @--var KEY=VALUE@ in order (last wins on collision).
  -> [FilePath]
  -- ^ CLI @--var-file@ paths in order (later file wins on collision; both lose to @--var@).
  -> Value
  -- ^ parsed YAML root
  -> IO (Either VarError Value)
applyBuildVars cliVars varFiles root = do
  fileResult <- loadVarFiles varFiles
  case fileResult of
    Left e -> pure (Left e)
    Right fromFiles -> pure $ do
      (declared, root') <- extractVarsBlock root
      resolved <- resolveVars declared fromFiles cliVars
      substituteValue resolved root'

-- ---------------------------------------------------------------------------
-- vars: block extraction

-- | Lift the top-level @vars:@ key off the root object, returning a
-- map of declared variables (with their optional defaults) and the
-- document with the @vars:@ key removed. A missing @vars:@ key
-- yields an empty declaration set.
extractVarsBlock :: Value -> Either VarError (Map Text (Maybe Text), Value)
extractVarsBlock (Object o) = case KM.lookup "vars" o of
  Nothing -> Right (Map.empty, Object o)
  Just (Object vo) -> do
    decl <- foldM step Map.empty (KM.toList vo)
    let stripped = KM.delete "vars" o
    Right (decl, Object stripped)
  Just Null -> Right (Map.empty, Object (KM.delete "vars" o))
  Just _ -> Left VarsBlockNotObject
  where
    step acc (k, v) = do
      let name = AK.toText k
      val <- declValue name v
      Right (Map.insert name val acc)
    declValue _ Null = Right Nothing
    declValue _ (String t) = Right (Just t)
    declValue _ (Number n) = Right (Just (numberToText n))
    declValue _ (Bool b) = Right (Just (if b then "true" else "false"))
    declValue name _ = Left (VarsBlockBadValue name)
extractVarsBlock v = Right (Map.empty, v) -- non-object roots: no vars to lift

-- | Render an Aeson 'Sci.Scientific' as an unambiguous decimal text
-- (used for default values in @vars:@ that the YAML parser deserialised as numbers).
numberToText :: Sci.Scientific -> Text
numberToText n = case Sci.floatingOrInteger n :: Either Double Integer of
  Right i -> T.pack (show i)
  Left _ -> T.pack (Sci.formatScientific Sci.Generic Nothing n)

-- ---------------------------------------------------------------------------
-- Variable resolution

-- | Merge declared defaults with values from @--var-file@ and
-- @--var@, in that precedence order (lowest first). Errors out on
-- undeclared CLI names or required-no-default variables left unset.
resolveVars
  :: Map Text (Maybe Text)
  -> Map Text Text
  -> [(Text, Text)]
  -> Either VarError (Map Text Text)
resolveVars declared fromFiles cliVars = do
  -- Reject CLI names not declared in vars:
  let cliNames = map fst cliVars
      fileNames = Map.keys fromFiles
      unknownCli =
        filter (`Map.notMember` declared) (cliNames ++ fileNames)
  case unknownCli of
    (n : _) -> Left (VarUndeclaredCli n)
    [] -> do
      let defaults = Map.mapMaybe id declared
          merged =
            foldl
              (\m (k, v) -> Map.insert k v m)
              (Map.union fromFiles defaults)
              cliVars
          missing =
            [ k | (k, Nothing) <- Map.toList declared, k `Map.notMember` merged
            ]
      case missing of
        (k : _) -> Left (VarUnsetRequired k)
        [] -> Right merged

-- ---------------------------------------------------------------------------
-- Substitution

-- | Walk the Value tree expanding @{{ name }}@ in every string leaf.
-- Keys and non-string scalars are left untouched.
substituteValue :: Map Text Text -> Value -> Either VarError Value
substituteValue vars = go
  where
    go (String t) = String <$> substituteText vars t
    go (Array xs) = Array <$> traverse go xs
    go (Object o) = Object <$> traverse go o
    go v = Right v

-- | Expand @{{ name }}@ references in @t@. @{{{{@ and @}}}}@ escape
-- literal delimiters, producing @{{@ and @}}@ respectively. Shell-style
-- variables such as @${HOME}@ are passed through verbatim.
substituteText :: Map Text Text -> Text -> Either VarError Text
substituteText vars = fmap T.concat . loop
  where
    loop :: Text -> Either VarError [Text]
    loop t
      | T.null t = Right []
      | "{{{{" `T.isPrefixOf` t = ("{{" :) <$> loop (T.drop 4 t)
      | "}}}}" `T.isPrefixOf` t = ("}}" :) <$> loop (T.drop 4 t)
      | "{{" `T.isPrefixOf` t =
          let rest = T.drop 2 t
           in case T.breakOn "}}" rest of
                (_, post) | T.null post -> Left (VarMalformedRef (excerpt t))
                (rawName, post) -> do
                  let after = T.drop 2 post
                      name = T.strip rawName
                  value <- lookupVar name (excerpt t)
                  (value :) <$> loop after
      | otherwise =
          let (lit, rest) = T.splitAt 1 t
           in (lit :) <$> loop rest

    lookupVar name ctx
      | not (isIdent name) = Left (VarUnknownRef name ctx)
      | otherwise = case Map.lookup name vars of
          Just v -> Right v
          Nothing -> Left (VarUnknownRef name ctx)

    isIdent t = case T.uncons t of
      Nothing -> False
      Just (c, rest) ->
        (isAlpha c || c == '_') && T.all (\x -> isAlphaNum x || x == '_') rest

    excerpt = T.take 40

-- ---------------------------------------------------------------------------
-- --var-file loading

loadVarFiles :: [FilePath] -> IO (Either VarError (Map Text Text))
loadVarFiles = go Map.empty
  where
    go acc [] = pure (Right acc)
    go acc (p : ps) = do
      r <- loadVarFile p
      case r of
        Left e -> pure (Left e)
        Right m -> go (Map.union m acc) ps -- later file wins

loadVarFile :: FilePath -> IO (Either VarError (Map Text Text))
loadVarFile p = do
  bs <- try (Yaml.decodeFileEither p) :: IO (Either SomeException (Either Yaml.ParseException Value))
  case bs of
    Left ioe -> pure (Left (VarFileRead p (T.pack (show ioe))))
    Right (Left perr) -> pure (Left (VarFileParse p (T.pack (Yaml.prettyPrintParseException perr))))
    Right (Right v) -> pure (toMap v)
  where
    toMap (Object o) = foldM step Map.empty (KM.toList o)
    toMap Null = Right Map.empty
    toMap _ = Left (VarFileParse p "expected a top-level mapping of string keys")
    step acc (k, v) =
      let name = AK.toText k
       in case v of
            String t -> Right (Map.insert name t acc)
            Number n -> Right (Map.insert name (numberToText n) acc)
            Bool b -> Right (Map.insert name (if b then "true" else "false") acc)
            Null -> Right acc
            _ -> Left (VarFileParse p ("value of `" <> name <> "` must be a scalar"))
