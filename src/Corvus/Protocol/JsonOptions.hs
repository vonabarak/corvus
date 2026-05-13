-- | Shared Aeson 'Options' for the response data types in
-- 'Corvus.Protocol.*'. The legacy 'Corvus.Protocol.Aeson' (Request /
-- Response sum-type options + Python-client schema reflection) is
-- gone — only the inner-record options survive, and they're tiny.
--
-- The shape: drop the lowercase prefix on each field name (@viId@ →
-- @Id@) and convert the remainder to snake_case (@Id@ → @id@,
-- @AttachedTo@ → @attached_to@). 'Nothing' values are omitted rather
-- than serialised as @null@.
module Corvus.Protocol.JsonOptions
  ( innerOptions
  )
where

import Data.Aeson (Options (..), defaultOptions)
import Data.Char (isLower, isUpper, toLower)

-- | Aeson options used by every inner response record
-- ('VmInfo', 'StatusInfo', 'TemplateDetails', ...).
innerOptions :: Options
innerOptions =
  defaultOptions
    { fieldLabelModifier = camelToSnake . dropLowerPrefix
    , omitNothingFields = True
    }

-- | Drop leading lowercase characters.
dropLowerPrefix :: String -> String
dropLowerPrefix = dropWhile isLower

-- | CamelCase → snake_case. The first character is lowercased
-- unconditionally; subsequent capitals get an underscore prefix.
camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (c : cs) = toLower c : go cs
  where
    go [] = []
    go (x : xs)
      | isUpper x = '_' : toLower x : go xs
      | otherwise = x : go xs
