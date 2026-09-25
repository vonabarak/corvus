module Corvus.CodeMetrics.Settings
  ( maximumModuleLines
  , maximumTopLevelDefinitionLines
  )
where

-- | Maximum physical lines in an authored Haskell module.
maximumModuleLines :: Int
maximumModuleLines = 1000

-- | Maximum source-span lines in an authored top-level definition.
maximumTopLevelDefinitionLines :: Int
maximumTopLevelDefinitionLines = 300
