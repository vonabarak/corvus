{-# LANGUAGE TemplateHaskell #-}

-- | QMP quasi-quoter for building QMP commands as ByteStrings.
-- Separated from Qmp.hs due to Template Haskell stage restriction.
module Corvus.Qemu.QmpQQ
  ( qmpQQ,
    jsonToCmd,
  )
where

import Data.Aeson (Value, encode)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | Quasi-quoter for QMP commands. Parses JSON and converts to ByteString.
-- Usage: [qmpQQ| { "execute": "system_powerdown" } |]
-- Supports variable interpolation with #{varName} syntax.
qmpQQ :: QuasiQuoter
qmpQQ =
  QuasiQuoter
    { quoteExp = \s -> [|jsonToCmd $(quoteExp aesonQQ s)|],
      quotePat = const $ fail "qmpQQ: patterns not supported",
      quoteType = const $ fail "qmpQQ: types not supported",
      quoteDec = const $ fail "qmpQQ: declarations not supported"
    }

-- | Convert a JSON Value to a ByteString command (with newline)
jsonToCmd :: Value -> BS.ByteString
jsonToCmd = BL.toStrict . (<> "\n") . encode
