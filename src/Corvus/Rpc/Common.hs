{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared helpers for the Cap'n Proto cap implementations:
-- 'EntityRef' resolution and a small "throw if Left" combinator.
module Corvus.Rpc.Common
  ( -- * EntityRef → Ref bridge
    capnpRefToRef

    -- * Failures
  , failOnLeft
  , failNotFound
  , throwWireError
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Common as CGCommon
import Capnp.Rpc (throwFailed)
import qualified Corvus.Protocol as P
import Corvus.Wire.Common (EntityRef (..), fromCapnpEntityRef)
import Corvus.Wire.Errors (WireError, showWireError)
import Data.Text (Text)
import qualified Data.Text as T

-- | Translate the wire-level 'EntityRef' union into the existing
-- text-based 'Corvus.Protocol.Ref' that 'Corvus.Handlers.Resolve.*'
-- understands. Numeric ids become @show id@; names pass through.
capnpRefToRef :: C.Parsed CGCommon.EntityRef -> IO P.Ref
capnpRefToRef raw = case fromCapnpEntityRef raw of
  Right (RefById n) -> pure $ P.Ref (T.pack (show n))
  Right (RefByName t) -> pure $ P.Ref t
  Left e -> throwFailed (showWireError e)

failOnLeft :: Either Text a -> IO a
failOnLeft (Right a) = pure a
failOnLeft (Left e) = throwFailed e

failNotFound :: Text -> IO a
failNotFound what = throwFailed (what <> " not found")

throwWireError :: WireError -> IO a
throwWireError = throwFailed . showWireError
