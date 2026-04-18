-- | Generic Request→Response bridge for the Python FFI.
--
-- With 'FromJSON Request' and 'ToJSON Response' derived in
-- "Corvus.Protocol", this module is just a thin pass-through: parse the
-- incoming JSON into a 'Request', call 'sendRequest', encode the
-- 'Response' as JSON. No per-op dispatch table.
--
-- Adding a new RPC no longer requires editing this file: add the
-- constructor in 'Request', implement the handler server-side, and the
-- JSON round-trip works automatically.
module Corvus.Python.Call
  ( dispatch
  )
where

import Corvus.Client.Connection (Connection, sendRequest)
import Corvus.Protocol (Request, Response)
import Corvus.Python.Marshal (CallError, connToError)
import Data.Aeson (ToJSON (..), Value)

-- | Given a parsed 'Request', send it, translate the 'Response' to JSON,
-- and return the result (or a typed error for connection failures).
dispatch :: Connection -> Request -> IO (Either CallError Value)
dispatch conn req = do
  r <- sendRequest conn req
  pure $ case r of
    Right resp -> Right (toJSON (resp :: Response))
    Left e -> Left (connToError e)
