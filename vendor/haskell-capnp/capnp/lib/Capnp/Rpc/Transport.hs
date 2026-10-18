{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module: Capnp.Rpc.Transport
-- Description: Support for exchanging messages with remote vats.
--
-- This module provides a 'Transport' type, which provides operations
-- used to transmit messages between vats in the RPC protocol.
module Capnp.Rpc.Transport
  ( Transport (..)
  , socketTransport
  )
where

import Capnp.Bits (WordCount)
import Capnp.IO (sGetMsg, sPutMsg)
import Capnp.Message (Message, Mutability (Const))
import Network.Socket (Socket)

-- | A @'Transport'@ handles transmitting RPC messages.
data Transport = Transport
  { sendMsg :: Message 'Const -> IO ()
  -- ^ Send a message
  , recvMsg :: IO (Message 'Const)
  -- ^ Receive a message
  }

-- | @'socketTransport' socket limit@ is a transport which reads and writes
-- messages to/from a socket. It uses @limit@ as the traversal limit when
-- reading messages and decoing.
socketTransport :: Socket -> WordCount -> Transport
socketTransport socket limit =
  Transport
    { sendMsg = sPutMsg socket
    , recvMsg = sGetMsg socket limit
    }
