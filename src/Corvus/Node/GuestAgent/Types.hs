module Corvus.Node.GuestAgent.Types
  ( GuestExecResult (..)
  , GuestIpAddress (..)
  , GuestNetIf (..)
  , ChunkSink
  )
where

import qualified Data.ByteString as BS
import Data.Text (Text)

data GuestExecResult
  = GuestExecSuccess !Int !Text !Text
  | GuestExecError !Text
  | GuestExecConnectionFailed !Text
  deriving (Eq, Show)

data GuestIpAddress = GuestIpAddress
  { giaType :: !Text
  , giaAddress :: !Text
  , giaPrefix :: !Int
  }
  deriving (Eq, Show)

data GuestNetIf = GuestNetIf
  { gniHardwareAddress :: !Text
  , gniIpAddresses :: ![GuestIpAddress]
  }
  deriving (Eq, Show)

type ChunkSink = BS.ByteString -> IO ()
