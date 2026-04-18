{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | C-ABI entry points called from the Python extension stub
-- (cbits/python_ext.c). The stub owns the CPython module shell;
-- everything here is a @foreign export ccall@ function.
--
-- Two call-style entries:
--
-- * 'corvusCall' — one-shot: opens a connection per call. Envelope:
--   @{"transport": ..., "request": <Request JSON>}@.
--
-- * 'corvusOpen' + 'corvusCallOpen' + 'corvusClose' — persistent
--   connection. 'corvusOpen' takes a transport JSON and returns a
--   'StablePtr' wrapped in a 'PyCapsule'; 'corvusCallOpen' takes a bare
--   'Request' JSON and uses the existing connection.
module Corvus.Python
  ( corvusInit
  , corvusShutdown
  , corvusCall
  , corvusOpen
  , corvusCallOpen
  , corvusClose
  , corvusFreeBuffer
  )
where

import Control.Exception (SomeException, try)
import Corvus.Client.Connection
  ( ConnectionError
  , withTcpConnection
  , withUnixConnection
  )
import Corvus.Protocol (Request)
import Corvus.Python.Call (dispatch)
import Corvus.Python.Connection (ConnHandle)
import qualified Corvus.Python.Connection as PC
import Corvus.Python.Marshal
  ( CallError (..)
  , OneShotEnvelope (..)
  , Transport (..)
  , connToError
  , decodeOneShot
  , decodeRequestOnly
  , decodeTransport
  , encodeEnvelopeError
  , encodeErr
  , encodeOk
  )
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr
  ( StablePtr
  , castPtrToStablePtr
  , castStablePtrToPtr
  , deRefStablePtr
  , freeStablePtr
  , newStablePtr
  )
import Foreign.Storable (poke)

-- | Exit codes. 0 = response bytes delivered (parse Python-side for
-- ok/err). Nonzero = catastrophic (out buffer untouched; do not free).
resultDelivered, resultInternalError, resultOpenFailed :: CInt
resultDelivered = 0
resultInternalError = 1
resultOpenFailed = 2

corvusInit :: IO ()
corvusInit = pure ()

corvusShutdown :: IO ()
corvusShutdown = pure ()

foreign export ccall corvusInit :: IO ()
foreign export ccall corvusShutdown :: IO ()

-- ---------------------------------------------------------------------------
-- One-shot entry: opens connection per call
-- ---------------------------------------------------------------------------

corvusCall
  :: Ptr Word8
  -> CSize
  -> Ptr (Ptr Word8)
  -> Ptr CSize
  -> IO CInt
corvusCall inPtr inLen outPtrPtr outLenPtr = do
  result <- try @SomeException $ do
    input <- BSU.unsafePackCStringLen (castPtr inPtr, fromIntegral inLen)
    output <- handleOneShot input
    writeBuffer output outPtrPtr outLenPtr
  case result of
    Right () -> pure resultDelivered
    Left _ -> do
      poke outPtrPtr nullPtr
      poke outLenPtr 0
      pure resultInternalError

foreign export ccall
  corvusCall
    :: Ptr Word8
    -> CSize
    -> Ptr (Ptr Word8)
    -> Ptr CSize
    -> IO CInt

-- ---------------------------------------------------------------------------
-- Persistent-connection entries
-- ---------------------------------------------------------------------------

corvusOpen
  :: Ptr Word8
  -> CSize
  -> Ptr (Ptr ())
  -> Ptr (Ptr Word8)
  -> Ptr CSize
  -> IO CInt
corvusOpen inPtr inLen outHandle outErrBuf outErrLen = do
  result <- try @SomeException $ do
    input <- BSU.unsafePackCStringLen (castPtr inPtr, fromIntegral inLen)
    case decodeTransport input of
      Left msg -> pure (Left (CallError "bad_transport" (object ["message" .= T.pack msg])))
      Right t -> do
        h <- openByTransport t
        pure (either (Left . connToError) Right h)
  case result of
    Left _ -> do
      poke outHandle nullPtr
      poke outErrBuf nullPtr
      poke outErrLen 0
      pure resultInternalError
    Right (Left err) -> do
      writeBuffer (encodeErr err) outErrBuf outErrLen
      poke outHandle nullPtr
      pure resultOpenFailed
    Right (Right handle) -> do
      sp <- newStablePtr handle
      poke outHandle (castStablePtrToPtr sp)
      poke outErrBuf nullPtr
      poke outErrLen 0
      pure resultDelivered

foreign export ccall
  corvusOpen
    :: Ptr Word8
    -> CSize
    -> Ptr (Ptr ())
    -> Ptr (Ptr Word8)
    -> Ptr CSize
    -> IO CInt

-- | Close and release a persistent connection. Wrapped in 'try' end-to-end
-- so no Haskell exception can leak across the FFI boundary and crash the
-- Python interpreter — including 'deRefStablePtr' / 'freeStablePtr'
-- themselves, which are undefined behaviour if invoked on a stale pointer.
corvusClose :: Ptr () -> IO ()
corvusClose p
  | p == nullPtr = pure ()
  | otherwise = do
      _ <- try @SomeException $ do
        let sp = castPtrToStablePtr p :: StablePtr ConnHandle
        h <- deRefStablePtr sp
        _ <- try @SomeException (PC.closeConn h)
        freeStablePtr sp
      pure ()

foreign export ccall corvusClose :: Ptr () -> IO ()

corvusCallOpen
  :: Ptr ()
  -> Ptr Word8
  -> CSize
  -> Ptr (Ptr Word8)
  -> Ptr CSize
  -> IO CInt
corvusCallOpen handlePtr inPtr inLen outPtrPtr outLenPtr = do
  result <- try @SomeException $ do
    input <- BSU.unsafePackCStringLen (castPtr inPtr, fromIntegral inLen)
    let sp = castPtrToStablePtr handlePtr :: StablePtr ConnHandle
    handle <- deRefStablePtr sp
    output <- case decodeRequestOnly input of
      Left err -> pure (encodeEnvelopeError (T.pack err))
      Right req -> do
        r <- PC.withConn handle (`dispatch` req)
        pure (serialiseDispatch r)
    writeBuffer output outPtrPtr outLenPtr
  case result of
    Right () -> pure resultDelivered
    Left _ -> do
      poke outPtrPtr nullPtr
      poke outLenPtr 0
      pure resultInternalError

foreign export ccall
  corvusCallOpen
    :: Ptr ()
    -> Ptr Word8
    -> CSize
    -> Ptr (Ptr Word8)
    -> Ptr CSize
    -> IO CInt

corvusFreeBuffer :: Ptr Word8 -> IO ()
corvusFreeBuffer = free

foreign export ccall corvusFreeBuffer :: Ptr Word8 -> IO ()

-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

handleOneShot :: BS.ByteString -> IO BS.ByteString
handleOneShot input = case decodeOneShot input of
  Left err -> pure (encodeEnvelopeError (T.pack err))
  Right (OneShotEnvelope tr req) -> runOnTransport tr req

runOnTransport :: Transport -> Request -> IO BS.ByteString
runOnTransport (TransportUnix path) req = do
  r <- withUnixConnection path (`dispatch` req)
  pure (serialiseConn r)
runOnTransport (TransportTcp host port) req = do
  r <- withTcpConnection host port (`dispatch` req)
  pure (serialiseConn r)

serialiseConn :: Either ConnectionError (Either CallError Value) -> BS.ByteString
serialiseConn (Right (Right v)) = encodeOk v
serialiseConn (Right (Left e)) = encodeErr e
serialiseConn (Left e) = encodeErr (connToError e)

serialiseDispatch :: Either CallError Value -> BS.ByteString
serialiseDispatch (Right v) = encodeOk v
serialiseDispatch (Left e) = encodeErr e

openByTransport :: Transport -> IO (Either ConnectionError ConnHandle)
openByTransport (TransportUnix path) = PC.openUnix path
openByTransport (TransportTcp host port) = PC.openTcp host port

writeBuffer :: BS.ByteString -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
writeBuffer bs outPtrPtr outLenPtr = do
  let n = BS.length bs
  buf <- mallocBytes n
  BSU.unsafeUseAsCString bs $ \src ->
    copyBytes buf (castPtr src) n
  poke outPtrPtr buf
  poke outLenPtr (fromIntegral n)
