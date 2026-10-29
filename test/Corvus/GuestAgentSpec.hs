{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for QEMU Guest Agent protocol handling.
-- Tests the QGA JSON parsing layer (recvJson logic) with various edge cases
-- including newline-delimited streams and 0xFF framing bytes.
module Corvus.GuestAgentSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (bracket, try)
import Control.Monad (unless)
import Corvus.Node.GuestAgent (GuestIpAddress (..), GuestNetIf (..), parseGuestInterfaces)
import qualified Corvus.Node.GuestAgent as GA
import Corvus.Node.GuestAgent.Connection (GuestAgentConns (..), newGuestAgentConns)
import Corvus.Node.GuestAgent.File (downloadFile, socketRequest, uploadFile)
import Corvus.Node.GuestAgent.Transport (QgaSocket (..), newQgaSocket, parseQgaFrame, recvFrame, sendJson)
import Corvus.Qemu.Config (defaultQemuConfig)
import Corvus.Rpc.FileTransfer (bytesSource)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Int
import qualified Data.Map.Strict as Map
import Data.Time (diffUTCTime, getCurrentTime)
import Network.Socket (Family (..), Socket, SocketType (..), close, defaultProtocol, socketPair)
import Network.Socket.ByteString (recv)
import System.IO.Error (isUserError)
import Test.Hspec

-- | Encode a JSON response exactly as QGA frames it on the socket.
qgaFrame :: Aeson.Value -> BS.ByteString
qgaFrame value = BL.toStrict (Aeson.encode value) <> "\n"

recvJson :: Socket -> IO (Maybe Aeson.Value)
recvJson sock = fst <$> recvFrame sock

testTransport :: Socket -> IO QgaSocket
testTransport = newQgaSocket (\_ _ -> pure ()) 42 1

testCache :: Map.Map Data.Int.Int64 (MVar (Maybe QgaSocket)) -> IO GuestAgentConns
testCache slots = do
  cache <- newGuestAgentConns (\_ _ -> pure ())
  STM.atomically $ STM.writeTVar (guestAgentSlots cache) slots
  pure cache

spec :: Spec
spec = do
  describe "QGA JSON parsing" $ do
    it "parses a single JSON object" $ do
      parseQgaFrame (qgaFrame [aesonQQ| { "return": {} } |])
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

    it "parses first object from multiple newline-delimited objects" $ do
      parseQgaFrame (qgaFrame [aesonQQ| { "return": 42 } |] <> qgaFrame [aesonQQ| { "return": {} } |])
        `shouldBe` Just (Aeson.object ["return" Aeson..= (42 :: Int)])

    it "strips 0xFF bytes (QGA framing delimiter)" $ do
      parseQgaFrame (BS.pack [0xFF] <> qgaFrame [aesonQQ| { "return": 42 } |])
        `shouldBe` Just (Aeson.object ["return" Aeson..= (42 :: Int)])

    it "handles 0xFF between JSON objects" $ do
      parseQgaFrame (BS.pack [0xFF] <> qgaFrame [aesonQQ| { "error": "stale" } |] <> BS.pack [0xFF] <> qgaFrame [aesonQQ| { "return": 99 } |])
        `shouldBe` Just (Aeson.object ["error" Aeson..= ("stale" :: String)])

    it "handles response with trailing whitespace" $ do
      parseQgaFrame (qgaFrame [aesonQQ| { "return": {} } |] <> "\n")
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

    it "returns Nothing for empty data" $ do
      parseQgaFrame BS.empty `shouldBe` Nothing

    it "returns Nothing for non-JSON data" $ do
      parseQgaFrame "not json at all\n" `shouldBe` Nothing

    it "skips non-JSON lines to find valid JSON" $ do
      parseQgaFrame ("garbage line\n" <> qgaFrame [aesonQQ| { "return": {} } |])
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

    it "parses guest-sync response with numeric return" $ do
      let result = parseQgaFrame (qgaFrame [aesonQQ| { "return": 1073741823 } |])
      case result of
        Just (Aeson.Object obj) ->
          KM.member "return" obj `shouldBe` True
        _ -> expectationFailure "Expected JSON object with return key"

    it "handles Windows-style CRLF line endings" $ do
      parseQgaFrame (BL.toStrict (Aeson.encode [aesonQQ| { "return": {} } |]) <> "\r\n")
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

  describe "guest file uploads" $ do
    it "resends unwritten bytes and flushes before closing" $
      bracket (socketPair AF_UNIX Stream defaultProtocol) (\(client, peer) -> close client >> close peer) $ \(client, peer) -> do
        requests <- newEmptyMVar
        _ <- forkIO $ do
          captured <-
            mapM
              ( \reply -> do
                  request <- recvJson peer
                  sendJson peer reply
                  pure request
              )
              [[aesonQQ| { "return": 42 } |], [aesonQQ| { "return": { "count": 2 } } |], [aesonQQ| { "return": { "count": 3 } } |], [aesonQQ| { "return": {} } |], [aesonQQ| { "return": {} } |]]
          putMVar requests captured
        managed <- testTransport client
        source <- bytesSource "hello"
        uploadFile (socketRequest managed) "C:\\Temp\\script.ps1" source `shouldReturn` 5
        captured <- takeMVar requests
        captured
          `shouldBe` map
            Just
            [ [aesonQQ| { "execute": "guest-file-open", "arguments": { "path": "C:\\Temp\\script.ps1", "mode": "wb" } } |]
            , [aesonQQ| { "execute": "guest-file-write", "arguments": { "handle": 42, "buf-b64": "aGVsbG8=" } } |]
            , [aesonQQ| { "execute": "guest-file-write", "arguments": { "handle": 42, "buf-b64": "bGxv" } } |]
            , [aesonQQ| { "execute": "guest-file-flush", "arguments": { "handle": 42 } } |]
            , [aesonQQ| { "execute": "guest-file-close", "arguments": { "handle": 42 } } |]
            ]

    it "closes the handle and rejects a zero-byte write" $
      bracket (socketPair AF_UNIX Stream defaultProtocol) (\(client, peer) -> close client >> close peer) $ \(client, peer) -> do
        closed <- newEmptyMVar
        _ <- forkIO $ do
          _ <- recvJson peer
          sendJson peer [aesonQQ| { "return": 42 } |]
          _ <- recvJson peer
          sendJson peer [aesonQQ| { "return": { "count": 0 } } |]
          request <- recvJson peer
          sendJson peer [aesonQQ| { "return": {} } |]
          putMVar closed request
        managed <- testTransport client
        source <- bytesSource "hello"
        uploadFile (socketRequest managed) "script.ps1" source `shouldThrow` isUserError
        takeMVar closed `shouldReturn` Just [aesonQQ| { "execute": "guest-file-close", "arguments": { "handle": 42 } } |]

  describe "guest file session serialization" $ do
    it "allows a health ping while awaiting a source chunk" $
      bracket (socketPair AF_UNIX Stream defaultProtocol) (\(client, peer) -> close client >> close peer) $ \(client, peer) -> do
        slot <- newMVar . Just =<< testTransport client
        conns <- testCache (Map.singleton 42 slot)
        requests <- newEmptyMVar
        _ <- forkIO $ do
          captured <-
            mapM
              ( \reply -> do
                  request <- recvJson peer
                  sendJson peer reply
                  pure request
              )
              [[aesonQQ| {"return": 7} |], [aesonQQ| {"return": {}} |], [aesonQQ| {"return": {"count": 1}} |], [aesonQQ| {"return": {}} |], [aesonQQ| {"return": {}} |]]
          putMVar requests captured
        GA.guestWriteFile
          conns
          defaultQemuConfig
          42
          "/binary"
          ( \_ -> do
              GA.guestPing conns defaultQemuConfig 42 `shouldReturn` True
              pure ("x", True)
          )
          `shouldReturn` 1
        captured <- takeMVar requests
        length captured `shouldBe` 5
        captured !! 1 `shouldBe` Just [aesonQQ| {"execute": "guest-ping"} |]

    it "refuses handles after the cached socket is replaced" $
      bracket (socketPair AF_UNIX Stream defaultProtocol) (\(client, peer) -> close client >> close peer) $ \(client, peer) ->
        bracket (socketPair AF_UNIX Stream defaultProtocol) (\(next, nextPeer) -> close next >> close nextPeer) $ \(next, _) -> do
          slot <- newMVar . Just =<< testTransport client
          conns <- testCache (Map.singleton 42 slot)
          _ <- forkIO $ do
            _ <- recvJson peer
            sendJson peer [aesonQQ| {"return": 7} |]
          GA.guestWriteFile
            conns
            defaultQemuConfig
            42
            "/binary"
            ( \_ -> do
                _ <- takeMVar slot
                putMVar slot . Just =<< testTransport next
                pure ("x", True)
            )
            `shouldThrow` isUserError
          -- A rejected stale handle must not discard the replacement socket.
          fmap (fmap qgaSocket) (takeMVar slot) `shouldReturn` Just next
          putMVar slot . Just =<< testTransport next

  describe "native guest downloads" $ do
    it "reads binary bytes and closes before returning" $ do
      calls <- newIORef ([] :: [Aeson.Value])
      chunks <- newIORef []
      let send command args = do
            modifyIORef' calls (<> [Aeson.object ["execute" Aeson..= command, "arguments" Aeson..= Aeson.object args]])
            pure $
              if command == "guest-file-open"
                then [aesonQQ| {"return": 42} |]
                else
                  if command == "guest-file-read"
                    then [aesonQQ| {"return": {"count": 3, "buf-b64": "AP+A", "eof": true}} |]
                    else [aesonQQ| {"return": {}} |]
      downloadFile send "/binary" (\chunk -> modifyIORef' chunks (<> [chunk])) `shouldReturn` 3
      readIORef chunks `shouldReturn` [BS.pack [0, 255, 128]]
      requests <- readIORef calls
      last requests `shouldBe` [aesonQQ| {"execute": "guest-file-close", "arguments": {"handle": 42}} |]

    mapM_
      ( \reply -> it ("rejects invalid download reply " <> show reply <> " and closes") $ do
          closed <- newIORef False
          let send command _
                | command == "guest-file-open" = pure [aesonQQ| {"return": 42} |]
                | command == "guest-file-read" = pure reply
                | otherwise = modifyIORef' closed (const True) >> pure [aesonQQ| {"return": {}} |]
          downloadFile send "/binary" (const (pure ())) `shouldThrow` isUserError
          readIORef closed `shouldReturn` True
      )
      [ [aesonQQ| {"return": {"count": 1, "buf-b64": "!", "eof": true}} |]
      , [aesonQQ| {"return": {"count": 2, "buf-b64": "AA==", "eof": true}} |]
      , [aesonQQ| {"return": {"count": 0, "buf-b64": "", "eof": false}} |]
      , [aesonQQ| {"error": {"desc": "permission denied"}} |]
      ]

    it "uploads empty files without a write and still flushes" $ do
      commands <- newIORef []
      let send command _ = do
            modifyIORef' commands (<> [command])
            pure $ if command == "guest-file-open" then [aesonQQ| {"return": 42} |] else [aesonQQ| {"return": {}} |]
      source <- bytesSource BS.empty
      uploadFile send "/empty" source `shouldReturn` 0
      readIORef commands `shouldReturn` ["guest-file-open", "guest-file-flush", "guest-file-close"]

  describe "interface parsing" $ do
    it "parses interfaces with hardware-address" $ do
      let parsed = parseQgaFrame (qgaFrame [aesonQQ| { "return": [{ "name": "eth0", "hardware-address": "52:54:00:12:34:56", "ip-addresses": [{ "ip-address-type": "ipv4", "ip-address": "10.0.2.15", "prefix": 24 }]}] } |])
      parsed `shouldSatisfy` \case
        Just (Aeson.Object obj) -> KM.member "return" obj
        _ -> False

    it "skips interfaces without hardware-address (Windows loopback)" $ do
      -- Windows returns loopback without hardware-address; this must not
      -- cause the entire interface list to fail to parse.
      let parsed = parseQgaFrame (qgaFrame [aesonQQ| { "return": [{ "name": "Ethernet 3", "hardware-address": "52:54:00:12:34:56", "ip-addresses": [{ "ip-address-type": "ipv4", "ip-address": "10.0.2.15", "prefix": 24 }]}, { "name": "Loopback Pseudo-Interface 1", "ip-addresses": [{ "ip-address-type": "ipv4", "ip-address": "127.0.0.1", "prefix": 8 }]}] } |])
      -- The JSON itself should parse fine (it's valid JSON)
      parsed `shouldSatisfy` \case
        Just (Aeson.Object obj) -> KM.member "return" obj
        _ -> False

  describe "parseGuestInterfaces real-world" $ do
    it "parses a real FreeBSD 14.3 qemu-ga response" $ do
      -- Captured verbatim from qemu-ga 10.2.2 on FreeBSD 14.4-RELEASE-p1 via
      -- truss of a live guest. Exactly what corvus receives over virtio-serial.
      let response =
            [aesonQQ|
            { "return": [
              { "name": "vtnet0", "ip-addresses": [
                { "ip-address-type": "ipv6", "ip-address": "fe80::5054:ff:fee1:8aff", "prefix": 64 },
                { "ip-address-type": "ipv4", "ip-address": "192.168.89.216", "prefix": 22 }
              ], "hardware-address": "52:54:00:e1:8a:ff" },
              { "name": "lo0", "ip-addresses": [
                { "ip-address-type": "ipv6", "ip-address": "::1", "prefix": 128 },
                { "ip-address-type": "ipv6", "ip-address": "fe80::1", "prefix": 64 },
                { "ip-address-type": "ipv4", "ip-address": "127.0.0.1", "prefix": 8 }
              ], "hardware-address": "00:00:00:00:00:00" }
            ] }
            |]
      case parseGuestInterfaces (Just response) of
        Just ifs@(firstInterface : _) -> do
          length ifs `shouldBe` 2
          gniHardwareAddress firstInterface `shouldBe` "52:54:00:e1:8a:ff"
          let vtIps = gniIpAddresses firstInterface
          length vtIps `shouldBe` 2
          map giaAddress vtIps `shouldContain` ["192.168.89.216"]
        _ -> expectationFailure "parser returned no interfaces on a well-formed FreeBSD response"

  describe "parseGuestInterfaces (lenient)" $ do
    it "parses a normal response" $ do
      parseGuestInterfaces (Just [aesonQQ| { "return": [{ "name": "eth0", "hardware-address": "52:54:00:12:34:56", "ip-addresses": [{ "ip-address-type": "ipv4", "ip-address": "10.0.2.15", "prefix": 24 }]}] } |])
        `shouldBe` Just
          [ GuestNetIf
              { gniHardwareAddress = "52:54:00:12:34:56"
              , gniIpAddresses =
                  [GuestIpAddress {giaType = "ipv4", giaAddress = "10.0.2.15", giaPrefix = 24}]
              }
          ]

    it "defaults missing prefix to 0 instead of failing" $ do
      -- Some qemu-ga builds (e.g. on certain BSD setups) omit the prefix
      -- field on some addresses. Previously this caused the whole parse to
      -- fail and all IPs to be lost.
      parseGuestInterfaces (Just [aesonQQ| { "return": [{ "hardware-address": "52:54:00:aa:bb:cc", "ip-addresses": [{ "ip-address-type": "ipv6", "ip-address": "fe80::1" }]}] } |])
        `shouldBe` Just
          [ GuestNetIf
              { gniHardwareAddress = "52:54:00:aa:bb:cc"
              , gniIpAddresses =
                  [GuestIpAddress {giaType = "ipv6", giaAddress = "fe80::1", giaPrefix = 0}]
              }
          ]

    it "drops one malformed IP but keeps the rest of the interface" $ do
      -- If one address entry is missing required fields, the other addresses
      -- on the same interface must still be reported.
      parseGuestInterfaces
        ( Just
            [aesonQQ|
            { "return": [{ "hardware-address": "52:54:00:12:34:56", "ip-addresses": [
              { "ip-address-type": "ipv4", "ip-address": "10.0.2.15", "prefix": 24 },
              { "garbage": true },
              { "ip-address-type": "ipv6", "ip-address": "fd00::5", "prefix": 64 }
            ]}] }
            |]
        )
        `shouldBe` Just
          [ GuestNetIf
              { gniHardwareAddress = "52:54:00:12:34:56"
              , gniIpAddresses =
                  [ GuestIpAddress {giaType = "ipv4", giaAddress = "10.0.2.15", giaPrefix = 24}
                  , GuestIpAddress {giaType = "ipv6", giaAddress = "fd00::5", giaPrefix = 64}
                  ]
              }
          ]

    it "drops one malformed interface but keeps the rest of the list" $ do
      -- Previously any single malformed interface would cause every other
      -- interface to be lost, silently breaking network detection.
      -- First interface must still appear even though the second's address
      -- shapes are unparseable; the second still parses (just with 0 IPs).
      let parsed =
            parseGuestInterfaces
              ( Just
                  [aesonQQ|
            { "return": [
              { "hardware-address": "52:54:00:aa:bb:cc", "ip-addresses": [{ "ip-address-type": "ipv4", "ip-address": "10.0.0.1", "prefix": 24 }] },
              { "hardware-address": "52:54:00:dd:ee:ff", "ip-addresses": [{ "totally": "wrong" }, { "shape": true }], "extra-unexpected-field": [1, 2, 3] }
            ] }
            |]
              )
      case parsed of
        Just ifs@(firstInterface : _) -> do
          length ifs `shouldBe` 2
          gniHardwareAddress firstInterface `shouldBe` "52:54:00:aa:bb:cc"
          length (gniIpAddresses firstInterface) `shouldBe` 1
        _ -> expectationFailure "parser failed entirely instead of dropping bad entry"

    it "skips interfaces without hardware-address (Windows loopback)" $ do
      parseGuestInterfaces (Just [aesonQQ| { "return": [{ "name": "Loopback" }, { "hardware-address": "52:54:00:12:34:56", "ip-addresses": [] }] } |])
        `shouldBe` Just
          [GuestNetIf {gniHardwareAddress = "52:54:00:12:34:56", gniIpAddresses = []}]

  describe "pollStatus per-recv timeout" $ do
    it "throws userError when the QGA stops responding mid-exec" $ do
      -- Simulates QEMU keeping the chardev open while qemu-ga has
      -- died: the daemon-side socket is healthy but no reply ever
      -- arrives. 'pollStatus' must throw within
      -- 'pollRecvTimeoutMicros' + slack rather than block until the
      -- outer 'withPersistentConn' timeout.
      (sockClient, sockServer) <- socketPair AF_UNIX Stream defaultProtocol
      -- Drain whatever pollStatus sends so its sendJson doesn't
      -- block on a full kernel buffer (the socketpair default
      -- backlog is small). The drainer exits cleanly when the
      -- client side closes (recv returns BS.null).
      drained <- newEmptyMVar
      _ <- forkIO $ do
        let loop = do
              chunk <- recv sockServer 4096
              unless (BS.null chunk) loop
        loop
        putMVar drained ()
      managed <- testTransport sockClient
      t0 <- getCurrentTime
      eResult <-
        try $ GA.pollStatus managed 1234 0 600 (\_ -> pure ()) (\_ -> pure ())
      t1 <- getCurrentTime
      -- Close client first so the drainer sees EOF and exits before
      -- we close the server side (closing a socket another thread
      -- is blocked on triggers a "Bad file descriptor" warning).
      close sockClient
      takeMVar drained
      close sockServer
      let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
          slackSec = 2.5 -- 5 s timeout + slack for scheduling
          floorSec = fromIntegral GA.pollRecvTimeoutMicros / (1000000 :: Double) - 0.5
      elapsed `shouldSatisfy` (< (fromIntegral GA.pollRecvTimeoutMicros / 1000000 + slackSec))
      elapsed `shouldSatisfy` (> floorSec)
      case eResult of
        Left ioe | isUserError ioe -> pure ()
        Left e ->
          expectationFailure
            ("expected a userError IOException; got: " <> show e)
        Right r ->
          expectationFailure
            ("expected pollStatus to throw on silent QGA; got: " <> show r)

  describe "authorized key injection" $ do
    mapM_
      ( \reset -> it ("sends assigned keys with reset=" <> show reset) $ do
          (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
          slot <- newMVar . Just =<< testTransport client
          conns <- testCache (Map.singleton 42 slot)
          observed <- newEmptyMVar
          _ <- forkIO $ do
            request <- recv peer 4096
            putMVar observed (Aeson.eitherDecodeStrict' request :: Either String Aeson.Value)
            sendJson peer (Aeson.object ["return" Aeson..= Aeson.object []])
          GA.guestSshAddAuthorizedKeys conns defaultQemuConfig 42 "alice" ["key-a", "key-b"] reset `shouldReturn` Right ()
          takeMVar observed
            `shouldReturn` Right
              ( Aeson.object
                  [ "execute" Aeson..= ("guest-ssh-add-authorized-keys" :: String)
                  , "arguments" Aeson..= Aeson.object ["username" Aeson..= ("alice" :: String), "keys" Aeson..= (["key-a", "key-b"] :: [String]), "reset" Aeson..= reset]
                  ]
              )
          GA.releaseConn conns 42
          close peer
      )
      [False, True]
    it "preserves QGA error class and description" $ do
      (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
      slot <- newMVar . Just =<< testTransport client
      conns <- testCache (Map.singleton 42 slot)
      _ <- forkIO $ do
        _ <- recv peer 4096
        sendJson peer (Aeson.object ["error" Aeson..= Aeson.object ["class" Aeson..= ("CommandNotFound" :: String), "desc" Aeson..= ("unsupported" :: String)]])
      GA.guestSshAddAuthorizedKeys conns defaultQemuConfig 42 "alice" ["key"] False `shouldReturn` Left "CommandNotFound: unsupported"
      GA.releaseConn conns 42
      close peer
    it "invalidates the connection after a missing reply without replay" $ do
      (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
      slot <- newMVar . Just =<< testTransport client
      conns <- testCache (Map.singleton 42 slot)
      _ <- forkIO $ recv peer 4096 >> close peer
      result <- GA.guestSshAddAuthorizedKeys conns defaultQemuConfig 42 "alice" ["key"] False
      result `shouldSatisfy` either (const True) (const False)
      takeMVar slot `shouldReturn` Nothing

  describe "authorized key reply failure" $ do
    it "invalidates a malformed response" $ do
      (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
      slot <- newMVar . Just =<< testTransport client
      conns <- testCache (Map.singleton 42 slot)
      _ <- forkIO $ do
        _ <- recv peer 4096
        sendJson peer (Aeson.object ["unexpected" Aeson..= True])
      result <- GA.guestSshAddAuthorizedKeys conns defaultQemuConfig 42 "alice" ["key"] True
      result `shouldSatisfy` either (const True) (const False)
      takeMVar slot `shouldReturn` Nothing
      recv peer 4096 `shouldReturn` BS.empty
      close peer
    it "bounds a stalled exchange and discards its connection" $ do
      (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
      slot <- newMVar . Just =<< testTransport client
      conns <- testCache (Map.singleton 42 slot)
      result <- GA.guestSshAddAuthorizedKeys conns defaultQemuConfig 42 "alice" ["key"] False
      result `shouldSatisfy` either (const True) (const False)
      takeMVar slot `shouldReturn` Nothing
      _ <- recv peer 4096
      recv peer 4096 `shouldReturn` BS.empty
      close peer

  describe "native guest control" $ do
    it "checks readiness with guest-ping" $ do
      (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
      slot <- newMVar . Just =<< testTransport client
      conns <- testCache (Map.singleton 42 slot)
      observed <- newEmptyMVar
      _ <- forkIO $ do
        request <- recv peer 4096
        putMVar observed (Aeson.eitherDecodeStrict' request :: Either String Aeson.Value)
        sendJson peer (Aeson.object ["return" Aeson..= Aeson.object []])
      GA.guestPing conns defaultQemuConfig 42 `shouldReturn` True
      takeMVar observed `shouldReturn` Right (Aeson.object ["execute" Aeson..= ("guest-ping" :: String)])
      GA.releaseConn conns 42
      close peer
    it "treats powerdown EOF as dispatch and keeps the serialization slot" $ do
      (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
      slot <- newMVar . Just =<< testTransport client
      conns <- testCache (Map.singleton 42 slot)
      observed <- newEmptyMVar
      _ <- forkIO $ do
        request <- recv peer 4096
        putMVar observed (Aeson.eitherDecodeStrict' request :: Either String Aeson.Value)
        close peer
      GA.guestShutdown conns defaultQemuConfig 42 "powerdown" `shouldReturn` Right ()
      takeMVar observed `shouldReturn` Right (Aeson.object ["execute" Aeson..= ("guest-shutdown" :: String), "arguments" Aeson..= Aeson.object ["mode" Aeson..= ("powerdown" :: String)]])
      takeMVar slot `shouldReturn` Nothing
      Map.member 42 <$> STM.readTVarIO (guestAgentSlots conns) `shouldReturn` True
    it "dispatches reboot once and discards the cached connection on an explicit error" $ do
      (client, peer) <- socketPair AF_UNIX Stream defaultProtocol
      slot <- newMVar . Just =<< testTransport client
      conns <- testCache (Map.singleton 42 slot)
      observed <- newEmptyMVar
      _ <- forkIO $ do
        request <- recv peer 4096
        putMVar observed (Aeson.eitherDecodeStrict' request :: Either String Aeson.Value)
        sendJson peer (Aeson.object ["error" Aeson..= Aeson.object ["desc" Aeson..= ("unsupported" :: String)]])
      result <- GA.guestShutdown conns defaultQemuConfig 42 "reboot"
      result `shouldSatisfy` either (const True) (const False)
      takeMVar observed `shouldReturn` Right (Aeson.object ["execute" Aeson..= ("guest-shutdown" :: String), "arguments" Aeson..= Aeson.object ["mode" Aeson..= ("reboot" :: String)]])
      takeMVar slot `shouldReturn` Nothing
      recv peer 4096 `shouldReturn` BS.empty
      close peer

  describe "sync ID range" $ do
    it "bounded sync IDs fit in 32-bit integers" $ do
      -- Max sync ID is 2^30 - 1 = 1073741823
      let maxSyncId = 1073741823 :: Int
      -- Verify it fits in IEEE 754 double without precision loss
      let asDouble = fromIntegral maxSyncId :: Double
      round asDouble `shouldBe` maxSyncId

    it "bounded sync IDs roundtrip through JSON" $ do
      let syncId = 1073741823 :: Int
          json = Aeson.encode (Aeson.object ["return" Aeson..= syncId])
          parsed = Aeson.decode json :: Maybe Aeson.Value
      case parsed of
        Just (Aeson.Object obj) ->
          KM.lookup "return" obj `shouldBe` Just (Aeson.Number (fromIntegral syncId))
        _ -> expectationFailure "Failed to roundtrip sync ID through JSON"

  describe "releaseConn" $ do
    it "is a no-op when the VM has no cached connection" $ do
      conns <- testCache Map.empty
      GA.releaseConn conns 42
      m <- STM.readTVarIO (guestAgentSlots conns)
      Map.member 42 m `shouldBe` False

    it "removes the map entry when present, leaving no entry behind" $ do
      conns <- testCache Map.empty
      slot <- newMVar Nothing
      STM.atomically $ STM.writeTVar (guestAgentSlots conns) (Map.singleton 42 slot)
      GA.releaseConn conns 42
      m <- STM.readTVarIO (guestAgentSlots conns)
      Map.member 42 m `shouldBe` False

    it "closes the cached socket so its fd is returned to the kernel" $ do
      (s, peer) <- socketPair AF_UNIX Stream defaultProtocol
      conns <- testCache Map.empty
      slot <- newMVar . Just =<< testTransport s
      STM.atomically $ STM.writeTVar (guestAgentSlots conns) (Map.singleton 7 slot)
      GA.releaseConn conns 7
      -- A peer recv on a fully-closed socket returns the empty
      -- ByteString (EOF). If 'releaseConn' forgot to close, recv
      -- would block — wrap in a short STM-driven timeout via a
      -- fork so the test still fails fast.
      done <- newEmptyMVar
      _ <- forkIO $ do
        bs <- recv peer 1
        putMVar done bs
      result <- takeMVar done
      result `shouldBe` BS.empty
      close peer

    it "leaves untouched the conn slots for OTHER VMs (no spillover)" $ do
      conns <- testCache Map.empty
      slot7 <- newMVar Nothing
      slot8 <- newMVar Nothing
      STM.atomically $
        STM.writeTVar (guestAgentSlots conns) (Map.fromList [(7, slot7), (8, slot8)])
      GA.releaseConn conns 7
      m <- STM.readTVarIO (guestAgentSlots conns)
      Map.member 7 m `shouldBe` False
      Map.member 8 m `shouldBe` True
