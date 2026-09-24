{-# LANGUAGE OverloadedStrings #-}

module Corvus.Node.GuestAgent.Network
  ( guestNetworkGetInterfaces
  , parseGuestInterfaces
  )
where

import Corvus.Node.GuestAgent.Connection (GuestAgentConns, withPersistentConn)
import Corvus.Node.GuestAgent.Transport (recvJson, sendJson)
import Corvus.Node.GuestAgent.Types
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

-- | Query network interfaces from the guest. Failures are represented as
-- 'Nothing'; a successful query with no interfaces returns @Just []@.
guestNetworkGetInterfaces :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Maybe [GuestNetIf])
guestNetworkGetInterfaces conns config vmId = do
  result <- withPersistentConn conns config vmId 5 15000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-network-get-interfaces" :: Text)]
    parseGuestInterfaces <$> recvJson sock
  pure $ either (const Nothing) id result

-- | Leniently parse QGA interfaces: malformed addresses and interfaces are
-- dropped independently, while address-less (e.g. Windows loopback)
-- interfaces are intentionally omitted.
parseGuestInterfaces :: Maybe Value -> Maybe [GuestNetIf]
parseGuestInterfaces mValue = do
  value <- mValue
  AT.parseMaybe parser value
  where
    parser = AT.withObject "response" $ \obj -> do
      returned <- obj .: "return" :: AT.Parser [Value]
      pure (mapMaybe (AT.parseMaybe parseInterface) returned)
    parseInterface = AT.withObject "interface" $ \obj -> do
      mHardwareAddress <- obj .:? "hardware-address"
      case mHardwareAddress of
        Nothing -> fail "no hardware-address"
        Just hardwareAddress -> do
          rawAddresses <- obj .:? "ip-addresses" AT..!= ([] :: [Value])
          pure
            GuestNetIf
              { gniHardwareAddress = hardwareAddress
              , gniIpAddresses = mapMaybe (AT.parseMaybe parseAddress) rawAddresses
              }
    parseAddress = AT.withObject "ip-address" $ \obj ->
      GuestIpAddress <$> obj .: "ip-address-type" <*> obj .: "ip-address" <*> (obj .:? "prefix" AT..!= 0)
