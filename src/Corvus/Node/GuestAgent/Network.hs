{-# LANGUAGE OverloadedStrings #-}

module Corvus.Node.GuestAgent.Network
  ( parseGuestInterfaces
  )
where

import Corvus.Node.GuestAgent.Types
import Data.Aeson (Value, (.:), (.:?))
import qualified Data.Aeson.Types as AT
import Data.Maybe (mapMaybe)

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
