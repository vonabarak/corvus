{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.SchemaBoundarySpec (spec) where

import Corvus.Model (NetInterfaceType (..))
import Corvus.Schema.Apply (ApplyNetIf (..))
import Corvus.Schema.Build
import Corvus.Schema.CloudInit
import Corvus.Schema.Template
import Data.Aeson (Value)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (FromJSON, ParseException, decodeEither')
import Test.Hspec

spec :: Spec
spec = do
  describe "schema rejection diagnostics" $ do
    rejects "empty pipeline step" ("{}" :: Text) "needs" (Proxy :: Proxy PipelineStep)
    rejects "ambiguous pipeline step" "{apply: {}, upload: {name: image, from: /a, format: raw}}" "exactly one" (Proxy :: Proxy PipelineStep)
    rejects "upload collision policy" "{name: image, from: /a, format: raw, ifExists: bogus}" "ifExists" (Proxy :: Proxy Upload)
    rejects "empty provisioner" "{}" "must have one" (Proxy :: Proxy Provisioner)
    rejects "ambiguous provisioner" "{shell: echo hi, reboot: {}}" "exactly one" (Proxy :: Proxy Provisioner)
    rejects "scalar provisioner" "true" "Provisioner" (Proxy :: Proxy Provisioner)
    rejects "scalar shell" "false" "Shell" (Proxy :: Proxy Shell)
    rejects "shell env array" "{inline: echo hi, env: []}" "shell.env" (Proxy :: Proxy Shell)
    rejects "shell env nonstring" "{inline: echo hi, env: {COUNT: 1}}" "shell.env.COUNT" (Proxy :: Proxy Shell)
    rejects "defaults env array" "{env: []}" "shellDefaults.env" (Proxy :: Proxy ShellDefaults)
    rejects "defaults env nonstring" "{env: {ENABLED: true}}" "shellDefaults.env.ENABLED" (Proxy :: Proxy ShellDefaults)
    rejects "empty wait" "{}" "specify one" (Proxy :: Proxy WaitFor)
    rejects "disabled ping wait" "{ping: false}" "exactly one" (Proxy :: Proxy WaitFor)
    rejects "ambiguous wait" "{ping: true, port: 22}" "exactly one" (Proxy :: Proxy WaitFor)
    rejects "unknown build strategy" "bogus" "unknown strategy" (Proxy :: Proxy BuildStrategy)
    rejects "unknown cleanup" "bogus" "unknown cleanup" (Proxy :: Proxy CleanupMode)
    rejects "template NIC without type or network" "{}" "must specify" (Proxy :: Proxy TemplateNetworkInterfaceYaml)
    rejects "template NIC with conflicting type" "{type: user, network: net}" "must have type" (Proxy :: Proxy TemplateNetworkInterfaceYaml)
    rejects "apply NIC with conflicting type" "{type: bridge, network: net}" "must have type" (Proxy :: Proxy ApplyNetIf)
  describe "schema positive boundaries" $ do
    it "infers managed template NICs and preserves explicit host devices" $ do
      parsed <- parse "{network: net, hostDevice: br0}"
      (tnyType parsed, tnyHostDevice parsed, tnyNetwork parsed) `shouldBe` (NetManaged, Just "br0", Just "net")
    it "defaults an unspecified apply NIC to user networking" $ do
      parsed <- parse "{}"
      aniType parsed `shouldBe` NetUser
    it "accepts an explicitly managed template NIC" $ do
      parsed <- parse "{type: managed, network: net}"
      tnyType parsed `shouldBe` NetManaged
    it "preserves raw cloud-init text and explicit empty network data" $ do
      parsed <- parse "{userData: '#cloud-config', networkConfig: '', injectSshKeys: false}"
      (cicyUserData parsed, cicyNetworkConfig parsed, cicyInjectSshKeys parsed) `shouldBe` (Just "#cloud-config", Just "", False)
    it "encodes structured cloud-init documents without losing their values" $ do
      parsed <- parse "{userData: {packages: [curl]}, networkConfig: {version: 2}}"
      let decodeValue = fmap (either (Left . show) Right . decodeEither' . TE.encodeUtf8) :: Maybe Text -> Maybe (Either String Value)
      decodeValue (cicyUserData parsed) `shouldBe` Just (either (Left . show) Right (decodeEither' "{packages: [curl]}"))
      decodeValue (cicyNetworkConfig parsed) `shouldBe` Just (either (Left . show) Right (decodeEither' "{version: 2}"))
      cicyInjectSshKeys parsed `shouldBe` True
    it "treats null cloud-init data as absent" $ do
      parsed <- parse "{userData: null, networkConfig: null}"
      (cicyUserData parsed, cicyNetworkConfig parsed) `shouldBe` (Nothing, Nothing)

rejects :: forall a. (FromJSON a) => String -> Text -> Text -> Proxy a -> Spec
rejects label yaml fragment _ = it label $ case decodeEither' (TE.encodeUtf8 yaml) :: Either ParseException a of
  Left err -> T.pack (show err) `shouldSatisfy` T.isInfixOf fragment
  Right _ -> expectationFailure "unexpectedly accepted invalid schema"

parse :: (FromJSON a) => Text -> IO a
parse yaml = case decodeEither' (TE.encodeUtf8 yaml) of
  Left err -> expectationFailure (show err) >> fail "schema fixture failed"
  Right value -> pure value
