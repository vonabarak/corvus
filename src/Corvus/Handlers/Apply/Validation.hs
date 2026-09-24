{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Apply.Validation
  ( handleApplyValidate
  , effectiveCloudInit
  , checksumSpecToImport
  ) where

import Control.Monad (forM_)
import Control.Monad.Logger (logWarnN)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Node.Image (isHttpUrl)
import Corvus.Protocol (Response (..))
import Corvus.Schema.Apply
import Corvus.Schema.Template (TemplateYaml (..))
import Corvus.Types (ServerState, runServerLogging)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (decodeEither')

handleApplyValidate :: ServerState -> Text -> IO (Either Response ApplyConfig)
handleApplyValidate state yamlContent = runServerLogging state $
  case decodeEither' (TE.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack $ show err
      logWarnN $ "Failed to parse apply config YAML: " <> msg
      pure $ Left $ RespError msg
    Right config -> case validateConfig config of
      Left err -> do
        logWarnN $ "Config validation failed: " <> err
        pure $ Left $ RespError err
      Right () -> pure $ Right config

validateConfig :: ApplyConfig -> Either Text ()
validateConfig config = do
  checkDuplicates "SSH key" $ map askName (acSshKeys config)
  checkDuplicates "disk" $ map adName (acDisks config)
  checkDuplicatesPerNode "VM" [(avName v, avNode v) | v <- acVms config]
  checkDuplicatesPerNode "network" [(anName n, anNode n) | n <- acNetworks config]
  checkDuplicates "template" $ map tyName (acTemplates config)
  forM_ (acSshKeys config) $ validateName "SSH key" . askName
  forM_ (acDisks config) $ validateName "Disk" . adName
  forM_ (acNetworks config) $ validateName "Network" . anName
  forM_ (acVms config) $ validateName "VM" . avName
  forM_ (acTemplates config) $ validateName "Template" . tyName
  forM_ (acDisks config) validateDisk
  forM_ (acVms config) validateVmCloudInit
  where
    checkDuplicates kind names = case findDuplicate names of
      Nothing -> Right ()
      Just d -> Left $ "Duplicate " <> kind <> " name: " <> d
    checkDuplicatesPerNode kind pairs = case findDuplicate pairs of
      Nothing -> Right ()
      Just (nm, nd) -> Left $ "Duplicate " <> kind <> " name '" <> nm <> "' on node '" <> if T.null nd then "(scheduler)'" else nd <> "'"
    findDuplicate [] = Nothing
    findDuplicate (x : xs) | x `elem` xs = Just x | otherwise = findDuplicate xs
    validateVmCloudInit v
      | not (null (avSshKeys v)) && not (effectiveCloudInit v) = Left $ "VM '" <> avName v <> "': has SSH keys but cloud-init is not enabled"
      | otherwise = Right ()
    validateDisk d =
      let hasImport = isJust (adImport d)
          hasOverlay = isJust (adOverlay d)
          hasClone = isJust (adClone d)
          hasRegister = isJust (adRegister d)
          hasBacking = isJust (adBacking d)
          hasCreate = isJust (adFormat d) && isJust (adSizeMb d) && not hasImport && not hasOverlay && not hasClone && not hasRegister
          strategies = length $ filter id [hasImport, hasOverlay, hasClone, hasRegister]
       in if strategies > 1
            then Left $ "Disk '" <> adName d <> "': cannot specify more than one of 'import', 'overlay', 'clone', 'register'"
            else
              if not hasImport && not hasOverlay && not hasClone && not hasRegister && not hasCreate
                then Left $ "Disk '" <> adName d <> "': must specify 'import', 'overlay', 'clone', 'register', or both 'format' and 'sizeMb'"
                else
                  if isJust (adPath d) && not hasOverlay && not hasClone && not hasCreate && not hasImport
                    then Left $ "Disk '" <> adName d <> "': 'path' can only be used with 'import', 'overlay', 'clone', or 'create'"
                    else
                      if hasBacking && not hasRegister
                        then Left $ "Disk '" <> adName d <> "': 'backing' can only be used with 'register'"
                        else case adChecksum d of
                          Nothing -> Right ()
                          Just cs
                            | not hasImport -> Left $ "Disk '" <> adName d <> "': 'checksum' can only be used with 'import'"
                            | maybe False (not . isHttpUrl) (adImport d) -> Left $ "Disk '" <> adName d <> "': 'checksum' can only be used with HTTP/HTTPS imports"
                            | not (isValidChecksum cs) -> Left $ "Disk '" <> adName d <> "': checksum value for " <> checksumAlgorithmText (csAlgorithm cs) <> " must be " <> T.pack (show (checksumHexLength (csAlgorithm cs))) <> " hex characters"
                            | otherwise -> Right ()
    isValidChecksum cs = T.length (csValue cs) == checksumHexLength (csAlgorithm cs) && T.all isHexDigit (csValue cs)
    isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

effectiveCloudInit :: ApplyVm -> Bool
effectiveCloudInit v = maybe (not $ null $ avSshKeys v) id (avCloudInit v)

checksumAlgorithmText ChecksumMd5 = "md5"
checksumAlgorithmText ChecksumSha1 = "sha1"
checksumAlgorithmText ChecksumSha256 = "sha256"
checksumAlgorithmText ChecksumSha512 = "sha512"
checksumAlgorithmText ChecksumBlake2b = "blake2b"
checksumTargetText ChecksumDownload = "download"
checksumTargetText ChecksumFinal = "final"
checksumHexLength ChecksumMd5 = 32
checksumHexLength ChecksumSha1 = 40
checksumHexLength ChecksumSha256 = 64
checksumHexLength ChecksumSha512 = 128
checksumHexLength ChecksumBlake2b = 128
checksumSpecToImport cs = (checksumAlgorithmText (csAlgorithm cs), csValue cs, checksumTargetText (csTarget cs))
