{-# LANGUAGE OverloadedStrings #-}

module Corvus.Client.Parser.AudioDevice (audioDeviceCommandParser) where

import Corvus.Client.Completion (vmCompleter)
import Corvus.Client.Types
import Data.Int (Int64)
import qualified Data.Text as T
import Options.Applicative

vmArg :: Parser T.Text
vmArg = argument (T.pack <$> str) (metavar "VM" <> completer vmCompleter)

backendArg :: Parser T.Text
backendArg = argument (T.pack <$> str) (metavar "BACKEND" <> completeWith ["pulse", "pipewire", "spice"])

optionsArg :: Parser T.Text
optionsArg = T.pack <$> strOption (long "options" <> metavar "KEY=VALUE,..." <> value "" <> help "QEMU audio backend properties")

deviceIdArg :: Parser Int64
deviceIdArg = argument auto (metavar "AUDIO_DEVICE_ID")

modelOption :: Parser T.Text
modelOption = T.pack <$> strOption (long "model" <> metavar "MODEL" <> value "virtio-sound" <> showDefault <> completeWith ["virtio-sound", "intel-hda", "ich9-intel-hda", "AC97"] <> help "Audio device model")

optionalModelOption :: Parser (Maybe T.Text)
optionalModelOption = optional (T.pack <$> strOption (long "model" <> metavar "MODEL" <> completeWith ["virtio-sound", "intel-hda", "ich9-intel-hda", "AC97"] <> help "Audio device model"))

audioDeviceCommandParser :: Parser Command
audioDeviceCommandParser =
  subparser
    ( command "add" (info (AudioDeviceAdd <$> vmArg <*> backendArg <*> optionsArg <*> modelOption) (progDesc "Add a duplex audio card"))
        <> command "edit" (info (AudioDeviceEdit <$> vmArg <*> deviceIdArg <*> backendArg <*> optionsArg <*> optionalModelOption) (progDesc "Edit an audio card"))
        <> command "remove" (info (AudioDeviceRemove <$> vmArg <*> deviceIdArg) (progDesc "Remove an audio card"))
        <> command "list" (info (AudioDeviceList <$> vmArg) (progDesc "List audio cards"))
    )
