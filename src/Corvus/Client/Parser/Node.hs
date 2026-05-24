{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv node@ subcommands.
module Corvus.Client.Parser.Node
  ( nodeCommandParser
  )
where

import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

-- | Parser for @crv node add@.
nodeAddCommand :: Parser Command
nodeAddCommand =
  NodeAdd
    <$> argument
      (T.pack <$> str)
      (metavar "NAME" <> help "Name for the new node (cluster-wide unique)")
    <*> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> help "IP address or hostname where the node's agents listen"
      )
    <*> option
      auto
      ( long "node-agent-port"
          <> metavar "PORT"
          <> value 9878
          <> help "TCP port for corvus-nodeagent (default: 9878)"
      )
    <*> option
      auto
      ( long "net-agent-port"
          <> metavar "PORT"
          <> value 9877
          <> help "TCP port for corvus-netd (default: 9877)"
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "base-path"
                <> metavar "PATH"
                <> help
                  ( "Daemon basePath on the node (where VM images live). "
                      <> "Defaults to $HOME/VMs resolved on the admin host — "
                      <> "the common case where admin and node run as the "
                      <> "same user. Pass an explicit absolute path when the "
                      <> "node's daemon-user differs."
                  )
            )
      )
    <*> optional
      ( strOption
          ( long "description"
              <> short 'd'
              <> metavar "TEXT"
              <> help "Free-form description"
          )
      )
    <*> strOption
      ( long "admin-state"
          <> metavar "STATE"
          <> value "online"
          <> help "Initial admin state: online | draining | maintenance (default: online)"
          <> completeWith ["online", "draining", "maintenance"]
      )

-- | Parser for @crv node list@.
nodeListCommand :: Parser Command
nodeListCommand = pure NodeList

-- | Parser for @crv node show@.
nodeShowCommand :: Parser Command
nodeShowCommand =
  NodeShow
    <$> argument
      (T.pack <$> str)
      (metavar "NODE" <> help "Name or ID of the node")

-- | Parser for @crv node edit@.
nodeEditCommand :: Parser Command
nodeEditCommand =
  NodeEdit
    <$> argument
      (T.pack <$> str)
      (metavar "NODE" <> help "Name or ID of the node")
    <*> optional (T.pack <$> strOption (long "name" <> metavar "NAME" <> help "New name"))
    <*> optional (T.pack <$> strOption (long "host" <> metavar "HOST" <> help "New host (ip / hostname)"))
    <*> optional (option auto (long "node-agent-port" <> metavar "PORT" <> help "New nodeagent TCP port"))
    <*> optional (option auto (long "net-agent-port" <> metavar "PORT" <> help "New netd TCP port"))
    <*> optional (T.pack <$> strOption (long "base-path" <> metavar "PATH" <> help "New basePath"))
    -- Description: Maybe (Maybe Text). 'Nothing' = leave
    -- unchanged; 'Just Nothing' = clear (pass empty value);
    -- 'Just (Just t)' = set to t.
    <*> optional
      ( fmap
          (\s -> if null s then Nothing else Just (T.pack s))
          ( strOption
              ( long "description"
                  <> metavar "TEXT"
                  <> help "New description (pass empty string to clear)"
              )
          )
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "admin-state"
                <> metavar "STATE"
                <> help "online | draining | maintenance"
                <> completeWith ["online", "draining", "maintenance"]
            )
      )

-- | Parser for @crv node drain@.
nodeDrainCommand :: Parser Command
nodeDrainCommand =
  NodeDrain
    <$> argument
      (T.pack <$> str)
      (metavar "NODE" <> help "Name or ID of the node to drain")

-- | Parser for @crv node delete@.
nodeDeleteCommand :: Parser Command
nodeDeleteCommand =
  NodeDelete
    <$> argument
      (T.pack <$> str)
      (metavar "NODE" <> help "Name or ID of the node to delete")

-- | Parser for all @crv node@ subcommands.
nodeCommandParser :: Parser Command
nodeCommandParser =
  subparser
    ( command
        "add"
        (info nodeAddCommand (progDesc "Register a new node"))
        <> command
          "list"
          (info nodeListCommand (progDesc "List all registered nodes"))
        <> command
          "show"
          (info nodeShowCommand (progDesc "Show node details"))
        <> command
          "edit"
          (info nodeEditCommand (progDesc "Edit node properties"))
        <> command
          "drain"
          (info nodeDrainCommand (progDesc "Stop the scheduler from picking this node"))
        <> command
          "delete"
          (info nodeDeleteCommand (progDesc "Remove a node (refuses while in use)"))
    )
