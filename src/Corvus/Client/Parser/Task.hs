{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv task@ subcommands.
module Corvus.Client.Parser.Task
  ( taskCommandParser
  )
where

import Corvus.Client.Types
import Options.Applicative

-- | Parser for task list
taskListCommand :: Parser Command
taskListCommand =
  TaskList
    <$> option
      auto
      ( long "last"
          <> short 'n'
          <> metavar "N"
          <> value 10
          <> help "Number of tasks to show (default: 10)"
      )
    <*> optional
      ( strOption
          ( long "subsystem"
              <> metavar "SUB"
              <> help "Filter by subsystem: vm, disk, network, ssh-key, template, snapshot, apply"
              <> completeWith ["vm", "disk", "network", "ssh-key", "template", "shared-dir", "snapshot", "system", "apply"]
          )
      )
    <*> optional
      ( strOption
          ( long "result"
              <> metavar "RESULT"
              <> help "Filter by result: success, error, running, not_started, cancelled"
              <> completeWith ["success", "error", "running", "not_started", "cancelled"]
          )
      )
    <*> switch
      ( long "all"
          <> short 'a'
          <> help "Include subtasks in the list"
      )

-- | Parser for task show
taskShowCommand :: Parser Command
taskShowCommand =
  TaskShow
    <$> argument auto (metavar "ID" <> help "Task ID to show")

-- | Parser for task wait
taskWaitCommand :: Parser Command
taskWaitCommand =
  TaskWait
    <$> argument auto (metavar "ID" <> help "Task ID to wait for")
    <*> optional
      ( option
          auto
          ( long "timeout"
              <> short 't'
              <> metavar "SECONDS"
              <> help "Timeout in seconds (default: no timeout)"
          )
      )

-- | Parser for all task subcommands
taskCommandParser :: Parser Command
taskCommandParser =
  subparser
    ( command
        "list"
        (info taskListCommand (progDesc "List task history"))
        <> command
          "show"
          (info taskShowCommand (progDesc "Show task details"))
        <> command
          "wait"
          (info taskWaitCommand (progDesc "Wait for a task to complete"))
    )
