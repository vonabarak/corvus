-- | Test prelude - convenient re-exports for writing tests.
module Test.Prelude
  ( -- * HSpec
    module Test.Hspec

    -- * Database setup
  , withTestDb

    -- * DSL Core
  , TestM
  , testCase
  , given
  , when_
  , then_
  , liftIO

    -- * DSL Given (setup)
  , module Test.DSL.Given

    -- * DSL When (commands)
  , module Test.DSL.When

    -- * DSL Then (assertions)
  , module Test.DSL.Then

    -- * Model types
  , VmStatus (..)
  , DriveFormat (..)
  , DriveInterface (..)
  , DriveMedia (..)
  , CacheType (..)
  , NetInterfaceType (..)
  , SharedDirCache (..)
  , TaskSubsystem (..)
  , TaskResult (..)

    -- * Protocol types
  , Response (..)
  , VmEditResult (..)

    -- * Database types
  , Entity (..)
  , Task (..)

    -- * Common types
  , Int64
  , Text
  )
where

import Control.Monad.IO.Class (liftIO)
import Corvus.Client.Rpc (VmEditResult (..))
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..), DriveMedia (..), NetInterfaceType (..), SharedDirCache (..), Task (..), TaskResult (..), TaskSubsystem (..), VmStatus (..))
import Corvus.Protocol
  ( Response (..)
  )
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist (Entity (..))
import Test.DSL.Core (TestM, given, testCase, then_, when_)
import Test.DSL.Given
import Test.DSL.Then
import Test.DSL.When
import Test.Database (withTestDb)
import Test.Hspec
