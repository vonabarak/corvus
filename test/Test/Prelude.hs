-- | Test prelude - convenient re-exports for writing tests.
module Test.Prelude
  ( -- * HSpec
    module Test.Hspec,

    -- * Database setup
    withTestDb,

    -- * DSL Core
    TestM,
    testCase,
    given,
    when_,
    then_,

    -- * DSL Given (setup)
    module Test.DSL.Given,

    -- * DSL When (commands)
    module Test.DSL.When,

    -- * DSL Then (assertions)
    module Test.DSL.Then,

    -- * Model types
    VmStatus (..),
    DriveFormat (..),
    DriveInterface (..),
    DriveMedia (..),
    CacheType (..),
    NetInterfaceType (..),
    SharedDirCache (..),

    -- * Protocol types
    Response (..),

    -- * Common types
    Int64,
    Text,
  )
where

import Corvus.Model
  ( CacheType (..),
    DriveFormat (..),
    DriveInterface (..),
    DriveMedia (..),
    NetInterfaceType (..),
    SharedDirCache (..),
    VmStatus (..),
  )
import Corvus.Protocol
  ( Response (..),
  )
import Data.Int (Int64)
import Data.Text (Text)
import Test.DSL.Core (TestM, given, testCase, then_, when_)
import Test.DSL.Given
import Test.DSL.Then
import Test.DSL.When
import Test.Database (withTestDb)
import Test.Hspec
