{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.AtomicUploadSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as S
import Control.Concurrent.STM (readTVarIO)
import qualified Control.Exception as E
import Corvus.Node.Transfer (FileWriterSink (..), newAtomicFileWriterSink)
import Corvus.Rpc.Streams (callSink)
import Data.List (isInfixOf)
import Supervisors (withSupervisor)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "atomic upload publication" $ do
  it "reports a collision, preserves the destination, and removes temporary files" $
    withSystemTempDirectory "upload" $ \dir -> withSupervisor $ \sup -> do
      let destination = dir </> "image.raw"
      writeFile destination "original"
      writer <- newAtomicFileWriterSink destination
      sink <- C.export @S.ByteSink sup writer
      callSink #write (S.ByteSink'write'params "replacement") sink
      callSink #end S.ByteSink'end'params sink
        `shouldThrow` (\err -> "Upload destination already exists" `isInfixOf` E.displayException (err :: E.SomeException))
      readFile destination `shouldReturn` "original"
      listDirectory dir `shouldReturn` ["image.raw"]
      -- Failure of one stream does not kill the capability's dispatcher.
      callSink #abort S.ByteSink'abort'params sink
  it "publishes all bytes on a successful end" $
    withSystemTempDirectory "upload" $ \dir -> withSupervisor $ \sup -> do
      let destination = dir </> "image.raw"
      writer <- newAtomicFileWriterSink destination
      sink <- C.export @S.ByteSink sup writer
      callSink #write (S.ByteSink'write'params "payload") sink
      callSink #end S.ByteSink'end'params sink
      readTVarIO (fwsBytesWritten writer) `shouldReturn` 7
      readFile destination `shouldReturn` "payload"
      listDirectory dir `shouldReturn` ["image.raw"]
