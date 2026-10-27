{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.ClientViewerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Monad (forM_, void, when)
import Corvus.Client.Config (ClientConfig (..))
import Corvus.Client.Types (ViewOptions (..), Viewer (..))
import Corvus.Client.Viewer (ViewerLaunch (..), prepareViewer, runViewer)
import Corvus.Wire.Common (ViewGrant (..))
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO (stderr)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = sequential $ describe "CLI native viewers" $ do
  describe "discovery" $ do
    forM_ [(False, False), (False, True), (True, False), (True, True)] $ \(remmina, virtViewer) ->
      forM_ [Nothing, Just Remmina, Just VirtViewer] $ \selection ->
        it (show (remmina, virtViewer, selection)) $ fixture $ \dir config -> do
          when remmina $ script (ccRemmina config) "exit 0\n"
          when virtViewer $ script (ccRemoteViewer config) "exit 0\n"
          let expected = case selection of
                Nothing | virtViewer -> Just VirtViewer
                Nothing | remmina -> Just Remmina
                Just Remmina | remmina -> Just Remmina
                Just VirtViewer | virtViewer -> Just VirtViewer
                _ -> Nothing
          result <- withCurrentDirectory dir $ prepareViewer config (ViewOptions selection (Just ".") ["--kiosk"])
          case expected of
            Nothing -> result `shouldSatisfy` isLeft
            Just viewer -> result `shouldBe` Right (ViewerLaunch viewer (case viewer of Remmina -> ccRemmina config; VirtViewer -> ccRemoteViewer config) (Just dir) ["--kiosk"])
    it "rejects files and missing shared folders" $ fixture $ \dir config -> do
      script (ccRemmina config) "exit 0\n"
      forM_ [dir </> "absent", ccRemmina config] $ \folder ->
        prepareViewer config (ViewOptions Nothing (Just folder) []) `shouldReturn` Left "--share-folder must name an existing client-local directory."
    it "ignores a non-executable virt-viewer and selects Remmina" $ fixture $ \_ config -> do
      writeFile (ccRemoteViewer config) "not executable"
      script (ccRemmina config) "exit 0\n"
      prepareViewer config (ViewOptions Nothing Nothing []) `shouldReturn` Right (ViewerLaunch Remmina (ccRemmina config) Nothing [])
  forM_ [Remmina, VirtViewer] $ \viewer ->
    forM_ [Nothing, Just "folder with spaces"] $ \folder ->
      it ("forwards literal extra arguments to the viewer only: " ++ show (viewer, folder)) $ fixture $ \dir config -> do
        let extraArgs = ["--kiosk", "--title", "VM with spaces", "single' and double\" quotes", "$(touch should-not-exist); * $HOME", ""]
            executable = case viewer of Remmina -> ccRemmina config; VirtViewer -> ccRemoteViewer config
            recordArgs = "printf '%s\\000' \"$@\" > '" ++ dir </> "args" ++ "'\n"
            viewerBody = recordArgs ++ "shift " ++ show (length extraArgs + if isNothing folder then 0 else 1) ++ "\n" ++ recordProfile dir
        script executable $ case viewer of
          VirtViewer -> viewerBody
          Remmina -> remminaScript dir (length extraArgs) "printf 'Encrypted password: MDEyMzQ1Njc=\\n'\nexit 0" (recordProfile dir)
        (_, ok) <- hCapture [stderr] $ runViewer (ViewerLaunch viewer executable folder extraArgs) "VM" grant
        ok `shouldBe` True
        actual <- readFile (dir </> "args")
        path <- T.unpack . T.strip <$> TIO.readFile (dir </> "path")
        let expected = case viewer of
              VirtViewer -> maybe [] (\f -> ["--spice-shared-dir=" ++ f]) folder ++ extraArgs ++ [path]
              Remmina -> extraArgs ++ ["--connect", path]
        actual `shouldBe` concatMap (++ "\0") expected
        assertPrivateCleanup dir
  describe "virt-viewer" $ do
    forM_ [False, True] $ \failViewer ->
      forM_ [Nothing, Just "folder with spaces"] $ \folder ->
        it ("passes a private connection file and optional literal folder: " ++ show (failViewer, folder)) $ fixture $ \dir config -> do
          script (ccRemoteViewer config) $
            unlines
              [ "printf '%s\\n' \"$#\" > '" ++ dir </> "argc" ++ "'"
              , "if [ \"$#\" = 2 ]; then printf '%s\\n' \"$1\" > '" ++ dir </> "folder" ++ "'; shift; fi"
              , recordProfile dir
              , if failViewer then "exit 2" else "exit 0"
              ]
          (err, ok) <- hCapture [stderr] $ runViewer (ViewerLaunch VirtViewer (ccRemoteViewer config) folder []) "VM" grant
          ok `shouldBe` not failViewer
          when failViewer $ err `shouldSatisfy` isInfixOf "Failed"
          readFile (dir </> "argc") `shouldReturn` (if isNothing folder then "1\n" else "2\n")
          case folder of
            Nothing -> if failViewer then pure () else err `shouldBe` ""
            Just path -> do
              readFile (dir </> "folder") `shouldReturn` "--spice-shared-dir=" ++ path ++ "\n"
              err `shouldSatisfy` isInfixOf "enable Share folder"
          body <- readFile (dir </> "profile")
          body `shouldBe` "[virt-viewer]\ntype=spice\nhost=host\nport=5900\npassword=secret\n"
          assertPrivateCleanup dir
  describe "Remmina" $ do
    forM_ [0 :: Int, 1] $ \helperExit ->
      forM_ [False, True] $ \failViewer ->
        it ("encrypts over stdin and cleans up on viewer exit: " ++ show (helperExit, failViewer)) $ fixture $ \dir config -> do
          script (ccRemmina config) $ remminaScript dir 0 ("printf 'Encrypted password: MDEyMzQ1Njc=\\n'\nexit " ++ show helperExit) (recordProfile dir ++ if failViewer then "\nexit 2\n" else "\nexit 0\n")
          let host = if helperExit == 0 then "2001:db8::1" else "[2001:db8::1]"
          (err, ok) <- hCapture [stderr] $ runViewer (ViewerLaunch Remmina (ccRemmina config) (Just " /folder\\name\t\r\n") []) " VM\nname" (grant {vgHost = host})
          ok `shouldBe` not failViewer
          if failViewer then err `shouldSatisfy` isInfixOf "Failed" else err `shouldBe` ""
          err `shouldSatisfy` (not . isInfixOf "secret")
          readFile (dir </> "password") `shouldReturn` "secret\n"
          body <- readFile (dir </> "profile")
          lines body
            `shouldBe` ["[remmina]", "protocol=SPICE", "name=\\sVM\\nname", "server=[2001:db8::1]:5900", "password=MDEyMzQ1Njc=", "enableaudio=1", "disableclipboard=0", "scale=2", "disablepasswordstoring=1", "sharefolder=\\s/folder\\\\name\\t\\r\\n"]
          helperId <- readFile (dir </> "helper-id")
          viewerId <- readFile (dir </> "viewer-id")
          helperId `shouldSatisfy` isInfixOf "org.corvus.Viewer.c"
          helperId `shouldNotBe` viewerId
          readFile (dir </> "locale") `shouldReturn` "C\n"
          assertPrivateCleanup dir
    it "omits folder configuration, brackets only IPv6, and uses unique invocation IDs" $ fixture $ \dir config -> do
      script (ccRemmina config) $ remminaScript dir 0 "printf 'Encrypted password: MDEyMzQ1Njc=\\n'\nexit 0" (recordProfile dir)
      let launch = runViewer (ViewerLaunch Remmina (ccRemmina config) Nothing []) "VM" grant
      launch `shouldReturn` True
      firstId <- readFile (dir </> "viewer-id")
      length firstId `shouldSatisfy` (> 0)
      launch `shouldReturn` True
      secondId <- readFile (dir </> "viewer-id")
      secondId `shouldNotBe` firstId
      body <- readFile (dir </> "profile")
      body `shouldSatisfy` isInfixOf "server=host:5900\n"
      body `shouldSatisfy` (not . isInfixOf "sharefolder")
      assertPrivateCleanup dir
    forM_
      [ "printf 'secret\\nEncrypted password: MDEyMzQ1Njc=\\n'; exit 2"
      , "printf 'Encrypted password: invalid!\\n'; exit 0"
      , "printf 'Encrypted password: \\n'; exit 0"
      , "printf 'Encrypted password: YQ==\\n'; exit 0"
      , "printf 'Encrypted password: MDEyMzQ1Njc=\\nEncrypted password: MDEyMzQ1Njc=\\n'; exit 0"
      , "printf 'unexpected secret\\n' >&2; exit 1"
      ]
      $ \helper ->
        it ("rejects invalid encryption without leaking output: " ++ helper) $ fixture $ \dir config -> do
          script (ccRemmina config) $ remminaScript dir 0 helper (recordProfile dir)
          (err, ok) <- hCapture [stderr] $ runViewer (ViewerLaunch Remmina (ccRemmina config) Nothing []) "VM" grant
          ok `shouldBe` False
          err `shouldSatisfy` isInfixOf "Failed"
          err `shouldSatisfy` (not . isInfixOf "secret")
          doesFileExist (dir </> "path") `shouldReturn` False
    it "times out a stuck encryption helper" $ fixture $ \dir config -> do
      script (ccRemmina config) $ remminaScript dir 0 "exec /bin/sleep 30" (recordProfile dir)
      (_, result) <- hCapture [stderr] $ timeout 15000000 $ runViewer (ViewerLaunch Remmina (ccRemmina config) Nothing []) "VM" grant
      result `shouldBe` Just False
      doesFileExist (dir </> "path") `shouldReturn` False
    it "removes the profile and propagates cancellation" $ fixture $ \dir config -> do
      script (ccRemmina config) $ remminaScript dir 0 "printf 'Encrypted password: MDEyMzQ1Njc=\\n'; exit 0" (recordProfile dir ++ "\nexec /bin/sleep 30\n")
      withAsync (runViewer (ViewerLaunch Remmina (ccRemmina config) Nothing []) "VM" grant) $ \running -> do
        ready <- timeout 3000000 (awaitFile (dir </> "profile"))
        ready `shouldBe` Just ()
        cancel running
        waitCatch running >>= (`shouldSatisfy` isLeft)
      assertPrivateCleanup dir
  it "sanitizes executable launch failures" $ fixture $ \dir _ -> do
    (err, ok) <- hCapture [stderr] $ runViewer (ViewerLaunch VirtViewer (dir </> "missing") Nothing []) "VM" grant
    ok `shouldBe` False
    err `shouldSatisfy` isInfixOf "Failed"

grant :: ViewGrant
grant = ViewGrant "host" 5900 "secret" 30

fixture :: (FilePath -> ClientConfig -> IO a) -> IO a
fixture action = withSystemTempDirectory "corvus viewer " $ \dir ->
  action dir (ClientConfig (dir </> "remmina") (dir </> "remote-viewer"))

script :: FilePath -> String -> IO ()
script path body = writeFile path ("#!/bin/sh\nset -eu\n" ++ body ++ "\n") >> setFileMode path 0o700

recordProfile :: FilePath -> String
recordProfile dir =
  unlines
    [ "printf '%s\\n' \"$1\" > '" ++ dir </> "path" ++ "'"
    , "stat -c %a \"$1\" > '" ++ dir </> "mode" ++ "'"
    , "cat \"$1\" > '" ++ dir </> "profile" ++ "'"
    ]

-- Check every argument before treating the final one as a profile path.
remminaScript :: FilePath -> Int -> String -> String -> String
remminaScript dir extraCount helper viewer =
  unlines
    [ "test \"$1\" = --gapplication-app-id"
    , "app_id=$2"
    , "test \"$3\" = --no-tray-icon"
    , "case \"$4\" in"
    , "--encrypt-password)"
    , "test \"$#\" = 4"
    , "printf '%s\\n' \"$app_id\" > '" ++ dir </> "helper-id" ++ "'"
    , "printf '%s\\n' \"$LC_ALL\" > '" ++ dir </> "locale" ++ "'"
    , "IFS= read -r password"
    , "printf '%s\\n' \"$password\" > '" ++ dir </> "password" ++ "'"
    , helper
    , ";;"
    , "*)"
    , "test \"$#\" = " ++ show (5 + extraCount)
    , "printf '%s\\n' \"$app_id\" > '" ++ dir </> "viewer-id" ++ "'"
    , "shift 3"
    , "printf '%s\\000' \"$@\" > '" ++ dir </> "args" ++ "'"
    , "shift " ++ show extraCount
    , "test \"$1\" = --connect"
    , "shift"
    , viewer
    , ";;"
    , "esac"
    ]

assertPrivateCleanup :: FilePath -> IO ()
assertPrivateCleanup dir = do
  readFile (dir </> "mode") `shouldReturn` "600\n"
  path <- T.unpack . T.strip <$> TIO.readFile (dir </> "path")
  doesFileExist path `shouldReturn` False

awaitFile :: FilePath -> IO ()
awaitFile path = do
  exists <- doesFileExist path
  if exists then void (readFile path) else threadDelay 1000 >> awaitFile path
