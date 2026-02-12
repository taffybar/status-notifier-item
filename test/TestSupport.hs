{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestSupport
  ( withIsolatedSessionBus
  , startWatcher
  , registerSimpleItem
  , waitFor
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, finally, try)
import Control.Monad (filterM)
import DBus (busName_, objectPath_)
import DBus.Client
  ( Client
  , Interface (..)
  , RequestNameReply (NamePrimaryOwner)
  , connectSession
  , export
  , readOnlyProperty
  , releaseName
  , requestName
  )
import StatusNotifier.Watcher.Constants
  ( defaultWatcherParams
  , watcherDBusClient
  , watcherStop
  )
import qualified StatusNotifier.Watcher.Client as WatcherClient
import qualified StatusNotifier.Watcher.Service as WatcherService
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode, ExitCode (ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

data BusEnv = BusEnv
  { previousAddress :: Maybe String
  , previousPid :: Maybe String
  , daemonPid :: String
  }

findDbusSessionConfig :: IO (Maybe FilePath)
findDbusSessionConfig = do
  mExe <- findExecutable "dbus-daemon"
  case mExe of
    Nothing -> pure Nothing
    Just exe -> do
      let prefix = takeDirectory (takeDirectory exe)
          candidates =
            [ prefix </> "share" </> "dbus-1" </> "session.conf"
            , prefix </> "etc" </> "dbus-1" </> "session.conf"
            ]
      existing <- filterM doesFileExist candidates
      pure $ case existing of
        c : _ -> Just c
        _ -> Nothing

withIsolatedSessionBus :: ActionWith () -> IO ()
withIsolatedSessionBus action = do
  setupResult <- (try setup :: IO (Either SomeException BusEnv))
  case setupResult of
    Right env -> action () `finally` teardown env
    Left e ->
      pendingWith $
        "Skipping D-Bus integration test: "
          <> displayException e

setup :: IO BusEnv
setup = do
  oldAddress <- lookupEnv "DBUS_SESSION_BUS_ADDRESS"
  oldPid <- lookupEnv "DBUS_SESSION_BUS_PID"
  mConfig <- findDbusSessionConfig
  let baseArgs = ["--fork", "--print-address=1", "--print-pid=1"]
      args =
        case mConfig of
          Just configPath -> ["--config-file=" <> configPath] <> baseArgs
          Nothing -> ["--session"] <> baseArgs
  (code, out, err) <-
    readProcessWithExitCode
      "dbus-daemon"
      args
      ""
  case (code, lines out) of
    (ExitSuccess, address : pidLine : _) -> do
      setEnv "DBUS_SESSION_BUS_ADDRESS" address
      setEnv "DBUS_SESSION_BUS_PID" pidLine
      pure BusEnv
        { previousAddress = oldAddress
        , previousPid = oldPid
        , daemonPid = pidLine
        }
    _ ->
      error $
        "Failed to start test dbus-daemon: exit="
          <> show code
          <> " stdout="
          <> show out
          <> " stderr="
          <> show err

teardown :: BusEnv -> IO ()
teardown BusEnv {..} = do
  -- Terminate bus daemon. If it is already gone, ignore the error.
  _ <-
    ( try $
        readProcessWithExitCode
          "kill"
          ["-TERM", daemonPid]
          ""
    ) ::
      IO (Either SomeException (ExitCode, String, String))
  restore "DBUS_SESSION_BUS_ADDRESS" previousAddress
  restore "DBUS_SESSION_BUS_PID" previousPid
  where
    restore key value = maybe (unsetEnv key) (setEnv key) value

startWatcher :: IO Client
startWatcher = do
  client <- connectSession
  (_, startFn) <-
    WatcherService.buildWatcher
      defaultWatcherParams
        { watcherDBusClient = Just client
        , watcherStop = pure ()
        }
  reply <- startFn
  reply `shouldBe` NamePrimaryOwner
  pure client

registerSimpleItem ::
  Client ->
  String ->
  String ->
  String ->
  IO (IO ())
registerSimpleItem client busName objectPath iconName = do
  let iface =
        Interface
          { interfaceName = "org.kde.StatusNotifierItem"
          , interfaceMethods = []
          , interfaceProperties =
              [ readOnlyProperty "IconName" (pure iconName)
              , readOnlyProperty "OverlayIconName" (pure ("" :: String))
              , readOnlyProperty "ItemIsMenu" (pure False)
              ]
          , interfaceSignals = []
          }
      path = objectPath_ objectPath

  export client path iface
  _ <- requestName client (busName_ busName) []
  registerResult <- WatcherClient.registerStatusNotifierItem client busName
  registerResult `shouldBe` Right ()
  pure $ do
    _ <- releaseName client (busName_ busName)
    pure ()

waitFor :: Int -> IO Bool -> IO Bool
waitFor timeoutMicros predicate =
  go 0
  where
    step = 20000
    go elapsed
      | elapsed >= timeoutMicros = predicate
      | otherwise = do
          ok <- predicate
          if ok
            then pure True
            else threadDelay step >> go (elapsed + step)
