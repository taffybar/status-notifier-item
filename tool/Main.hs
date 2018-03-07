module Main where

import StatusNotifier.Watcher.Client
import DBus.Client
import Data.String

main = do
  client <- connectSession
  registeredItems <-
    getRegisteredStatusNotifierItems
      client
      (fromString "org.kde.StatusNotifierWatcher")
      (fromString "/StatusNotifierWatcher")
  print registeredItems
  return ()
