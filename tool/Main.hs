module Main where

import StatusNotifier.Watcher.Client
import DBus.Client
import Data.String

main = do
  client <- connectSession
  registeredItems <-
    getRegisteredStatusNotifierItems
      client
  print registeredItems
  return ()
