module Main where

import StatusNotifier.Watcher
import Control.Concurrent.MVar

main = do
  stop <- newEmptyMVar
  startWatcher defaultWatcherParams { watcherStop = putMVar stop True }
  takeMVar stop
