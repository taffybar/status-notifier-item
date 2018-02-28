module Main where

import StatusNotifier.Watcher
import Control.Concurrent.MVar

main = do
  stop <- newEmptyMVar
  startWatcher stop
  takeMVar stop
