module Main where

import Control.Concurrent.MVar
import Data.Semigroup ((<>))
import Options.Applicative
import StatusNotifier.Watcher.Constants
import StatusNotifier.Watcher.Service
import System.Log.Logger

setWatcherParams namespace path =
  defaultWatcherParams
  { watcherNamespace = namespace
  , watcherPath = path
  }

watcherParamsParser :: Parser WatcherParams
watcherParamsParser = setWatcherParams
  <$> strOption
  (  long "namespace"
  <> short 'n'
  <> metavar "NAMESPACE"
  <> value "org.kde"
  <> help "The namespace the watcher should register at.")
  <*> strOption
  (  long "path"
  <> short 'p'
  <> metavar "DBUS-PATH"
  <> value "/StatusNotifierWatcher"
  <> help "The path at which to run the watcher." )

main = do
  watcherParams <- execParser $
                   info (watcherParamsParser <**> helper)
                   (  fullDesc
                   <> progDesc "Run a StatusNotifierWatcher")
  stop <- newEmptyMVar
  (_, startWatcher) <- buildWatcher watcherParams { watcherStop = putMVar stop () }
  startWatcher
  takeMVar stop
