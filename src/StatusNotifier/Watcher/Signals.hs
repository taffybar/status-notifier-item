{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Watcher.Signals where

import DBus.Generation
import Language.Haskell.TH

import StatusNotifier.Watcher.Constants

generateSignals watcherClientGenerationParams
                defaultWatcherInterfaceName watcherSignals

printWatcherSignals =
  runQ (generateSignals watcherClientGenerationParams
                defaultWatcherInterfaceName watcherSignals) >>=
       putStrLn . pprint
