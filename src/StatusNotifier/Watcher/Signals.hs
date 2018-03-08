{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Watcher.Signals where

import DBus.Generate
import Language.Haskell.TH

import StatusNotifier.Watcher.Constants

generateSignals watcherClientGenerationParams
                defaultWatcherInterfaceName watcherSignals
