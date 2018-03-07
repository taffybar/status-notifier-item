{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Watcher.Client where

import DBus.Generate
import Language.Haskell.TH

import StatusNotifier.Watcher.Service

generateClient watcherClientGenerationParams watcherInterface

printWatcherClient =
  runQ (generateClient watcherClientGenerationParams watcherInterface) >>=
       putStrLn . pprint
