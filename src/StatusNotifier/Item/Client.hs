{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Item.Client where

import DBus.Generation
import StatusNotifier.Item.Constants as C
import Language.Haskell.TH

generateClient C.generationParams C.introspectionInterface
generateSignalsFromInterface C.generationParams C.introspectionInterface

printItemClient =
  runQ (generateClient C.generationParams C.introspectionInterface) >>=
       putStrLn . pprint
