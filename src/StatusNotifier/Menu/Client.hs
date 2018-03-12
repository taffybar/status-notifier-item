{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Menu.Client where

import DBus.Generation
import StatusNotifier.Menu.Constants

generateClient defaultGenerationParams introspectionInterface
generateSignalsFromInterface defaultGenerationParams introspectionInterface
