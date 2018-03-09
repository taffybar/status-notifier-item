{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Item.Client where

import DBus.Generation
import StatusNotifier.Item.Constants as C

generateClient C.generationParams C.introspectionInterface
generateSignalsFromInterface C.generationParams C.introspectionInterface
