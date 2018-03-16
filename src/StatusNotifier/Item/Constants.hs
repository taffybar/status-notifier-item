{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Item.Constants where

import DBus.Generation
import DBus.Introspection
import DBus.Internal.Types
import Data.Maybe
import Language.Haskell.TH
import System.IO.Unsafe

import StatusNotifier.Util

{-# NOINLINE introspectionObject #-}
introspectionObject = unsafePerformIO $
  head . maybeToList . parseXML "/" <$>
  readFile "xml/StatusNotifierItem.xml"

introspectionInterface =
  head $ objectInterfaces introspectionObject

defaultPath :: ObjectPath
defaultPath = objectPath introspectionObject

generationParams = defaultGenerationParams
