{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Item.Constants where

import DBus.Generation
import DBus.Introspection
import Data.Maybe
import System.IO.Unsafe

import StatusNotifier.Util

{-# NOINLINE introspectionObject #-}
introspectionObject = unsafePerformIO $
  head . maybeToList . parseXML "/" <$>
  (getXMLDataFile "StatusNotifierItem.xml" >>= readFile)

introspectionInterface =
  head $ objectInterfaces introspectionObject

generationParams =
  defaultGenerationParams
  { genObjectPath = Just $ objectPath introspectionObject }
