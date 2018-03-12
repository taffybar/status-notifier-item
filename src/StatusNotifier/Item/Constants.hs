{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Item.Constants where

import DBus.Generation
import DBus.Introspection
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

generationParams =
  defaultGenerationParams
  { genObjectPath = Just $ objectPath introspectionObject
  }
