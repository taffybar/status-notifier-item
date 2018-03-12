{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Menu.Constants where

import DBus.Generation
import DBus.Introspection
import Data.Maybe
import Language.Haskell.TH
import System.IO.Unsafe

import StatusNotifier.Util

{-# NOINLINE introspectionObject #-}
introspectionObject = unsafePerformIO $
  head . maybeToList . parseXML "/" <$>
  readFile "xml/DBusMenu.xml"

introspectionInterface =
  head $ objectInterfaces introspectionObject
