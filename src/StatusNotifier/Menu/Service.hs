{-# LANGUAGE TemplateHaskell #-}

module StatusNotifier.Menu.Service where


import           Control.Concurrent.MVar
import           Control.Lens hiding ((??))
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           DBus.Client
import           DBus.Generation
import           DBus.Internal.Types as T
import           Data.Int
import qualified Data.Map as Map
import           Data.Maybe
import           System.Log.Logger

import qualified StatusNotifier.Menu.Client as C
import           StatusNotifier.Util

data EntryType
  = Separator
  | Menu
  | Entry deriving (Eq, Show)

data MenuEntry = MenuEntry
  { menuID :: Int32
  , entryType :: EntryType
  , label :: String
  , childIDs :: [Int32]
  , enabled :: Bool
  , visible :: Bool
  , iconName :: Maybe String
  -- TODO: handle toggles
  } deriving (Eq, Show)

makeLensesWithLSuffix ''MenuEntry

defaultMenuEntry = MenuEntry
  { menuID = (-1)
  , entryType = Entry
  , label = ""
  , childIDs = []
  , enabled = True
  , visible = True
  , iconName = Nothing
  }

lookupProperty ::
  (IsVariant b, Ord k) => k -> Map.Map k Variant -> Maybe b
lookupProperty propertyName properties =
  Map.lookup propertyName properties >>= fromVariant

getEntryType :: Map.Map String Variant -> EntryType
getEntryType properties =
  case (lookupProperty "type" properties, lookupProperty "children-display" properties) of
    (_, Just "submenu") -> Menu
    (Just "separator", _) -> Separator
    _ -> Entry

layoutProperties =
  ["type", "children-display", "visible", "enabled", "label", "icon-name"]

processMenuEntry (thisID, properties, children) =
  let unpackedChildren = catMaybes $ map T.fromVariant children
      descendants = unpackedChildren >>= processMenuEntry
      thisEntry = MenuEntry
                  { menuID = thisID
                  , childIDs = map (\(childID, _, _) -> childID) unpackedChildren
                  , label = fromMaybe "" $ lookupProperty "label" properties
                  , enabled = fromMaybe True $ lookupProperty "enabled" properties
                  , visible = fromMaybe True $ lookupProperty "visible" properties
                  , iconName = lookupProperty "icon-name" properties
                  , entryType = getEntryType properties
                  }
  in
    (thisID, thisEntry) : descendants

getMenuEntries = runExceptT $ do
  (_, rootEntry) <- ExceptT $ dbusInvoke C.getLayout ?? 0 ?? (-1) ?/? layoutProperties
  let results = processMenuEntry rootEntry
  return $ Map.fromList results

data MenuParams = MenuParams
  { menuLogger :: Logger
  }

defaultMenuParams =
  MenuParams { menuLogger = makeDefaultLogger "StatusNotifier.Menu.Service" }

buildMenu MenuParams { menuLogger = logger } = do
  info@(client, busName, path) <- ask
  menuEntriesMapVar <- lift $ newMVar Map.empty

  let run = flip runReaderT info
      matchRule = matchAny { matchSender = Just busName
                           , matchPath = Just path
                           }
      -- TODO: handle updates
      handleLayoutUpdated _ _ _ = return ()
      handlePropertiesUpdated _ _ _ = return()

  stop <- lift $ modifyMVar menuEntriesMapVar $ \menuEntriesMap ->
    do
      entries <- run getMenuEntries >>=
                 logErrorWithDefault logger Map.empty
                 "Failed to retrieve menu entries."
      layoutUpdated <-
        C.registerForLayoutUpdated client matchRule handleLayoutUpdated
      propertiesUpdated <-
        C.registerForItemsPropertiesUpdated client matchRule handleLayoutUpdated
      let stop = mapM_ (removeMatch client) [layoutUpdated, propertiesUpdated]
      return (entries, stop)

  return (menuEntriesMapVar, stop)
