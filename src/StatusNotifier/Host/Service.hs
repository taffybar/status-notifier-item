{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module StatusNotifier.Host.Service where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Lens.Tuple
import           Control.Monad
import           Control.Monad.Except
import           DBus
import           DBus.Client
import qualified DBus.Internal.Message as M
import qualified Data.ByteString as BS
import           Data.Either
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String
import           Data.Word
import           System.Log.Logger
import           Text.Printf

import qualified StatusNotifier.Item.Constants as I
import qualified StatusNotifier.Item.Client as I
import           StatusNotifier.Util
import qualified StatusNotifier.Watcher.Client as W
import qualified StatusNotifier.Watcher.Signals as W

statusNotifierHostString :: String
statusNotifierHostString = "StatusNotifierHost"

getBusName :: String -> String -> String
getBusName namespace =
  printf "%s.%s-%s" namespace statusNotifierHostString

data UpdateType
  = ItemAdded
  | ItemRemoved
  | IconUpdated
  | IconNameUpdated
  | TooltipUpdated deriving (Eq, Show)

data Params = Params
  { dbusClient :: Maybe Client
  , uniqueIdentifier :: String
  , namespace :: String
  , handleUpdate :: UpdateType -> ItemInfo -> IO ()
  , hostLogger :: Logger
  }

defaultParams = Params
  { dbusClient = Nothing
  , uniqueIdentifier = ""
  , namespace = "org.kde"
  , handleUpdate = \_ _ -> return ()
  , hostLogger = makeDefaultLogger "StatusNotifier.Watcher.Service"
  }

data ItemInfo = ItemInfo
  { itemServiceName :: BusName
  , itemServicePath :: ObjectPath
  , iconName :: String
  , iconThemePath :: Maybe String
  , iconPixmaps :: [(Int32, Int32, BS.ByteString)]
  , menuPath :: ObjectPath
  } deriving (Eq, Show)

defaultItemInfo =
  ItemInfo
  { itemServiceName = ""
  , itemServicePath = ""
  , iconThemePath = Nothing
  , iconName = ""
  , iconPixmaps = []
  , menuPath = "/"
  }

makeLensesWithLSuffix ''ItemInfo

convertPixmapsToHostByteOrder ::
  [(Int32, Int32, BS.ByteString)] -> [(Int32, Int32, BS.ByteString)]
convertPixmapsToHostByteOrder = map $ over _3 networkToSystemByteOrder

callFromInfo fn ItemInfo { itemServiceName = name
                         , itemServicePath = path
                         } = fn name path

build :: Params -> IO (IO RequestNameReply)
build Params { dbusClient = mclient
             , namespace = namespaceString
             , uniqueIdentifier = uniqueID
             , handleUpdate = updateHandler
             , hostLogger = logger
             } = do
  client <- maybe connectSession return mclient
  itemInfoMapVar <- newMVar Map.empty
  let busName = getBusName namespaceString uniqueID

      logError = logL logger ERROR
      logErrorM message error = logError message >> (logError $ show error)
      logInfo = logL logger INFO
      logErrorAndThen andThen e = logError (show e) >> andThen

      doUpdate utype uinfo =
        (logInfo (printf "Sending update: %s %s" (show utype)
                           (show $ uinfo { iconPixmaps = [] }))) >>
        (void $ forkIO $ updateHandler utype uinfo)

      getPixmaps a1 a2 a3 = fmap convertPixmapsToHostByteOrder <$> I.getIconPixmap a1 a2 a3

      buildItemInfo name = runExceptT $ do
        pathString <- ExceptT $ W.getObjectPathForItemName client name
        let busName = fromString name
            path = objectPath_ pathString
            getThemePath a b c = (right Just <$> I.getIconThemePath a b c)
            doGetDef def fn =
              ExceptT $ (exemptAll def) <$> fn client busName path
            doGet fn = ExceptT $ fn client busName path
        pixmaps <- doGetDef [] getPixmaps
        iName <- doGetDef name I.getIconName
        themePath <- doGetDef Nothing getThemePath
        menu <- doGetDef path I.getMenu
        return ItemInfo
                 { itemServiceName = busName_ name
                 , itemServicePath = path
                 , iconPixmaps = pixmaps
                 , iconThemePath = themePath
                 , iconName = iName
                 , menuPath = menu
                 }

      createAll serviceNames = do
        (errors, itemInfos) <- partitionEithers <$> mapM buildItemInfo serviceNames
        mapM_ (logErrorM "Error in item building at startup:") errors
        return itemInfos

      registerWithPairs =
        mapM (uncurry clientSignalRegister)
        where clientSignalRegister signalRegisterFn =
                signalRegisterFn client matchAny

      handleItemAdded _ serviceName =
        modifyMVar_ itemInfoMapVar $ \itemInfoMap ->
          buildItemInfo serviceName >>=
                        either (logErrorAndThen $ return itemInfoMap)
                                 (addItemInfo itemInfoMap)
          where addItemInfo map itemInfo = forkIO (updateHandler ItemAdded itemInfo) >>
                  return (Map.insert (itemServiceName itemInfo) itemInfo map)

      getObjectPathForItemName name = (fromMaybe I.defaultPath) .
        (fmap itemServicePath) . Map.lookup name <$> readMVar itemInfoMapVar

      handleItemRemoved _ serviceName = let busName = busName_ serviceName in
        modifyMVar_ itemInfoMapVar (return . Map.delete busName ) >>
        doUpdate ItemRemoved defaultItemInfo { itemServiceName = busName }

      watcherRegistrationPairs =
        [ (W.registerForStatusNotifierItemRegistered, handleItemAdded)
        , (W.registerForStatusNotifierItemUnregistered, handleItemRemoved)
        ]

      getSender fn M.Signal { M.signalSender = Just sender} = fn sender
      getSender _ s = logError $ "Received signal with no sender:" ++ show s

      makeUpdaterFromProp lens updateType prop = getSender run
        where run sender =
                getObjectPathForItemName sender >>=
                prop client sender >>=
                either (logErrorM "Error updating property:")
                       (runUpdate lens updateType sender)

      runUpdate lens updateType sender newValue =
        modifyMVar itemInfoMapVar modify >>= callUpdate
          where modify infoMap =
                  let newMap = set (at sender . non defaultItemInfo . lens) newValue infoMap
                  in return (newMap, Map.lookup sender newMap)
                callUpdate = flip whenJust (doUpdate updateType)

      handleIconUpdated =
        makeUpdaterFromProp iconPixmapsL IconUpdated getPixmaps
      handleIconNameUpdated =
        makeUpdaterFromProp iconNameL IconNameUpdated I.getIconName

      clientRegistrationPairs =
        [ (I.registerForNewTitle, handleIconNameUpdated)
        , (I.registerForNewIcon, handleIconUpdated)
        ]

      initializeItemInfoMap = modifyMVar_ itemInfoMapVar $ \itemInfoMap -> do
        -- All initialization is done inside this modifyMvar to avoid race
        -- conditions with the itemInfoMapVar.
        clientSignalHandlers <- registerWithPairs clientRegistrationPairs
        watcherSignalHandlers <- registerWithPairs watcherRegistrationPairs
        let unregisterAll =
                mapM_ (removeMatch client) $
                    clientSignalHandlers ++ watcherSignalHandlers
            shutdownHost = do
                logInfo "Shutting down StatusNotifierHost"
                unregisterAll
                releaseName client (fromString busName)
                return ()
            logErrorAndShutdown error =
              logError (show error) >> shutdownHost >> return Map.empty
            finishInitialization serviceNames = do
              itemInfos <- createAll serviceNames
              mapM_ (doUpdate ItemAdded) itemInfos
              let newMap = Map.fromList $ map (itemServiceName &&& id) itemInfos
                  -- Extra paranoia about the map
                  resultMap = if Map.null itemInfoMap then newMap else Map.union itemInfoMap newMap
              W.registerStatusNotifierHost client busName >>=
               either logErrorAndShutdown (const $ return resultMap)
        W.getRegisteredStatusNotifierItems client >>=
         either logErrorAndShutdown finishInitialization

      startup =
        do
          nameRequestResult <- requestName client (fromString busName) []
          when (nameRequestResult == NamePrimaryOwner) initializeItemInfoMap
          return nameRequestResult
  return startup
