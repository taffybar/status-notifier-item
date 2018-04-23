{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import           DBus.Generation
import           DBus.Internal.Types
import qualified DBus.Internal.Message as M
import qualified DBus.TH as DTH
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Either
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String
import           Data.Unique
import           Data.Word
import           System.Log.Logger
import           Text.Printf

import qualified StatusNotifier.Item.Client as I
import qualified StatusNotifier.Item.Constants as I
import           StatusNotifier.Util
import qualified StatusNotifier.Watcher.Client as W
import qualified StatusNotifier.Watcher.Constants as W
import qualified StatusNotifier.Watcher.Signals as W
import qualified StatusNotifier.Watcher.Service as W

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
  | TitleUpdated
  | TooltipUpdated deriving (Eq, Show)

type UpdateHandler = UpdateType -> ItemInfo -> IO ()

data Params = Params
  { dbusClient :: Maybe Client
  , uniqueIdentifier :: String
  , namespace :: String
  , startWatcher :: Bool
  }

hostLogger = logM "StatusNotifier.Host.Service"

defaultParams = Params
  { dbusClient = Nothing
  , uniqueIdentifier = ""
  , namespace = "org.kde"
  , startWatcher = False
  }

data ItemInfo = ItemInfo
  { itemServiceName :: BusName
  , itemServicePath :: ObjectPath
  , iconTitle :: String
  , iconName :: String
  , iconThemePath :: Maybe String
  , iconPixmaps :: [(Int32, Int32, BS.ByteString)]
  , menuPath :: Maybe ObjectPath
  } deriving (Eq, Show)

defaultItemInfo =
  ItemInfo
  { itemServiceName = "a.b"
  , itemServicePath = "/"
  , iconThemePath = Nothing
  , iconName = ""
  , iconTitle = ""
  , iconPixmaps = []
  , menuPath = Nothing
  }

makeLensesWithLSuffix ''ItemInfo

convertPixmapsToHostByteOrder ::
  [(Int32, Int32, BS.ByteString)] -> [(Int32, Int32, BS.ByteString)]
convertPixmapsToHostByteOrder = map $ over _3 networkToSystemByteOrder

callFromInfo fn ItemInfo { itemServiceName = name
                         , itemServicePath = path
                         } = fn name path

data Host = Host
  { itemInfoMap :: IO (Map.Map BusName ItemInfo)
  , addUpdateHandler :: UpdateHandler -> IO Unique
  , removeUpdateHandler :: Unique -> IO ()
  }

build :: Params -> IO (Maybe Host)
build Params { dbusClient = mclient
             , namespace = namespaceString
             , uniqueIdentifier = uniqueID
             , startWatcher = shouldStartWatcher
             } = do
  client <- maybe connectSession return mclient
  itemInfoMapVar <- newMVar Map.empty
  updateHandlersVar <- newMVar ([] :: [(Unique, UpdateHandler)])
  let busName = getBusName namespaceString uniqueID

      logError = hostLogger ERROR
      logErrorWithMessage message error = logError message >> logError (show error)
      logInfo = hostLogger INFO
      logErrorAndThen andThen e = logError (show e) >> andThen

      doUpdateForHandler utype uinfo (unique, handler) = do
        logInfo (printf "Sending update (iconPixmaps suppressed): %s %s, for handler %s"
                          (show utype)
                          (show $ uinfo { iconPixmaps = [] })
                          (show $ hashUnique unique))
        forkIO $ handler utype uinfo

      doUpdate utype uinfo =
        readMVar updateHandlersVar >>= mapM_ (doUpdateForHandler utype uinfo)

      addHandler handler = do
        unique <- newUnique
        modifyMVar_ updateHandlersVar (return . ((unique, handler):))
        let doUpdateForInfo info = doUpdateForHandler ItemAdded info (unique, handler)
        readMVar itemInfoMapVar >>= mapM_ doUpdateForInfo
        return unique

      removeHandler unique =
        modifyMVar_ updateHandlersVar (return . filter ((/= unique) . fst))

      getPixmaps a1 a2 a3 = fmap convertPixmapsToHostByteOrder <$>
                            I.getIconPixmap a1 a2 a3

      buildItemInfo name = runExceptT $ do
        pathString <- ExceptT $ W.getObjectPathForItemName client name
        let busName = fromString name
            path = objectPath_ pathString
            getMaybe fn a b c = right Just <$> fn a b c
            doGetDef def fn =
              ExceptT $ exemptAll def <$> fn client busName path
            doGet fn = ExceptT $ fn client busName path
        pixmaps <- doGetDef [] getPixmaps
        iName <- doGetDef name I.getIconName
        themePath <- doGetDef Nothing $ getMaybe I.getIconThemePath
        menu <- doGetDef Nothing $ getMaybe I.getMenu
        title <- doGetDef "" I.getTitle
        return ItemInfo
                 { itemServiceName = busName_ name
                 , itemServicePath = path
                 , iconPixmaps = pixmaps
                 , iconThemePath = themePath
                 , iconName = iName
                 , iconTitle = title
                 , menuPath = menu
                 }

      createAll serviceNames = do
        (errors, itemInfos) <-
          partitionEithers <$> mapM buildItemInfo serviceNames
        mapM_ (logErrorWithMessage "Error in item building at startup:") errors
        return itemInfos

      registerWithPairs =
        mapM (uncurry clientSignalRegister)
        where logUnableToCallSignal signal =
                hostLogger ERROR $ printf "Unable to call handler with %s" $
                     show signal
              clientSignalRegister signalRegisterFn handler =
                signalRegisterFn client matchAny handler logUnableToCallSignal

      handleItemAdded _ serviceName =
        modifyMVar_ itemInfoMapVar $ \itemInfoMap ->
          buildItemInfo serviceName >>=
                        either (logErrorAndThen $ return itemInfoMap)
                                 (addItemInfo itemInfoMap)
          where addItemInfo map itemInfo = doUpdate ItemAdded itemInfo >>
                  return (Map.insert (itemServiceName itemInfo) itemInfo map)

      getObjectPathForItemName name =
        maybe I.defaultPath itemServicePath . Map.lookup name <$>
        readMVar itemInfoMapVar

      handleItemRemoved _ serviceName = let busName = busName_ serviceName in
        modifyMVar_ itemInfoMapVar (return . Map.delete busName ) >>
        doUpdate ItemRemoved defaultItemInfo { itemServiceName = busName }

      watcherRegistrationPairs =
        [ (W.registerForStatusNotifierItemRegistered, handleItemAdded)
        , (W.registerForStatusNotifierItemUnregistered, handleItemRemoved)
        ]

      getSender fn s@M.Signal { M.signalSender = Just sender} =
        logInfo (show s) >> fn sender
      getSender _ s = logError $ "Received signal with no sender: " ++ show s

      logPropError = logErrorWithMessage "Error updating property: "

      makeUpdaterFromProp = makeUpdaterFromProp' logPropError

      makeUpdaterFromProp' onError lens updateType prop = getSender run
        where run sender =
                getObjectPathForItemName sender >>=
                prop client sender >>=
                either onError (runUpdate lens updateType sender)

      runUpdate lens updateType sender newValue =
        modifyMVar itemInfoMapVar modify >>= callUpdate
          where modify infoMap =
                  let newMap = set (at sender . non defaultItemInfo . lens)
                               newValue infoMap
                  in return (newMap, Map.lookup sender newMap)
                callUpdate = flip whenJust (doUpdate updateType)

      updatePixmaps =
        makeUpdaterFromProp iconPixmapsL IconUpdated getPixmaps
      handleNewIcon signal =
        makeUpdaterFromProp'
        (const $ updatePixmaps signal)
        iconNameL IconNameUpdated I.getIconName signal
      handleNewTitle =
        makeUpdaterFromProp iconTitleL TitleUpdated I.getTitle

      clientRegistrationPairs =
        [ (I.registerForNewIcon, handleNewIcon)
        , (I.registerForNewTitle, handleNewTitle)
        ]

      initializeItemInfoMap = modifyMVar itemInfoMapVar $ \itemInfoMap -> do
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
              logError (show error) >> shutdownHost >> return (Map.empty, False)
            finishInitialization serviceNames = do
              itemInfos <- createAll serviceNames
              let newMap = Map.fromList $ map (itemServiceName &&& id) itemInfos
                  -- Extra paranoia about the map
                  resultMap = if Map.null itemInfoMap
                              then newMap
                              else Map.union itemInfoMap newMap
              W.registerStatusNotifierHost client busName >>=
               either logErrorAndShutdown (const $ return (resultMap, True))
        W.getRegisteredStatusNotifierItems client >>=
         either logErrorAndShutdown finishInitialization

      startWatcherIfNeeded = do
        let watcherName = maybe "" coerce $ genBusName W.watcherClientGenerationParams
            startWatcher = do
              (_, doIt) <- W.buildWatcher W.defaultWatcherParams
              doIt
        res <- DTH.getNameOwner client watcherName
        case res of
          Right _ -> return ()
          Left _ -> void $ forkIO $ void startWatcher

  when shouldStartWatcher startWatcherIfNeeded
  nameRequestResult <- requestName client (fromString busName) []
  if nameRequestResult == NamePrimaryOwner
  then do
    initializationSuccess <- initializeItemInfoMap
    return $ if initializationSuccess
    then
      Just Host
      { itemInfoMap = readMVar itemInfoMapVar
      , addUpdateHandler = addHandler
      , removeUpdateHandler = removeHandler
      }
    else Nothing
  else do
    logErrorWithMessage "Failed to obtain desired service name" nameRequestResult
    return Nothing

