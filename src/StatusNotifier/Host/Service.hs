{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Host.Service where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Except
import           DBus
import           DBus.Client
import qualified DBus.Internal.Message as M
import qualified Data.ByteString as BS
import           Data.Either
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.String
import           Data.Word
import           System.IO.Unsafe
import           System.Log.Logger
import           Text.Printf

import qualified StatusNotifier.Item.Client as C
import qualified StatusNotifier.Watcher.Client as W
import qualified StatusNotifier.Watcher.Signals as W

statusNotifierHostString :: String
statusNotifierHostString = "StatusNotifierHost"

getBusName :: String -> String -> String
getBusName namespace =
  printf "%s.%s-%s" namespace statusNotifierHostString

data UpdateType
  = NewItem
  | NewTitle
  | ItemAdded
  | ItemRemoved

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
  , handleUpdate = (\_ _ -> return ())
  , hostLogger = unsafePerformIO $ getLogger "StatusNotifier.Host.Service"
  }

data ItemInfo = ItemInfo
  { itemServiceName :: BusName
  , iconName :: String
  , iconPixmaps :: [(Int32, Int32, BS.ByteString)]
  }

build Params { dbusClient = mclient
             , namespace = namespaceString
             , uniqueIdentifier = uniqueID
             , handleUpdate = updateHandler
             , hostLogger = logger
             } = do
  client <- maybe connectSession return mclient
  itemInfoMapVar <- newMVar Map.empty
  let busName = getBusName namespaceString uniqueID
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
              mapM_ (updateHandler NewItem) itemInfos
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
      logError = logL logger ERROR
      logInfo = logL logger INFO
      logErrorAndThen andThen e = logError (show e) >> andThen
      createAll serviceNames = do
        (errors, itemInfos) <- partitionEithers <$> mapM buildItemInfo serviceNames
        mapM_ (logError . show) errors
        return itemInfos
      handleItemAdded _ serviceName =
        modifyMVar_ itemInfoMapVar $ \itemInfoMap ->
          buildItemInfo serviceName >>=
                        either (logErrorAndThen $ return itemInfoMap)
                                 (addItemInfo itemInfoMap)
          where addItemInfo map itemInfo = forkIO (updateHandler ItemAdded itemInfo) >>
                  (return $ Map.insert (itemServiceName itemInfo) itemInfo map)
      handleItemRemoved _ serviceName = return () -- XXX: finish
      getSender fn M.Signal { M.signalSender = Just sender} = fn sender
      getSender _ s = logError $ "Received signal with no sender:" ++ show s
      handleNewTitle sender = return () -- XXX: finish
      handleNewIcon sender = return ()  -- XXX: finish
      clientRegistrationPairs = map (fmap getSender)
        [ (C.registerForNewTitle, handleNewTitle)
        , (C.registerForNewIcon, handleNewIcon)
        ]
      watcherRegistrationPairs =
        [ (W.registerForStatusNotifierItemRegistered, handleItemAdded)
        , (W.registerForStatusNotifierItemUnregistered, handleItemRemoved)
        ]
      registerWithPairs pairs =
        mapM (uncurry clientSignalRegister) pairs
        where clientSignalRegister signalRegister handler =
                signalRegister client matchAny  handler
      buildItemInfo name = runExceptT $ do
        let busName = fromString name
            doGet fn = ExceptT $ fn client busName
        pixmaps <- doGet C.getIconPixmap
        -- SNI proxy does not have this property so well need to figure
        -- something out here.
        -- iconName <- doGet C.getIconName
        return $ ItemInfo { itemServiceName = busName_ name
                          , iconPixmaps = pixmaps
                          -- , iconName = iconName
                          }
  return startup
