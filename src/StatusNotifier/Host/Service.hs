{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module StatusNotifier.Host.Service where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Lens.Tuple
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           DBus
import           DBus.Client
import           DBus.Generation
import qualified DBus.Internal.Message as M
import           DBus.Internal.Types
import qualified DBus.TH as DTH
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Either
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String
import           Data.Typeable
import           Data.Unique
import           Data.Word
import           System.Log.Logger
import           Text.Printf

import qualified StatusNotifier.Item.Client as I
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
  | OverlayIconUpdated
  | StatusUpdated
  | TitleUpdated
  | ToolTipUpdated deriving (Eq, Show)

type UpdateHandler = UpdateType -> ItemInfo -> IO ()

data Params = Params
  { dbusClient :: Maybe Client
  , uniqueIdentifier :: String
  , namespace :: String
  , startWatcher :: Bool
  , matchSenderWhenNameOwnersUnmatched :: Bool
  }

hostLogger = logM "StatusNotifier.Host.Service"

defaultParams = Params
  { dbusClient = Nothing
  , uniqueIdentifier = ""
  , namespace = "org.kde"
  , startWatcher = False
  , matchSenderWhenNameOwnersUnmatched = True
  }

type ImageInfo = [(Int32, Int32, BS.ByteString)]

data ItemInfo = ItemInfo
  { itemServiceName :: BusName
  , itemServicePath :: ObjectPath
  , itemId :: Maybe String
  , itemStatus :: Maybe String
  , itemCategory :: Maybe String
  , itemToolTip :: Maybe (String, ImageInfo, String, String)
  , iconTitle :: String
  , iconName :: String
  , overlayIconName :: Maybe String
  , iconThemePath :: Maybe String
  , iconPixmaps :: ImageInfo
  , overlayIconPixmaps :: ImageInfo
  , menuPath :: Maybe ObjectPath
  } deriving (Eq, Show)

supressPixelData info =
  info { iconPixmaps = map (\(w, h, _) -> (w, h, "")) $ iconPixmaps info }

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
  , forceUpdate :: BusName -> IO ()
  } deriving Typeable

build :: Params -> IO (Maybe Host)
build Params { dbusClient = mclient
             , namespace = namespaceString
             , uniqueIdentifier = uniqueID
             , startWatcher = shouldStartWatcher
             , matchSenderWhenNameOwnersUnmatched = doMatchUnmatchedSender
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
                          (show $ supressPixelData uinfo)
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

      getPixmaps getter a1 a2 a3 =
        fmap convertPixmapsToHostByteOrder <$> getter a1 a2 a3

      getMaybe fn a b c = right Just <$> fn a b c

      buildItemInfo name = runExceptT $ do
        pathString <- ExceptT $ W.getObjectPathForItemName client name
        let busName = fromString name
            path = objectPath_ pathString
            doGetDef def fn =
              ExceptT $ exemptAll def <$> fn client busName path
            doGet fn = ExceptT $ fn client busName path
        pixmaps <- doGetDef [] $ getPixmaps I.getIconPixmap
        iName <- doGetDef name I.getIconName
        overlayPixmap <- doGetDef [] $ getPixmaps I.getOverlayIconPixmap
        overlayIName <- doGetDef Nothing $ getMaybe I.getOverlayIconName
        themePath <- doGetDef Nothing $ getMaybe I.getIconThemePath
        menu <- doGetDef Nothing $ getMaybe I.getMenu
        title <- doGetDef "" I.getTitle
        tooltip <- doGetDef Nothing $ getMaybe I.getToolTip
        idString <- doGetDef Nothing $ getMaybe I.getId
        status <- doGetDef Nothing $ getMaybe I.getStatus
        category <- doGetDef Nothing $ getMaybe I.getCategory
        return ItemInfo
                 { itemServiceName = busName_ name
                 , itemId = idString
                 , itemStatus = status
                 , itemCategory = category
                 , itemServicePath = path
                 , itemToolTip = tooltip
                 , iconPixmaps = pixmaps
                 , iconThemePath = themePath
                 , iconName = iName
                 , iconTitle = title
                 , menuPath = menu
                 , overlayIconName = overlayIName
                 , overlayIconPixmaps = overlayPixmap
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

      handleItemAdded serviceName =
        modifyMVar_ itemInfoMapVar $ \itemInfoMap ->
          buildItemInfo serviceName >>=
                        either (logErrorAndThen $ return itemInfoMap)
                                 (addItemInfo itemInfoMap)
          where addItemInfo map itemInfo = doUpdate ItemAdded itemInfo >>
                  return (Map.insert (itemServiceName itemInfo) itemInfo map)

      getObjectPathForItemName name =
        maybe I.defaultPath itemServicePath . Map.lookup name <$>
        readMVar itemInfoMapVar

      handleItemRemoved serviceName =
        modifyMVar itemInfoMapVar doRemove >>=
        maybe logNonExistentRemoval (doUpdate ItemRemoved)
        where
          busName = busName_ serviceName
          doRemove currentMap =
            return (Map.delete busName currentMap, Map.lookup busName currentMap)
          logNonExistentRemoval =
            hostLogger WARNING $ printf "Attempt to remove unknown item %s" $
                       show busName

      watcherRegistrationPairs =
        [ (W.registerForStatusNotifierItemRegistered, const handleItemAdded)
        , (W.registerForStatusNotifierItemUnregistered, const handleItemRemoved)
        ]

      getSender fn s@M.Signal { M.signalSender = Just sender} =
        logInfo (show s) >> fn sender
      getSender _ s = logError $ "Received signal with no sender: " ++ show s

      runProperty prop serviceName =
        getObjectPathForItemName serviceName >>= prop client serviceName

      logUnknownSender updateType signal =
        hostLogger WARNING $
                   printf "Got signal for update type: %s from unknown sender: %s"
                   (show updateType) (show signal)

      identifySender M.Signal { M.signalSender = Just sender
                              , M.signalPath = senderPath
                              } = do
        infoMap <- readMVar itemInfoMapVar
        let identifySenderBySender = return (Map.lookup sender infoMap)
            identifySenderById = fmap join $
              identifySenderById_ >>= logEitherError hostLogger "Failed to identify sender"
            identifySenderById_ = runExceptT $ do
              senderId <- ExceptT $ I.getId client sender senderPath
              let matchesSender info =
                    if itemId info == Just senderId
                    then do
                      senderNameOwner <- DTH.getNameOwner client (coerce sender)
                      infoNameOwner <- DTH.getNameOwner client (coerce $ itemServiceName info)
                      let warningMsg =
                            "Matched sender id: %s, but name owners do not \
                            \ match: %s %s. Considered match: %s."
                          warningText = printf warningMsg
                                        (show senderId)
                                        (show senderNameOwner)
                                        (show infoNameOwner)
                      when (senderNameOwner /= infoNameOwner) $
                           hostLogger WARNING warningText
                      return doMatchUnmatchedSender
                    else return False
              lift $ findM matchesSender (Map.elems infoMap)
        identifySenderBySender <||> identifySenderById
        where a <||> b = runMaybeT $ MaybeT a <|> MaybeT b
      identifySender _ = return Nothing

      updateItemByLensAndProp lens prop busName = runExceptT $ do
        newValue <- ExceptT (runProperty prop busName)
        let modify infoMap =
              -- This noops when the value is not present
              let newMap = set (at busName . _Just . lens) newValue infoMap
              in return (newMap, Map.lookup busName newMap)
        ExceptT $ maybeToEither (methodError (Serial 0) errorFailed) <$>
                modifyMVar itemInfoMapVar modify

      logErrorsHandler lens updateType prop =
        runUpdaters [updateItemByLensAndProp lens prop] updateType

      -- Run all the provided updaters with the expectation that at least one
      -- will succeed.
      runUpdatersForService updaters updateType serviceName = do
        updateResults <- mapM ($ serviceName) updaters
        let (failures, updates) = partitionEithers updateResults
            logLevel = if null updates then ERROR else DEBUG
        mapM_ (doUpdate updateType) updates
        when (not $ null failures) $
             hostLogger logLevel $ printf "Property update failures %s" $
                        show failures

      runUpdaters updaters updateType signal =
        identifySender signal >>= maybe runForAll (runUpdateForService . itemServiceName)
        where runUpdateForService = runUpdatersForService updaters updateType
              runForAll = logUnknownSender updateType signal >>
                          readMVar itemInfoMapVar >>=
                          mapM_ runUpdateForService . Map.keys

      updateIconPixmaps =
        updateItemByLensAndProp iconPixmapsL $ getPixmaps I.getIconPixmap

      updateIconName =
        updateItemByLensAndProp iconNameL I.getIconName

      updateIconTheme =
        updateItemByLensAndProp iconThemePathL getThemePathDefault

      updateFromIconThemeFromSignal signal =
        identifySender signal >>= traverse (updateIconTheme . itemServiceName)

      handleNewIcon signal = do
        -- XXX: This avoids the case where the theme path is updated before the
        -- icon name is updated when both signals are sent simultaneously
        updateFromIconThemeFromSignal signal
        runUpdaters [updateIconPixmaps, updateIconName]
                    IconUpdated signal

      updateOverlayIconName =
        updateItemByLensAndProp overlayIconNameL $
                                getMaybe I.getOverlayIconName

      updateOverlayIconPixmaps =
        updateItemByLensAndProp overlayIconPixmapsL $
                                getPixmaps I.getOverlayIconPixmap

      handleNewOverlayIcon signal = do
        updateFromIconThemeFromSignal signal
        runUpdaters [updateOverlayIconPixmaps, updateOverlayIconName]
                    OverlayIconUpdated signal

      getThemePathDefault client busName objectPath =
        right Just <$> I.getIconThemePath client busName objectPath

      handleNewTitle =
        logErrorsHandler iconTitleL TitleUpdated I.getTitle

      handleNewTooltip =
        logErrorsHandler itemToolTipL ToolTipUpdated $ getMaybe I.getToolTip

      handleNewStatus =
        logErrorsHandler itemStatusL StatusUpdated $ getMaybe I.getStatus

      clientRegistrationPairs =
        [ (I.registerForNewIcon, handleNewIcon)
        , (I.registerForNewIconThemePath, handleNewIcon)
        , (I.registerForNewOverlayIcon, handleNewOverlayIcon)
        , (I.registerForNewTitle, handleNewTitle)
        , (I.registerForNewToolTip, handleNewTooltip)
        , (I.registerForNewStatus, handleNewStatus)
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
      , forceUpdate = handleItemAdded . coerce
      }
    else Nothing
  else do
    logErrorWithMessage "Failed to obtain desired service name" nameRequestResult
    return Nothing
