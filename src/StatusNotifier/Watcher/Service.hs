{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module StatusNotifier.Watcher.Service where

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           DBus
import           DBus.Client
import           DBus.Generation
import           DBus.Internal.Message as M
import           DBus.Internal.Types
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import qualified DBus.TH as DBusTH
import           Data.Coerce
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified StatusNotifier.Item.Client as Item
import           StatusNotifier.Util
import           StatusNotifier.Watcher.Constants
import           StatusNotifier.Watcher.Signals
import           System.IO.Unsafe
import           System.Log.Logger
import           Text.Printf

buildWatcher WatcherParams
               { watcherNamespace = interfaceNamespace
               , watcherStop = stopWatcher
               , watcherPath = path
               , watcherDBusClient = mclient
               } = do
  let watcherInterfaceName = getWatcherInterfaceName interfaceNamespace
      logNamespace = "StatusNotifier.Watcher.Service"
      log = logM logNamespace  INFO
      logError = logM logNamespace ERROR
      mkLogCb cb msg = lift (log (show msg)) >> cb msg
      mkLogMethod method = method { methodHandler = mkLogCb $ methodHandler method }
      mkLogProperty name fn =
        readOnlyProperty name $ log (coerce name ++ " Called") >> fn

  client <- maybe connectSession return mclient

  notifierItems <- newMVar []
  notifierHosts <- newMVar []

  let itemIsRegistered item items =
        isJust $ find (== item) items
      renderServiceName ItemEntry { serviceName = busName
                                  , servicePath = path
                                  } =
        let bus = coerce busName
            objPath = coerce path
            defaultPath = coerce Item.defaultPath
        in if objPath == defaultPath
           then bus
           else bus ++ objPath

      registerStatusNotifierItem MethodCall
                                   { methodCallSender = sender }
                                 name = runExceptT $ do
        let parsedBusName = T.parseBusName name
            parseServiceError = makeErrorReply errorInvalidParameters $
              printf "the provided service %s could not be parsed \
                     \as a bus name or an object path." name
            senderMissingError = makeErrorReply errorInvalidParameters $
              "Unable to identify sender for registration."
            path = fromMaybe Item.defaultPath $ T.parseObjectPath name
            remapErrorName =
              left $ (`makeErrorReply` "Failed to verify ownership.") .
                   M.methodErrorName
            resolveOwner bus
              | ":" `isPrefixOf` (coerce bus :: String) = return (Just bus)
              | otherwise = do
                  result <- DBusTH.getNameOwner client (coerce bus)
                  return $ busName_ <$> either (const Nothing) Just result
        when (isNothing parsedBusName && isNothing (T.parseObjectPath name)) $
          throwE parseServiceError
        senderName <- ExceptT $ return $ maybeToEither senderMissingError sender
        busName <-
          case parsedBusName of
            Just providedBusName -> do
              owner <- ExceptT $ remapErrorName <$>
                       DBusTH.getNameOwner client (coerce providedBusName)
              unless (owner == coerce senderName) $
                throwE $ makeErrorReply errorInvalidParameters $
                  printf "Sender %s does not own service %s."
                    (coerce senderName :: String)
                    name
              return providedBusName
            Nothing -> return senderName
        let item = ItemEntry { serviceName = busName
                             , servicePath = path
                             }
        lift $ modifyMVar_ notifierItems $ \currentItems ->
          do
            ownerPathMatches <- filterM (\existingItem ->
              if servicePath existingItem == path
              then do
                existingOwner <- resolveOwner (serviceName existingItem)
                return $ existingOwner == Just senderName
              else return False) currentItems
            if itemIsRegistered item currentItems || not (null ownerPathMatches)
            then return currentItems
            else do
              emitStatusNotifierItemRegistered client $ renderServiceName item
              return $ item : currentItems

      registerStatusNotifierHost name =
        let item = ItemEntry { serviceName = busName_ name
                             , servicePath = "/StatusNotifierHost"
                             } in
        modifyMVar_ notifierHosts $ \currentHosts ->
          if itemIsRegistered item currentHosts
          then
            return currentHosts
          else
            do
              emitStatusNotifierHostRegistered client
              return $ item : currentHosts

      registeredStatusNotifierItems :: IO [String]
      registeredStatusNotifierItems =
        map renderServiceName <$> readMVar notifierItems

      registeredSNIEntries :: IO [(String, String)]
      registeredSNIEntries =
        map getTuple <$> readMVar notifierItems
          where getTuple (ItemEntry bname path) = (coerce bname, coerce path)

      objectPathForItem :: String -> IO (Either Reply String)
      objectPathForItem name =
        case splitServiceName name of
          (_, Just path) -> return $ Right path
          (bus, Nothing) ->
            maybeToEither notFoundError . fmap (coerce . servicePath) .
            find ((== busName_ bus) . serviceName) <$>
            readMVar notifierItems
        where notFoundError =
                makeErrorReply errorInvalidParameters $
                printf "Service %s is not registered." name

      isStatusNotifierHostRegistered = not . null <$> readMVar notifierHosts

      protocolVersion = return 0 :: IO Int32

      filterDeadService :: String -> MVar [ItemEntry] -> IO [ItemEntry]
      filterDeadService deadService mvar = modifyMVar mvar $
        return . partition ((/= busName_ deadService) . serviceName)

      handleNameOwnerChanged _ name oldOwner newOwner =
        when (newOwner == "") $ do
          removedItems <- filterDeadService name notifierItems
          unless (null removedItems) $ do
            log $ printf "Unregistering item %s because it disappeared." name
            forM_ removedItems $ \item ->
              emitStatusNotifierItemUnregistered client $ renderServiceName item
          removedHosts <- filterDeadService name notifierHosts
          unless (null removedHosts) $
            log $ printf "Unregistering host %s because it disappeared." name
          return ()

      watcherMethods = map mkLogMethod
        [ autoMethodWithMsg "RegisterStatusNotifierItem"
          registerStatusNotifierItem
        , autoMethod "RegisterStatusNotifierHost"
          registerStatusNotifierHost
        , autoMethod "StopWatcher"
          stopWatcher
        , autoMethod "GetObjectPathForItemName"
          objectPathForItem
        ]

      watcherProperties =
        [ mkLogProperty "RegisteredStatusNotifierItems"
          registeredStatusNotifierItems
        , mkLogProperty "RegisteredSNIEntries"
          registeredSNIEntries
        , mkLogProperty "IsStatusNotifierHostRegistered"
          isStatusNotifierHostRegistered
        , mkLogProperty "ProtocolVersion"
          protocolVersion
        ]

      watcherInterface =
        Interface
        { interfaceName = watcherInterfaceName
        , interfaceMethods = watcherMethods
        , interfaceProperties = watcherProperties
        , interfaceSignals = watcherSignals
        }

      startWatcher = do
        nameRequestResult <- requestName client (coerce watcherInterfaceName) []
        case nameRequestResult of
          NamePrimaryOwner ->
            do
              _ <- DBusTH.registerForNameOwnerChanged client
                   matchAny handleNameOwnerChanged
              export client (fromString path) watcherInterface
          _ -> stopWatcher
        return nameRequestResult

  return (watcherInterface, startWatcher)

-- For Client generation
-- TODO: get rid of unsafePerformIO here by making function that takes mvars so
-- IO isn't needed to build watcher
{-# NOINLINE watcherInterface #-}
watcherInterface = buildIntrospectionInterface clientInterface
  where (clientInterface, _) =
          unsafePerformIO $ buildWatcher
          defaultWatcherParams { watcherDBusClient = Just undefined }
