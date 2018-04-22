{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Watcher.Service where

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans
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
import           StatusNotifier.Util
import           StatusNotifier.Watcher.Constants
import           StatusNotifier.Watcher.Signals
import           System.IO.Unsafe
import           System.Log.Logger
import           Text.Printf

buildWatcher WatcherParams
               { watcherNamespace = interfaceNamespace
               , watcherLogger = logger
               , watcherStop = stopWatcher
               , watcherPath = path
               , watcherDBusClient = mclient
               } = do
  let watcherInterfaceName = getWatcherInterfaceName interfaceNamespace
      log = logL logger INFO
      logError = logL logger ERROR
      mkLogCb cb msg = lift (log (show msg)) >> cb msg
      mkLogMethod method = method { methodHandler = mkLogCb $ methodHandler method }
      mkLogProperty name fn =
        readOnlyProperty name $ log (coerce name ++ " Called") >> fn

  client <- maybe connectSession return mclient

  notifierItems <- newMVar []
  notifierHosts <- newMVar []

  let itemIsRegistered item items =
        isJust $ find (== item) items

      registerStatusNotifierItem MethodCall { methodCallSender = sender } name = runExceptT $ do
        let maybeBusName = getFirst $ mconcat $
                           map First [T.parseBusName name, sender]
            parseServiceError = makeErrorReply errorInvalidParameters $
              printf "the provided service %s could not be parsed \
                     \as a bus name or an object path." name
            path = fromMaybe "/StatusNotifierItem"  $ T.parseObjectPath name
            remapErrorName =
              left $ (`makeErrorReply` "Failed to verify ownership.") .
                   M.methodErrorName
        busName <- ExceptT $ return $ maybeToEither parseServiceError maybeBusName
        let item = ItemEntry { serviceName = busName
                             , servicePath = path
                             }
        hasOwner <- ExceptT $ remapErrorName <$> DBusTH.nameHasOwner client (coerce busName)
        lift $ modifyMVar_ notifierItems $ \currentItems ->
          if itemIsRegistered item currentItems
          then
            return currentItems
          else
            do
              emitStatusNotifierItemRegistered client $ coerce busName
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
        map (coerce . serviceName) <$> readMVar notifierItems

      registeredSNIEntries :: IO [(String, String)]
      registeredSNIEntries =
        map getTuple <$> readMVar notifierItems
          where getTuple (ItemEntry bname path) = (coerce bname, coerce path)

      objectPathForItem :: String -> IO (Either Reply String)
      objectPathForItem name =
        maybeToEither notFoundError .  fmap (coerce . servicePath) .
                      find ((== busName_ name) . serviceName) <$>
                      readMVar notifierItems
        where notFoundError =
                makeErrorReply errorInvalidParameters $
                printf "Service %s is not registered." name

      isStatusNotifierHostRegistered = not . null <$> readMVar notifierHosts

      protocolVersion = return 1 :: IO Int32

      filterDeadService :: String -> MVar [ItemEntry] -> IO [ItemEntry]
      filterDeadService deadService mvar = modifyMVar mvar $
        return . partition ((/= busName_ deadService) . serviceName)

      handleNameOwnerChanged _ name oldOwner newOwner =
        when (newOwner == "") $ do
          removedItems <- filterDeadService name notifierItems
          unless (null removedItems) $ do
            log $ printf "Unregistering item %s because it disappeared." name
            emitStatusNotifierItemUnregistered client name
          removedHosts <- filterDeadService name notifierHosts
          unless (null removedHosts) $
            log $ printf "Unregistering host %s because it disappeared." name
          return ()

      watcherMethods = map mkLogMethod
        [ autoMethodWithMsg "RegisterStatusNotifierItem" registerStatusNotifierItem
        , autoMethod "RegisterStatusNotifierHost" registerStatusNotifierHost
        , autoMethod "StopWatcher" stopWatcher
        , autoMethod "GetObjectPathForItemName" objectPathForItem
        ]

      watcherProperties =
        [ mkLogProperty "RegisteredStatusNotifierItems" registeredStatusNotifierItems
        , mkLogProperty "RegisteredSNIEntries" registeredSNIEntries
        , mkLogProperty "IsStatusNotifierHostRegistered" isStatusNotifierHostRegistered
        , mkLogProperty "ProtocolVersion" protocolVersion
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
              _ <- DBusTH.registerForNameOwnerChanged client matchAny handleNameOwnerChanged
              export client (fromString path) watcherInterface
          _ -> stopWatcher
        return nameRequestResult

  return (watcherInterface, startWatcher)

-- For Client generation
-- TODO: get rid of unsafePerformIO here by making function that takes mvars so
-- IO isn't needed to build watcher
{-# NOINLINE watcherInterface #-}
watcherInterface = buildIntrospectionInterface clientInterface
  where (clientInterface, _) = unsafePerformIO $ buildWatcher
                               defaultWatcherParams { watcherDBusClient = Just undefined }
