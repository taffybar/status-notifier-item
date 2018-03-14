{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Watcher.Service where

import           Control.Concurrent.MVar
import           Control.Monad
import           DBus
import           DBus.Client
import           DBus.Generation
import           DBus.Internal.Message
import           DBus.Internal.Types
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
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

nameOwnerChangedMatchRule :: MatchRule
nameOwnerChangedMatchRule =
  matchAny
  { matchSender = Just "org.freedesktop.DBus"
  , matchMember = Just "NameOwnerChanged"
  }

buildWatcher WatcherParams
               { watcherNamespace = interfaceNamespace
               , watcherLogger = logger
               , watcherStop = stopWatcher
               , watcherPath = path
               , watcherDBusClient = mclient
               } = do
  let watcherInterfaceName = getWatcherInterfaceName interfaceNamespace
      log = putStrLn -- logL logger INFO
      logError = putStrLn -- logL logger ERROR
      mkLogCb cb msg = log (show msg) >> cb msg
      mkLogMethod m = m { methodHandler = mkLogCb $ methodHandler m }
      mkLogProperty name fn =
        readOnlyProperty name $ log (coerce name ++ " Called") >> fn

  client <- maybe connectSession return mclient

  notifierItems <- newMVar []
  notifierHosts <- newMVar []

  let nameIsRegistered name items =
        isJust $ find ((== name) . serviceName) items

      registerStatusNotifierItem MethodCall { methodCallSender = sender } name =
        let logRejection =
              -- XXX: this should maybe be a warning
              logError $ printf "Item registration for service %s rejected." name
            maybeBusName = getFirst $ mconcat $
                           map First [parseBusName name
                                     -- TODO: Support ayatana style paths as
                                     -- argument here and parse bus name to get sender.
                                     -- , sender
                                     ]
            continue (T.BusName busName) =
              modifyMVar_ notifierItems $ \currentItems ->
                if nameIsRegistered busName currentItems
                then
                  logRejection >> return currentItems
                else
                  do
                    emitStatusNotifierItemRegistered client busName
                    return $ ItemEntry { serviceName = busName } : currentItems
        in
          maybe logRejection (void . continue) maybeBusName

      registerStatusNotifierHost name =
        modifyMVar_ notifierHosts $ \currentHosts ->
          if nameIsRegistered name currentHosts
          then
            return currentHosts
          else
            do
              emitStatusNotifierHostRegistered client
              return $ ItemEntry { serviceName = name } : currentHosts

      registeredStatusNotifierItems =
        map serviceName <$> readMVar notifierItems

      isStatusNotifierHostRegistered = not . null <$> readMVar notifierHosts

      protocolVersion = return 1 :: IO Int32

      filterDeadService deadService mvar =
        modifyMVar mvar $ return . partition ((/= deadService) . serviceName)

      handleNameOwnerChanged signal =
        case map fromVariant $ signalBody signal of
          [Just name, Just oldOwner, Just newOwner] ->
            when (newOwner == "") $
                 do
                   removedItems <- filterDeadService name notifierItems
                   unless (null removedItems) $
                        emitStatusNotifierItemUnregistered client name
                   removedHosts <- filterDeadService name notifierHosts
                   return ()
          _ -> return ()

      watcherMethods = map mkLogMethod
        [ autoMethodWithMsg "RegisterStatusNotifierItem" registerStatusNotifierItem
        , autoMethod "RegisterStatusNotifierHost" registerStatusNotifierHost
        , autoMethod "StopWatcher" stopWatcher
        ]

      watcherProperties =
        [ mkLogProperty "RegisteredStatusNotifierItems" registeredStatusNotifierItems
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
              _ <- addMatch client nameOwnerChangedMatchRule handleNameOwnerChanged
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
