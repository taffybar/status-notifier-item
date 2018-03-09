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
import           Data.String
import           StatusNotifier.Util
import           StatusNotifier.Watcher.Constants
import           StatusNotifier.Watcher.Signals
import           System.IO.Unsafe

nameOwnerChangedMatchRule :: MatchRule
nameOwnerChangedMatchRule =
  matchAny
  { matchSender = Just "org.freedesktop.DBus"
  , matchMember = Just "NameOwnerChanged"
  }

buildWatcher WatcherParams
               { watcherNamespace = interfaceNamespace
               , watcherLogger = log
               , watcherStop = stopWatcher
               , watcherPath = path
               , watcherDBusClient = mclient
               } = do
  let watcherInterfaceName = getWatcherInterfaceName interfaceNamespace

  client <- maybe connectSession return mclient

  notifierItems <- newMVar []
  notifierHosts <- newMVar []

  let nameIsRegistered name items =
        isJust $ find ((== name) . serviceName) items

      registerStatusNotifierItem name =
        modifyMVar_ notifierItems $ \currentItems ->
          if nameIsRegistered name currentItems
          then
            return currentItems
          else
            do
              emitStatusNotifierItemRegistered client name
              return $ ItemEntry { serviceName = name } : currentItems

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
        log "Registered items requested" >>
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

      watcherMethods =
        [ autoMethod "RegisterStatusNotifierItem" registerStatusNotifierItem
        , autoMethod "RegisterStatusNotifierHost" registerStatusNotifierHost
        , autoMethod "StopWatcher" stopWatcher
        ]

      watcherProperties =
        [ readOnlyProperty "RegisteredStatusNotifierItems" registeredStatusNotifierItems
        , readOnlyProperty "IsStatusNotifierHostRegistered" isStatusNotifierHostRegistered
        , readOnlyProperty "ProtocolVersion" protocolVersion
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
{-# NOINLINE watcherInterface #-}
watcherInterface = buildIntrospectionInterface clientInterface
  where (clientInterface, _) = unsafePerformIO $ buildWatcher
                               defaultWatcherParams { watcherDBusClient = Just Client {} }
