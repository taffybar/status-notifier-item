{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Watcher where

import Control.Monad
import Control.Concurrent.MVar
import DBus
import DBus.Client
import DBus.Internal.Message
import Data.Int
import Data.List
import Data.String
import StatusNotifier.Util
import Text.Printf

statusNotifierWatcherString :: String
statusNotifierWatcherString = "StatusNotifierWatcher"

watcherInterface :: InterfaceName
watcherInterface = fromString $ printf "%s.%s" protocolPrefix statusNotifierWatcherString

makeWatcherMethod :: AutoMethod fn => MemberName -> fn -> Method
makeWatcherMethod = autoMethod watcherInterface

watcherSignal =
  Signal
  { signalPath = "/"
  , signalInterface = watcherInterface
  , signalMember = ""
  , signalSender = Nothing
  , signalDestination = Nothing
  , signalBody = []
  }

nameOwnerChangedMatchRule =
  matchAny
  { matchSender = Just "org.freedesktop.DBus"
  , matchMember = Just "NameOwnerChanged"
  }

data ItemEntry = ItemEntry
  { serviceName :: String
  }

startWatcher = do
  client <- connectSession
  notifierItems <- newMVar []
  notifierHosts <- newMVar []

  let emitFromWatcher signal name =
        emit client
             watcherSignal { signalMember = signal
                           , signalBody = [toVariant name]
                           }

      registerStatusNotifierItem name =
        modifyMVar_ notifierItems $ \currentItems ->
          do
            emitFromWatcher "StatusNotifierItemRegistered" name
            return $ ItemEntry { serviceName = name } : currentItems

      registerStatusNotifierHost name =
        modifyMVar_ notifierHosts $ \currentHosts ->
          do
            emit client watcherSignal { signalMember = "StatusNotifierHostRegistered" }
            return $ ItemEntry { serviceName = name } : currentHosts

      registeredStatusNotifierItems = map serviceName <$> readMVar notifierItems

      isStatusNotifierHostRegistered = not . null <$> readMVar notifierHosts

      protocolVersion = return 1 :: IO Int32

      filterDeadService deadService mvar =
        modifyMVar mvar $ return . partition ((/= deadService) . serviceName)

      handleNameOwnerChanged signal =
        case map fromVariant $ signalBody signal of
          (Just name):(Just oldOwner):(Just newOwner):_ ->
            when (newOwner == "") $
                 do
                   removedItems <- filterDeadService name notifierItems
                   when (not $ null removedItems) $
                        emitFromWatcher "StatusNotifierItemUnregistered" name
                   removedHosts <- filterDeadService name notifierHosts
                   return ()
          _ -> return ()

  _ <- requestName client "org.freedesktop.StatusNotifierWatcher" []
  _ <- addMatch client nameOwnerChangedMatchRule handleNameOwnerChanged
  export client "/"
           [ makeWatcherMethod "RegisterStatusNotifierItem" registerStatusNotifierItem
           , makeWatcherMethod "RegisterStatusNotifierHost" registerStatusNotifierHost
           , makeWatcherMethod "RegisteredStatusNotifierItems" registeredStatusNotifierItems
           , makeWatcherMethod "IsStatusNotifierHostRegistered" isStatusNotifierHostRegistered
           , makeWatcherMethod "ProtocolVersion" protocolVersion
           ]
