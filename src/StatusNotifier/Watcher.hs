{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Watcher where

import Control.Monad
import Control.Concurrent.MVar
import DBus
import DBus.Client
import Data.Int
import Data.List
import Data.String
import StatusNotifier.Util
import Text.Printf

statusNotifierWatcherString :: String
statusNotifierWatcherString = "StatusNotifierWatcher"

watcherInterface :: InterfaceName
watcherInterface = fromString $ printf "%s.%s" protocolPrefix statusNotifierWatcherString

data ItemEntry = ItemEntry
  { serviceName :: String
  }

nameOwnerChangedMatchRule =
  matchAny
  { matchSender = Just "org.freedesktop.DBus"
  , matchMember = Just "NameOwnerChanged"
  }

startWatcherRequests = do
  client <- connectSession
  notifierItems <- newMVar []
  notifierHosts <- newMVar []

  let registerStatusNotifierItem name =
        modifyMVar_ notifierItems $ \currentItems ->
          do
            return $ ItemEntry { serviceName = name } : currentItems

      registerStatusNotifierHost name =
        modifyMVar_ notifierHosts $ \currentHosts ->
          do
            return $ ItemEntry { serviceName = name } : currentHosts

      registeredStatusNotifierItems = map serviceName <$> readMVar notifierItems

      isStatusNotifierHostRegistered = not . null <$> readMVar notifierHosts

      protocolVersion = return 1 :: IO Int32

      makeMethod :: AutoMethod fn => MemberName -> fn -> Method
      makeMethod = autoMethod watcherInterface

      filterDeadService deadService mvar =
        modifyMVar mvar $ return . partition ((/= deadService) . serviceName)

      handleNameOwnerChanged signal =
        case map fromVariant $ signalBody signal of
          (Just name):(Just oldOwner):(Just newOwner):_ ->
            when (newOwner == "") $
                 do
                   removedItems <- filterDeadService name notifierItems
                   removedHosts <- filterDeadService name notifierHosts
                   return ()
          _ -> return ()

  _ <- requestName client "org.freedesktop.StatusNotifierWatcher" []
  _ <- addMatch client nameOwnerChangedMatchRule handleNameOwnerChanged
  export client "/"
           [ makeMethod "RegisterStatusNotifierItem" registerStatusNotifierItem
           , makeMethod "RegisterStatusNotifierHost" registerStatusNotifierHost
           , makeMethod "RegisteredStatusNotifierItems" registeredStatusNotifierItems
           , makeMethod "IsStatusNotifierHostRegistered" isStatusNotifierHostRegistered
           , makeMethod "ProtocolVersion" protocolVersion
           ]
