{-# LANGUAGE OverloadedStrings #-}

module WatcherSpec (spec) where

import Control.Exception (finally)
import Data.Either (isLeft)
import Data.List (isPrefixOf)
import DBus.Client
import qualified StatusNotifier.Watcher.Client as WatcherClient
import Test.Hspec

import TestSupport

spec :: Spec
spec = around withIsolatedSessionBus $ do
  describe "StatusNotifier.Watcher.Service integration" $ do
    it "starts with no registered items" $ \() -> do
      watcher <- startWatcher
      entries <- WatcherClient.getRegisteredSNIEntries watcher
      entries `shouldBe` Right []

    it "registers an item and exposes it via RegisteredSNIEntries" $ \() -> do
      watcher <- startWatcher
      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.ItemA" defaultPath "network-wireless"
      (do
        result <- WatcherClient.getRegisteredSNIEntries watcher
        result `shouldSatisfy` \v ->
          case v of
            Right entries -> any ((== defaultPath) . snd) entries
            Left _ -> False
        ) `finally` cleanup

    it "is idempotent when the same item registers twice" $ \() -> do
      watcher <- startWatcher
      itemClient <- connectSession
      _ <- requestName itemClient "org.test.ItemB" []
      WatcherClient.registerStatusNotifierItem itemClient "org.test.ItemB"
        `shouldReturn` Right ()
      WatcherClient.registerStatusNotifierItem itemClient "org.test.ItemB"
        `shouldReturn` Right ()
      Right entries <- WatcherClient.getRegisteredSNIEntries watcher
      let matches = filter (== ("org.test.ItemB", "/StatusNotifierItem")) entries
      length matches `shouldBe` 1

    it "returns the object path for a registered name with no explicit path" $ \() -> do
      watcher <- startWatcher
      itemClient <- connectSession
      _ <- requestName itemClient "org.test.ItemC" []
      WatcherClient.registerStatusNotifierItem itemClient "org.test.ItemC"
        `shouldReturn` Right ()
      WatcherClient.getObjectPathForItemName watcher "org.test.ItemC"
        `shouldReturn` Right "/StatusNotifierItem"

    it "accepts path-only registration and stores the sender unique name" $ \() -> do
      watcher <- startWatcher
      itemClient <- connectSession
      WatcherClient.registerStatusNotifierItem itemClient "/CustomPath"
        `shouldReturn` Right ()
      Right entries <- WatcherClient.getRegisteredSNIEntries watcher
      let customEntries = filter ((== "/CustomPath") . snd) entries
      length customEntries `shouldBe` 1
      case customEntries of
        (serviceName, _) : _ -> serviceName `shouldSatisfy` (":" `isPrefixOf`)
        _ -> expectationFailure "Expected exactly one custom entry"

    it "returns direct paths from service/path lookups without registration" $ \() -> do
      watcher <- startWatcher
      WatcherClient.getObjectPathForItemName watcher "org.test.Missing/CustomPath"
        `shouldReturn` Right "/CustomPath"

    it "returns an error for unknown services without explicit paths" $ \() -> do
      watcher <- startWatcher
      result <- WatcherClient.getObjectPathForItemName watcher "org.test.Missing"
      result `shouldSatisfy` isLeft

    it "rejects registration for unowned well-known names" $ \() -> do
      watcher <- startWatcher
      itemClient <- connectSession
      result <- WatcherClient.registerStatusNotifierItem itemClient "org.test.Unowned"
      result `shouldSatisfy` isLeft
      Right entries <- WatcherClient.getRegisteredSNIEntries watcher
      entries `shouldNotContain` [("org.test.Unowned", "/StatusNotifierItem")]

    it "deduplicates a client that registers by path and then by service name" $ \() -> do
      watcher <- startWatcher
      itemClient <- connectSession
      WatcherClient.registerStatusNotifierItem itemClient "/StatusNotifierItem"
        `shouldReturn` Right ()
      _ <- requestName itemClient "org.test.PathThenName" []
      WatcherClient.registerStatusNotifierItem itemClient "org.test.PathThenName"
        `shouldReturn` Right ()
      Right entries <- WatcherClient.getRegisteredSNIEntries watcher
      let matching = filter ((== "/StatusNotifierItem") . snd) entries
      length matching `shouldBe` 1

    it "unregisters items when their bus name disappears" $ \() -> do
      watcher <- startWatcher
      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.ItemD" defaultPath "utilities-terminal"
      Right before <- WatcherClient.getRegisteredSNIEntries watcher
      before `shouldContain` [("org.test.ItemD", "/StatusNotifierItem")]
      cleanup
      removed <-
        waitFor 1500000 $ do
          result <- WatcherClient.getRegisteredSNIEntries watcher
          pure $ case result of
            Right entries -> ("org.test.ItemD", "/StatusNotifierItem") `notElem` entries
            Left _ -> False
      removed `shouldBe` True

    it "only unregisters the disconnected item when multiple are present" $ \() -> do
      watcher <- startWatcher
      itemClientA <- connectSession
      itemClientB <- connectSession
      cleanupA <- registerSimpleItem itemClientA "org.test.ItemE" defaultPath "media-playback-start"
      cleanupB <- registerSimpleItem itemClientB "org.test.ItemF" defaultPath "media-playback-stop"
      (do
          Right before <- WatcherClient.getRegisteredSNIEntries watcher
          before `shouldContain` [("org.test.ItemE", "/StatusNotifierItem")]
          before `shouldContain` [("org.test.ItemF", "/StatusNotifierItem")]
          cleanupA
          stable <-
            waitFor 1500000 $ do
              result <- WatcherClient.getRegisteredSNIEntries watcher
              pure $ case result of
                Right entries ->
                  ("org.test.ItemE", "/StatusNotifierItem") `notElem` entries
                    && ("org.test.ItemF", "/StatusNotifierItem") `elem` entries
                Left _ -> False
          stable `shouldBe` True
        ) `finally` cleanupB

defaultPath :: String
defaultPath = "/StatusNotifierItem"
