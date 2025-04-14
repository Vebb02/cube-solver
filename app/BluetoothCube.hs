{-# LANGUAGE OverloadedStrings #-}
module BluetoothCube (bluetooth) where

import DBus
import DBus.Client
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Cont (when)
import Data.ByteString

bluetooth :: IO ()
bluetooth = do
    client <- connectSystem
    let path = "/org/bluez/hci0/dev_AB_12_34_00_DB_F7/service0016/char0019"
    _ <- call_ client (methodCall path "org.bluez.GattCharacteristic1" "StartNotify")
        { methodCallDestination = Just "org.bluez" }
    listenForNotifications client

listenForNotifications :: Client -> IO ()
listenForNotifications client = do
    _ <- addMatch client matchRule handler
    forever $ threadDelay 1000000
    where
        matchRule :: MatchRule
        matchRule = matchAny
            { matchInterface = Just "org.freedesktop.DBus.Properties"
            , matchMember = Just "PropertiesChanged"
            }
        handler :: Signal -> IO ()
        handler sig = case signalBody sig of
            [interface, val, _] -> do
                when (interface == toVariant ("org.bluez.GattCharacteristic1" :: String)) $ 
                    case readMoveFromVariant val of
                        Just value -> print value
                        Nothing -> putStrLn "Could not parse value"
            _ -> return ()

readMoveFromVariant :: Variant -> Maybe ByteString
readMoveFromVariant val = do
    dict <- fromVariant val :: Maybe Dictionary
    v1 <- lookup (toVariant ("Value" :: String)) (dictionaryItems dict)
    v2 <- fromVariant v1
    fromVariant v2 :: Maybe ByteString