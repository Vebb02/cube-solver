{-# LANGUAGE OverloadedStrings #-}
module BluetoothCube (bluetooth, decryptionTest) where

import DBus
import DBus.Client
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Cont (when)
import qualified Data.ByteString as BS
import Crypto.Cipher.AES
import Crypto.Error (CryptoFailable (CryptoPassed, CryptoFailed))
import Crypto.Cipher.Types
import Data.Word (Word8)

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

readMoveFromVariant :: Variant -> Maybe [Word8]
readMoveFromVariant val = do
    dict <- fromVariant val :: Maybe Dictionary
    v1 <- lookup (toVariant ("Value" :: String)) (dictionaryItems dict)
    v2 <- fromVariant v1
    value <- fromVariant v2 :: Maybe BS.ByteString
    return $ BS.unpack value

salt :: [Word8]
salt = reverse [0xAB, 0x12, 0x34, 0x00, 0xDB, 0xF7] ++ replicate 10 0x00

failableKey :: CryptoFailable AES128
failableKey = cipherInit $ BS.pack $ zipWith (\keyByte saltByte -> (keyByte + saltByte) `mod` 0xFF) 
    [0x01, 0x02, 0x42, 0x28, 0x31, 0x91, 0x16, 0x07, 0x20, 0x05, 0x18, 0x54, 0x42, 0x11, 0x12, 0x53]
    salt

key :: AES128
key = case failableKey of
    CryptoPassed a -> a
    CryptoFailed e -> error $ show e

maybeIV :: Maybe (IV AES128)
maybeIV = makeIV $ BS.pack $ zipWith (\ivByte saltByte -> (ivByte + saltByte) `mod` 0xFF) 
    [0x11, 0x03, 0x32, 0x28, 0x21, 0x01, 0x76, 0x27, 0x20, 0x95, 0x78, 0x14, 0x32, 0x12, 0x02, 0x43] 
    salt

iv :: IV AES128
iv = case maybeIV of
    Just res -> res
    Nothing -> error "Could not create IV"


decryptionTest :: IO ()
decryptionTest = mapM_ (print . BS.unpack . decrypt . BS.pack) moves

decryptChunk :: BS.ByteString -> BS.ByteString
decryptChunk = cbcDecrypt key iv

decrypt :: BS.ByteString -> BS.ByteString
decrypt move = 
    let lastChunk = decryptChunk (BS.drop 4 move)
        firstChunk = decryptChunk (BS.take 4 move <> BS.take 12 lastChunk)
    in
        firstChunk <> BS.drop 12 lastChunk


moves :: [[Word8]]
moves = 
    [ [247,32,161,13,98,69,233,18,99,251,38,95,41,24,97,103,173,58,23,233]
    , [170,156,157,197,22,192,171,124,130,148,110,51,126,232,227,46,136,162,57,108]
    , [18,98,209,102,202,7,99,75,187,89,248,237,33,103,41,140,100,229,83,213]
    , [169,43,211,98,230,99,230,144,61,167,220,143,210,90,235,140,55,65,46,78]
    , [183,103,21,182,250,101,101,113,159,89,9,105,98,81,144,154,87,68,56,204]
    , [229,14,97,112,172,78,163,132,240,203,200,145,138,35,34,242,42,17,3,174]
    , [226,90,9,124,170,36,12,145,27,71,36,195,186,122,146,157,6,99,168,22]
    , [203,127,163,100,85,3,138,9,61,146,94,144,158,4,29,202,132,101,114,50]
    ]

