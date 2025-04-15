module BluetoothCube (bluetooth) where

import PDFCube (generatePDFFromCubeState)
import System.IO
import System.Process
import Cube (move, Move)
import CubeParser (parseMove)
import qualified Data.Text as T
import Text.Megaparsec (runParser)
import CubeState (CubeState, solvedCube)
import Control.Monad.State (execState, when)
import Control.Concurrent (threadDelay)


bluetooth :: IO ()
bluetooth = do
    callCommand "rfkill unblock bluetooth"
    let process = shell "cd app/bluetooth && npm start"
    (_, maybeHout, _, ph) <- createProcess process { std_out = CreatePipe }
    case maybeHout of
        Just hout -> bluetoothInteraction hout
        Nothing -> error "Could not get input handle"
    terminateProcess ph
    callCommand "rfkill block bluetooth"
    callCommand "rfkill unblock bluetooth"
    return ()

bluetoothInteraction :: Handle -> IO ()
bluetoothInteraction hout = do
    readLines 5 hout
    putStrLn "Cube is connected"
    threadDelay 500000
    putStrLn "Reset the cube to the solved state"
    threadDelay 500000
    countDown 3
    flushOutput hout
    putStrLn "Start scramblin!"
    scrambleCube 0 hout solvedCube

readLines :: Int -> Handle -> IO ()
readLines count hout
    | count <= 0 = return ()
    | otherwise = do
        _ <- hGetLine hout
        readLines (count - 1) hout

countDown :: Int -> IO ()
countDown count
    | count <= 0 = putStrLn "0"
    | otherwise = do
        print count
        threadDelay 1000000
        countDown (count - 1)

flushOutput :: Handle -> IO ()
flushOutput hout = do
    isMoreToRead <- hIsEOF hout
    when isMoreToRead $ do
        _ <- hGetLine hout
        flushOutput hout

scrambleCube :: Int -> Handle -> CubeState -> IO ()
scrambleCube 100 _ _ = return ()
scrambleCube count hout cubeState = do
    m <- parseNextMove hout
    let newCubeState = nextState m cubeState
    generatePDFFromCubeState newCubeState
    scrambleCube (count + 1) hout newCubeState

nextState :: Move -> CubeState -> CubeState
nextState m = execState (move m)

parseNextMove :: Handle -> IO Move
parseNextMove hout = do
    input <- hGetLine hout
    let eitherMove = runParser parseMove "" (T.pack input)
    case eitherMove of
        Right m -> return m
        Left errorMessage -> error $ show errorMessage
