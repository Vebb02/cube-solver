module BluetoothCube (bluetooth) where

import PDFCube (generatePDFFromSolution)
import System.IO
import System.Process
import Cube (move, Move, Algorithm, combineMoves, reverseMove)
import CubeParser (parseMove)
import qualified Data.Text as T
import Text.Megaparsec (runParser)
import CubeState (CubeState, solvedCube)
import Control.Monad.State (execState, when, evalState)
import Control.Concurrent (threadDelay)
import CFOP.CFOP (cfop)
import Data.List (minimumBy)
import Data.Time (getCurrentTime, UTCTime, nominalDiffTimeToSeconds, diffUTCTime)

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
    putStrLn "Move top layer until cube is connected"
    readLines 5 hout
    putStrLn "Cube is connected"
    threadDelay 500000
    putStrLn "Reset the top layer so that the cube is solved"
    threadDelay 500000
    countDown 3
    flushOutput hout
    putStrLn "Start scramblin!"
    systemTimeNow <- getCurrentTime
    cubeState <- scrambleCube systemTimeNow hout solvedCube
    putStrLn "Start solvin!"
    threadDelay 500000
    callCommand "open solution_manual.pdf"
    solveCube hout cubeState (evalState cfop cubeState)

readLines :: Int -> Handle -> IO ()
readLines count hout =
    if count <= 0
    then return ()
    else do
        _ <- hGetLine hout
        readLines (count - 1) hout

countDown :: Int -> IO ()
countDown count =
    if count <= 0
    then return ()
    else do
        print count
        threadDelay 1000000
        countDown (count - 1)

flushOutput :: Handle -> IO ()
flushOutput hout = do
    isMoreToRead <- hReady hout
    when isMoreToRead $ do
        _ <- hGetLine hout
        flushOutput hout

scrambleCube :: UTCTime -> Handle -> CubeState -> IO CubeState
scrambleCube startTime hout cubeState = do
    systemTimeNow <- getCurrentTime
    let secondsSinceLastMove = nominalDiffTimeToSeconds $ diffUTCTime systemTimeNow startTime
    if secondsSinceLastMove >= 3
        then return cubeState
        else do
            isReadyToRead <- hReady hout
            if isReadyToRead then do
                m <- parseNextMove hout
                let newCubeState = nextState m cubeState
                scrambleCube systemTimeNow hout newCubeState
            else scrambleCube startTime hout cubeState

solveCube :: Handle -> CubeState -> Algorithm -> IO ()
solveCube _ cubeState [] = generatePDFFromSolution cubeState []
solveCube hout cubeState solution@(x:xs) = do
    generatePDFFromSolution cubeState solution
    m <- parseNextMove hout
    let newCubeState = nextState m cubeState
    let currentSolution = combineMoves (reverseMove m) x ++ xs
    let possibleNewSolution = evalState cfop newCubeState
    let newSolution = minimumBy (\l1 l2 -> if length l1 <= length l2 then LT else GT) [currentSolution, possibleNewSolution]
    solveCube hout newCubeState newSolution

nextState :: Move -> CubeState -> CubeState
nextState m = execState (move m)

parseNextMove :: Handle -> IO Move
parseNextMove hout = do
    input <- hGetLine hout
    let eitherMove = runParser parseMove "" (T.pack input)
    case eitherMove of
        Right m -> return m
        Left errorMessage -> error $ show errorMessage
