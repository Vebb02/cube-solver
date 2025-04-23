module BluetoothCube (bluetooth) where

import PDFCube (generatePDFSolution)
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
    printAllWithDelay
        [ "Make sure the cube is solved!"
        , "Hold the cube so that the green side is facing towards you and the white side is on the top"
        , "Continously move the top layer until cube is connected"
        ] 8
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
    printAllWithDelay 
        [ "Cube is connected"
        , "Reset the top layer so that the cube is solved"
        ] 1
    printAllWithDelay (map show ([10,9..1] :: [Integer])) 1
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

printAllWithDelay :: [String] -> Int -> IO ()
printAllWithDelay [] _ = return ()
printAllWithDelay [x] _ = putStrLn x
printAllWithDelay (x:xs) secondsDelay = do
    putStrLn x
    threadDelay $ 1000000 * secondsDelay
    printAllWithDelay xs secondsDelay

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
    if secondsSinceLastMove >= 5
        then return cubeState
        else do
            isReadyToRead <- hReady hout
            if isReadyToRead then do
                m <- parseNextMove hout
                let newCubeState = nextState m cubeState
                scrambleCube systemTimeNow hout newCubeState
            else scrambleCube startTime hout cubeState

solveCube :: Handle -> CubeState -> Algorithm -> IO ()
solveCube _ cubeState [] = generatePDFSolution [] cubeState
solveCube hout cubeState solution@(x:xs) = do
    generatePDFSolution solution cubeState
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
