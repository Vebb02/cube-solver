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
    cubeState <- scrambleCube 0 hout solvedCube
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

scrambleCube :: Int -> Handle -> CubeState -> IO CubeState
scrambleCube 30 _ cubeState = return cubeState
scrambleCube count hout cubeState = do
    m <- parseNextMove hout
    print m
    let newCubeState = nextState m cubeState
    scrambleCube (count + 1) hout newCubeState

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
