module BluetoothCube (bluetooth) where

import PDFCube (generatePDFFromCubeState)
import System.IO
import System.Process
import Cube (move, Move)
import CubeParser (parseMove)
import qualified Data.Text as T
import Text.Megaparsec (runParser)
import CubeState (CubeState, solvedCube)
import Control.Monad.State (execState)


bluetooth :: IO ()
bluetooth = do
    callCommand "rfkill unblock bluetooth"
    let process = shell "cd app/bluetooth && npm start"
    (_, maybeHout, _, ph) <- createProcess process { std_out = CreatePipe }
    case maybeHout of
        Just hout -> readLines 0 hout
        Nothing -> error "Could not get input handle"
    terminateProcess ph
    callCommand "rfkill block bluetooth"
    callCommand "rfkill unblock bluetooth"
    return ()


readLines :: Int -> Handle -> IO ()
readLines 25 hout = scrambleCube 0 hout solvedCube
readLines count hout = do
    input <- hGetLine hout
    putStrLn input
    readLines (count + 1) hout

scrambleCube :: Int -> Handle -> CubeState -> IO ()
scrambleCube 100 _ _ = return ()
scrambleCube count hout cubeState = do
    m <- getNextMove hout
    let newCubeState = nextState m cubeState
    generatePDFFromCubeState newCubeState
    scrambleCube (count + 1) hout newCubeState

nextState :: Move -> CubeState -> CubeState
nextState m = execState (move m)

getNextMove :: Handle -> IO Move
getNextMove hout = do    
    input <- hGetLine hout
    let eitherMove = runParser parseMove "" (T.pack input)
    case eitherMove of
        Right m -> return m
        Left errorMessage -> error $ show errorMessage
