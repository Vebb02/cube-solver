{-# LANGUAGE OverloadedStrings #-}
module Test where

import CubeParser ( parseCubeState, parseScramble )
import CubeState
import CubeValidator ( validateCubeState )
import CFOP.CFOP ( auf, solve )
import CFOP.PLL
import PDFCube
import Text.Megaparsec ( runParser, ParseErrorBundle )
import qualified Data.Text as T

import System.Directory ( listDirectory )
import Control.Monad.State
import Data.Void (Void)
import Cube


pdfTest :: IO ()
pdfTest = do
    inputText <- readFile "scramble.in"
    let parsedResult = runParser parseCubeState "" (T.pack inputText)
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            runPDFTest (evalState solve cubeState) cubeState
            -- runPDFTest [Move F Normal, Move F Prime, Move R Normal, Move R Prime, Move U Normal, Move U Prime, Move B Normal, Move B Prime, Move L Normal, Move L Prime, Move D Normal, Move D Prime] cubeState

parseScrambleTest :: IO ()
parseScrambleTest = do
    let dirPath = "input/scramble"
    inputFiles <- listDirectory dirPath
    readScrambles dirPath inputFiles

readScrambles :: String -> [FilePath] -> IO ()
readScrambles dirPath (file:files) = do
    inputText <- readFile (dirPath ++ "/" ++ file)
    let parsedResult = runParser parseScramble "" (T.pack inputText)
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right scramble -> do
            print file
            print scramble
            let (_, cubeState) = runState (applyAlgorithm scramble) solvedCube
            print cubeState
    readScrambles dirPath files
readScrambles _ [] = return ()

pllTest :: IO ()
pllTest = do
    let dirPath = "input/pll"
    inputFiles <- listDirectory dirPath
    readCubeFiles dirPath inputFiles solvePll

solvePll :: Either (ParseErrorBundle T.Text Void) CubeState -> FilePath -> IO()
solvePll parsedResult file = do
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            print file
            print cubeState
            print $ cornerSwapType cubeState
            let (result, newCubeState) = runState pll cubeState
            print result
            print newCubeState

aufTest :: IO ()
aufTest = do
    let dirPath = "input/auf"
    inputFiles <- listDirectory dirPath
    readCubeFiles dirPath inputFiles solveAuf

solveAuf :: Either (ParseErrorBundle T.Text Void) CubeState -> FilePath -> IO()
solveAuf parsedResult file = do
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            print file
            print cubeState
            let (result, newCubeState) = runState auf cubeState
            print result
            print newCubeState


validateTest :: IO ()
validateTest = do
    let dirPath = "input/validate"
    inputFiles <- listDirectory dirPath
    readCubeFiles dirPath inputFiles validateCubes

readCubeFiles :: String -> [FilePath] -> (Either (ParseErrorBundle T.Text Void) CubeState -> FilePath -> IO()) -> IO ()
readCubeFiles dirPath (file:files) process = do
    inputText <- readFile (dirPath ++ "/" ++ file)
    let parsedResult = runParser parseCubeState "" (T.pack inputText)
    process parsedResult file
    readCubeFiles dirPath files process
readCubeFiles _ [] _ = return ()

validateCubes :: Either (ParseErrorBundle T.Text Void) CubeState -> FilePath -> IO()
validateCubes parsedResult file = do
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            putStrLn $ file ++ ": " ++ show (validateCubeState cubeState)

scramblesTest :: IO ()
scramblesTest = do
    inputText <- readFile "input/scrambles.in"
    let unparsedScrambles = lines inputText
    print $ solveScrambles unparsedScrambles

solveScrambles :: [String] -> (Int, (Algorithm, Algorithm, Int), (Algorithm, Algorithm, Int)) 
solveScrambles [] = (0, ([], [], 1000000), ([], [], 0))
solveScrambles (x:xs) = do
    case runParser parseScramble "" (T.pack x) of
        Left errorMessage -> error $ show errorMessage
        Right scramble -> do
            let cubeState = execState (applyAlgorithm scramble) solvedCube
            let (solution, _) = runState solve cubeState
            -- print solution
            let (moveCount, (minAlg, minScramble, minMoves), (maxAlg, maxScramble, maxMoves)) = solveScrambles xs
            let totalMoveCount = moveCount + length solution
            let shortest = if length solution < minMoves then (solution, scramble, length solution) else (minAlg, minScramble, minMoves)
            let longest =  if length solution > maxMoves then (solution, scramble, length solution) else (maxAlg, maxScramble, maxMoves)
            (totalMoveCount, shortest, longest)
