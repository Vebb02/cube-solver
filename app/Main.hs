{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cube (applyAlgorithm)
import CubeParser ( parseCubeState )
import CubeState (CubeState)
import CubeValidator ( validateCubeState )
import CFOP.CFOP ( auf )
import CFOP.PLL

import Text.Megaparsec ( runParser, ParseErrorBundle )
import qualified Data.Text as T

import System.Directory (listDirectory)
import Control.Monad.State (runState)
import Data.Void (Void)

main :: IO ()
main = do
    -- validateTest
    -- aufTest
    pllTest


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
            let (result, newCubeState) = runState (applyAlgorithm jbPerm) cubeState
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
