{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cube ()
import CubeParser ( parseCubeState )
import CubeState ()
import CubeValidator ( validateCubeState )
import Text.Megaparsec ( runParser )

import qualified Data.Text as T

import System.Directory (listDirectory)

main :: IO ()
main = do
    inputFiles <- listDirectory "input"
    readCubeFiles inputFiles

readCubeFiles :: [FilePath] -> IO ()
readCubeFiles (file:files) = do
    inputText <- readFile ("input" ++ "/" ++ file)
    let parsedResult = runParser parseCubeState "" (T.pack inputText)
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            putStrLn $ file ++ ": " ++ show (validateCubeState cubeState)
            -- print cubeState
            -- putStrLn $ showCube tPerm cubeState
    readCubeFiles files
readCubeFiles [] = return ()
