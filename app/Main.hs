{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cube
import CubeParser
import CubeState
import Text.Megaparsec

import qualified Data.Text as T


main :: IO ()
main = do
    inputText <- readFile "solved.in" 
    let parsedResult = runParser parseCubeState "" (T.pack inputText)
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            print $ validateCubeState cubeState
            print cubeState
            putStrLn $ showCube tPerm cubeState