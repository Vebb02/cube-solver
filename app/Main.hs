{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cube
import Text.Megaparsec
import CubeParser
import qualified Data.Text as T


main :: IO ()
main = do
    inputText <- readFile "solved.in" 
    let parsedResult = runParser parseCubeState "" (T.pack inputText)
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            print cubeState
            putStrLn $ showCube tPerm cubeState