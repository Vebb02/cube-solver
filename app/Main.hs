{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cube
import CubeParser

main :: IO ()
main = do
    putStrLn $ showCube tPerm
    runTest "Y"
