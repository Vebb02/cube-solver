module CFOP.CFOP where

import Cube
import CubeState
import Control.Monad.State
import CFOP.PLL

solve :: Cube Algorithm
solve = do
    pllMoves <- pll
    aufMove <- auf
    return $ pllMoves ++ aufMove

auf :: Cube Algorithm
auf = do
    tryAufMoves aufMoves

tryAufMoves :: Algorithm -> Cube Algorithm
tryAufMoves (m:ms) = do
    move m
    cubeState <- get
    if cubeState == solvedCube
        then return [m]
        else do
            undoMove m
            tryAufMoves ms
tryAufMoves [] = do
    cubeState <- get
    if cubeState == solvedCube
    then return []
    else undefined
