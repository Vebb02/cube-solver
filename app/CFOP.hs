module CFOP where

import Cube
import CubeState
import Control.Monad.State

solve :: Cube [Move]
solve = do
    auf

auf :: Cube [Move]
auf = do
    tryAufMoves [Move U Normal, Move U Two, Move U Prime]

tryAufMoves :: [Move] -> Cube [Move]
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

undoMove :: Move -> Cube ()
undoMove (Move face Normal) = move (Move face Prime)
undoMove (Move face Prime) = move (Move face Normal)
undoMove (Move face Two) = move (Move face Two)


