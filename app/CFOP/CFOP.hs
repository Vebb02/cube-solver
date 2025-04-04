module CFOP.CFOP where

import Cube
    ( Algorithm
    , Cube
    , removeCancellingMoves
    , tryAlg
    , aufMoves
    )
import CubeState (cubeSolved)
import CFOP.Cross (cross)
import CFOP.PLL (pll)
import CFOP.OLL (oll)
import CFOP.F2L (f2l)
import Data.List (singleton)

cfop :: Cube Algorithm
cfop = do
    crossMoves <- cross
    f2lMoves <- f2l
    ollMoves <- oll
    pllMoves <- pll
    aufMove <- auf
    let solution = crossMoves ++ f2lMoves ++ ollMoves ++ pllMoves ++ aufMove
    let shortenedSolution = removeCancellingMoves solution []
    return shortenedSolution

auf :: Cube Algorithm
auf = do
    tryAlg ([] : map singleton aufMoves) cubeSolved
