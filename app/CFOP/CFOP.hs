module CFOP.CFOP where

import Cube
import CFOP.Cross
import CFOP.PLL
import CFOP.OLL
import CFOP.F2L

solve :: Cube Algorithm
solve = do
    crossMoves <- cross
    f2lMoves <- f2l
    ollMoves <- oll
    pllMoves <- pll
    aufMove <- auf
    return $ removeCancellingMoves (crossMoves ++ f2lMoves ++ ollMoves ++ pllMoves ++ aufMove) []

auf :: Cube Algorithm
auf = do
    tryAlg ([] : map (: []) aufMoves) cubeSolved
