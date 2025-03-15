module CFOP.CFOP where

import Cube
import CFOP.PLL
import CFOP.OLL
import CFOP.F2L

solve :: Cube Algorithm
solve = do
    f2lMoves <- f2l
    ollMoves <- oll
    pllMoves <- pll
    aufMove <- auf
    return $ f2lMoves ++ ollMoves ++ pllMoves ++ aufMove

auf :: Cube Algorithm
auf = do
    tryAlg ([] : map (: []) aufMoves) cubeSolved
