module CFOP.CFOP where

import Cube
import CFOP.PLL
import CFOP.OLL

solve :: Cube Algorithm
solve = do
    ollMoves <- oll
    pllMoves <- pll
    aufMove <- auf
    return $ ollMoves ++ pllMoves ++ aufMove

auf :: Cube Algorithm
auf = do
    tryAlg ([] : map (: []) aufMoves) cubeSolved
