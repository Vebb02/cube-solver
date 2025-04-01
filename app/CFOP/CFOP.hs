module CFOP.CFOP where

import Cube
    ( Algorithm
    , Cube
    , removeCancellingMoves
    , tryAlg
    , aufMoves
    , cubeSolved 
    )
import CFOP.Cross ( cross )
import CFOP.PLL ( pll )
import CFOP.OLL ( oll )
import CFOP.F2L ( f2l )

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
