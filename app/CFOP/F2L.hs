module CFOP.F2L where

import Cube
import CubeState
import Control.Monad.State

f2l :: Cube Algorithm
f2l = do
    cubeState <- get
    if f2lSolved cubeState
        then return []
        else solveF2l allPairs

type F2LPair = (Edge, Corner)

data F2LSlot = FL | FR | BL | BR
    deriving (Eq, Show)

allPairs :: [F2LSlot]
allPairs = [FL, FR, BL, BR]

f2lSlot :: CubeState -> F2LSlot -> F2LPair
f2lSlot cubeState FL = (fl cubeState, dlf cubeState)
f2lSlot cubeState FR = (fr cubeState, dfr cubeState)
f2lSlot cubeState BL = (bl cubeState, dbl cubeState)
f2lSlot cubeState BR = (br cubeState, drb cubeState)

f2lSolved :: CubeState -> Bool
f2lSolved cubeState = all (\slot -> f2lSlot cubeState slot == f2lSlot solvedCube slot) allPairs

solveF2l :: [F2LSlot] -> Cube Algorithm
solveF2l (x:xs) = do
    pair <- solveF2lPair x
    rest <- solveF2l xs
    return $ pair ++ rest
solveF2l [] = return []

solveF2lPair :: F2LSlot -> Cube Algorithm
solveF2lPair slot = do
    orientMoves <- orientEdgeAndCorner slot
    pairMoves <-  putPairInSlot slot
    return $ orientMoves ++ pairMoves

orientEdgeAndCorner :: F2LSlot -> Cube Algorithm
orientEdgeAndCorner = undefined

putPairInSlot :: F2LSlot -> Cube Algorithm
putPairInSlot = undefined
