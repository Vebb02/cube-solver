module CFOP.Cross where

import Cube
import CubeState
import Control.Monad.State
import CubeValidator
import Data.Maybe
import Data.List ( permutations )

cross :: Cube Algorithm
cross = do
    cubeState <- get
    if crossSolved cubeState
        then return []
        else solveCross crossEdges

solveCross :: [CubeState -> Edge] -> Cube Algorithm
solveCross (x:xs) = do
    moves <- solveCrossPiece x
    rest <- solveCross xs
    return $ moves ++ rest
solveCross [] = return []

solveCrossPiece :: (CubeState -> Edge) -> Cube Algorithm
solveCrossPiece x = tryAlg [] (`crossPieceSolved` x)

data CrossEdge = FEdge | REdge | BEdge | LEdge
    deriving (Eq, Show)

data CrossEdgeCase = SolvedEdge | WrongPermutation | MidEdge | TopEdge | TopEdgeFlipped | BottomEdgeFlipped
    deriving (Eq, Show)

crossEdgeCase :: CubeState -> (CubeState -> Edge) -> CrossEdgeCase
crossEdgeCase cubeState getEdge = let edge = getEdge solvedCube in
    if any (\getTopEdge -> edge == getTopEdge cubeState) topLayerEdges
        then TopEdge
        else if any (\getTopEdge -> edge == flipEdge (getTopEdge cubeState)) topLayerEdges
            then TopEdgeFlipped
            else if any 
                    (\getMidEdge -> edge == getMidEdge cubeState ||
                                    edge == flipEdge (getMidEdge cubeState)) 
                    midLayerEdges
                then MidEdge
                else if any (\getCrossEdge -> edge == flipEdge (getCrossEdge cubeState)) crossEdges
                    then BottomEdgeFlipped
                    else if any (\getCrossEdge -> edge == getCrossEdge cubeState) (optimalCrossEdges cubeState)
                        then SolvedEdge
                        else WrongPermutation

optimalCrossEdges :: CubeState -> [CubeState -> Edge]
optimalCrossEdges = undefined

crossEdgeIndex :: CrossEdge -> Int
crossEdgeIndex FEdge = 0
crossEdgeIndex REdge = 1
crossEdgeIndex BEdge = 2
crossEdgeIndex LEdge = 3

-- CrossEdge where edge is supposed to go
targetEdge :: Edge -> Maybe CrossEdge
targetEdge = undefined

moveTargetToDestination :: Maybe CrossEdge -> CrossEdge -> Cube Algorithm
moveTargetToDestination Nothing _ = return []
moveTargetToDestination (Just target) destination = applyAlgorithm (dMoveBetween target destination)

dMoveBetween :: CrossEdge -> CrossEdge -> Algorithm
dMoveBetween from to =
    let moveCount = (crossEdgeIndex to - crossEdgeIndex from)
        absMove = if moveCount < 0 then moveCount + 4 else moveCount
    in
    case absMove of
        0  -> []
        1  -> [Move D Normal]
        2  -> [Move D Two]
        3  -> [Move D Prime]
        _ -> error "Not valid move"

crossEdges :: [CubeState -> Edge]
crossEdges = [df, dr, db, dl]

topLayerEdges :: [CubeState -> Edge]
topLayerEdges = [uf, ur, ub, ul]

midLayerEdges :: [CubeState -> Edge]
midLayerEdges = [fr, fl, br, bl]

crossSolved :: CubeState -> Bool
crossSolved cubeState = all (crossPieceSolved cubeState) crossEdges

crossPieceSolved :: CubeState -> (CubeState -> Edge) -> Bool
crossPieceSolved cubeState edge = edge cubeState == edge solvedCube
