module CFOP.Cross (cross, crossSolved) where

import Cube
import CubeState
import Control.Monad.State
import Data.List (permutations)

cross :: Cube Algorithm
cross = do
    cubeState <- get
    if crossSolved cubeState
        then return []
        else applyAlgorithm (bestPossibleCross cubeState)

bestPossibleCross :: CubeState -> Algorithm
bestPossibleCross cubeState = bestPossibleSolution cubeState solveCross (permutations crossEdges)

solveCross :: [CubeState -> Edge] -> Cube Algorithm
solveCross = flip solveCross' []

solveCross' :: [CubeState -> Edge] -> [CubeState -> Edge] -> Cube Algorithm
solveCross' (x:xs) solvedPieces = do
    moves <- solveCrossPiece x solvedPieces
    rest <- solveCross' xs (x:solvedPieces)
    return $ moves ++ rest
solveCross' [] _ = do
    cubeState <- get
    case tryAlg [[], [Move DFace Normal], [Move DFace Prime], [Move DFace Two]] cubeState crossSolved of
        Left errorMessage -> error $ "Failed doing last move of cross: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

solveCrossPiece :: (CubeState -> Edge) -> [CubeState -> Edge] -> Cube Algorithm
solveCrossPiece x solvedPieces = do
    cubeState <- get
    let edgeCase = crossEdgeCase cubeState x solvedPieces
    let x1 = findPieceOnCube cubeState (x solvedCube)
    solveCrossPieceByCase edgeCase x1 solvedPieces

solveCrossPieceByCase :: CrossEdgeCase -> (CubeState -> Edge) -> [CubeState -> Edge] -> Cube Algorithm
solveCrossPieceByCase SolvedEdge _  _ = return []
solveCrossPieceByCase WrongPermutation x solvedPieces = do
    cubeState <- get
    let m = topBottomMoveFace x
    moveOutOfLayer <- applyAlgorithm [Move m Normal]
    moveBottomLayer <- moveTargetToDestination (targetEdge cubeState (x cubeState) solvedPieces) (moveFaceCrossEdge m)
    moveBackToLayer <- applyAlgorithm [Move m Prime]
    return $ moveOutOfLayer ++ moveBottomLayer ++ moveBackToLayer
solveCrossPieceByCase MidEdge x solvedPieces = do
    cubeState <- get
    let (Move m dir) = midMove cubeState x
    moveBottomLayer <- moveTargetToDestination (targetEdge cubeState (x cubeState) solvedPieces) (moveFaceCrossEdge m)
    moveToBottomLayer <- applyAlgorithm [Move m dir]
    return $ moveBottomLayer ++ moveToBottomLayer
solveCrossPieceByCase TopEdge x solvedPieces = do
    cubeState <- get
    let m = topBottomMoveFace x
    moveBottomLayer <- moveTargetToDestination (targetEdge cubeState (x cubeState) solvedPieces) (moveFaceCrossEdge m)
    moveToBottomLayer <- applyAlgorithm [Move m Two]
    return $ moveBottomLayer ++ moveToBottomLayer
solveCrossPieceByCase TopEdgeFlipped x solvedPieces =  do
    cubeState <- get
    let m = topBottomMoveFace x
    let m1 = leftMoveFace m
    moveBottomLayer <- moveTargetToDestination (targetEdge cubeState (x cubeState) solvedPieces) (moveFaceCrossEdge m1)
    insertPiece <- applyAlgorithm [Move m Prime, Move m1 Normal, Move m Normal]
    return $ moveBottomLayer ++ insertPiece
solveCrossPieceByCase BottomEdgeFlipped x solvedPieces = do
    cubeState <- get
    let m = topBottomMoveFace x
    let m1 = leftMoveFace m
    moveOutOfLayer <- applyAlgorithm [Move m Normal]
    moveBottomLayer <- moveTargetToDestination (targetEdge cubeState (x cubeState) solvedPieces) (moveFaceCrossEdge m1)
    moveBackToLayer <- applyAlgorithm [Move m1 Normal]
    return $ moveOutOfLayer ++ moveBottomLayer ++ moveBackToLayer

topBottomMoveFace :: (CubeState -> Edge) -> MoveFace
topBottomMoveFace x = case secondE $ x solvedCube of
    Green -> FFace
    Red -> RFace
    Blue -> BFace
    Orange -> LFace
    _ -> error "Color not defined"

midMove :: CubeState -> (CubeState -> Edge) -> Move
midMove cubeState x = let
    solvedEdge = x solvedCube
    unsolvedEdge = x cubeState
    edgeSum = pieceSum unsolvedEdge in
    case solvedEdge of
        Edge Green Red -> if edgeSum == 0 then Move RFace Prime else Move FFace Normal
        Edge Green Orange -> if edgeSum == 0 then Move LFace Normal else Move FFace Prime
        Edge Blue Red -> if edgeSum == 0 then Move RFace Normal else Move BFace Prime
        Edge Blue Orange -> if edgeSum == 0 then Move LFace Prime else Move BFace Normal
        _ -> error $ show solvedEdge


leftMoveFace :: MoveFace -> MoveFace
leftMoveFace FFace = LFace
leftMoveFace RFace = FFace
leftMoveFace BFace = RFace
leftMoveFace LFace = BFace
leftMoveFace _ = error "Moveface does not have a face to the left"

moveFaceCrossEdge :: MoveFace -> CrossEdge
moveFaceCrossEdge FFace = FEdge
moveFaceCrossEdge RFace = REdge
moveFaceCrossEdge BFace = BEdge
moveFaceCrossEdge LFace = LEdge
moveFaceCrossEdge _ = error "Moveface is not assosiated with a cross edge"


data CrossEdge = FEdge | REdge | BEdge | LEdge
    deriving (Eq, Show)

data CrossEdgeCase = SolvedEdge | WrongPermutation | MidEdge | TopEdge | TopEdgeFlipped | BottomEdgeFlipped
    deriving (Eq, Show)

crossEdgeCase :: CubeState -> (CubeState -> Edge) -> [CubeState -> Edge] -> CrossEdgeCase
crossEdgeCase cubeState getEdge solvedPieces = let edge = getEdge solvedCube in
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
                    else if edge `elem` map fst (filter (uncurry (==)) $ optimalCrossEdges cubeState solvedPieces)
                        then SolvedEdge
                        else WrongPermutation

optimalCrossEdges :: CubeState -> [CubeState -> Edge] -> [(Edge, Edge)]
optimalCrossEdges cubeState solvedPieces = do
    let currCrossEdges = map (\crossEdge -> crossEdge cubeState) crossEdges
    let solvedCrossEdges = map (\crossEdge -> crossEdge solvedCube) crossEdges
    let possibleSolvedCrossEdges = allRotations solvedCrossEdges
    fst $ compareCrossEdgesWithPossibleSolvedCrossEdges currCrossEdges possibleSolvedCrossEdges solvedPieces

compareCrossEdgesWithPossibleSolvedCrossEdges :: [Edge] -> [[Edge]] -> [CubeState -> Edge] ->  ([(Edge, Edge)], Int)
compareCrossEdgesWithPossibleSolvedCrossEdges _ [] _ = ([], -1)
compareCrossEdgesWithPossibleSolvedCrossEdges edges (x:xs) solvedPieces =
    let currentEdges = zip edges x
        equalEdges = map fst $ filter (uncurry (==)) currentEdges
        solvedEdges = map (\x1 -> x1 solvedCube) solvedPieces
        solvedPiecesIncluded = null solvedPieces || all (`elem` equalEdges) solvedEdges
        solvedCount = length equalEdges
        rest = compareCrossEdgesWithPossibleSolvedCrossEdges edges xs solvedPieces
    in
    if solvedCount > snd rest && solvedPiecesIncluded
        then (currentEdges, solvedCount) else rest

allRotations :: [Edge] -> [[Edge]]
allRotations edges = if null edges then [] else helperRotations (length edges-1) where
    helperRotations 0 = [edges]
    helperRotations n = (drop n edges ++ take n edges) : helperRotations (n-1)

crossEdgeIndex :: CrossEdge -> Int
crossEdgeIndex FEdge = 0
crossEdgeIndex REdge = 1
crossEdgeIndex BEdge = 2
crossEdgeIndex LEdge = 3

targetEdge :: CubeState -> Edge -> [CubeState -> Edge] -> Maybe CrossEdge
targetEdge cubeState edge solvedPieces =
    let currentCrossEdges = optimalCrossEdges cubeState solvedPieces
        flippedEdge = if pieceSum edge == 0 then edge else flipEdge edge
    in
        if not (any (uncurry (==)) currentCrossEdges)
        then Nothing
        else Just $ findEdge flippedEdge (map snd currentCrossEdges) (4 :: Int)
    where
        findEdge edge1 (x:xs) 4 = if edge1 == x then FEdge else findEdge edge1 xs 3
        findEdge edge1 (x:xs) 3 = if edge1 == x then REdge else findEdge edge1 xs 2
        findEdge edge1 (x:xs) 2 = if edge1 == x then BEdge else findEdge edge1 xs 1
        findEdge edge1 (x:xs) 1 = if edge1 == x then LEdge else findEdge edge1 xs 0
        findEdge _ _ _ = error "Edge is not in the possible target edges"

moveTargetToDestination :: Maybe CrossEdge -> CrossEdge -> Cube Algorithm
moveTargetToDestination Nothing _ = return []
moveTargetToDestination (Just target) destination = applyAlgorithm (dMoveBetween target destination)

dMoveBetween :: CrossEdge -> CrossEdge -> Algorithm
dMoveBetween from to = 
    case moveSumToMoveDirection (crossEdgeIndex to - crossEdgeIndex from) of
        Nothing  -> []
        Just dir  -> [Move DFace dir]

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
