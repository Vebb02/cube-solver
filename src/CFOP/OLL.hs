module CFOP.OLL (oll, ollSolved) where

import Cube
import Triggers
import CubeState
import Control.Monad.State
import CFOP.Cross (crossSolved)
import CFOP.F2L (f2lSolved)

oll :: Cube Algorithm
oll = do
    cubeState <- get
    if crossSolved cubeState && f2lSolved cubeState 
        then do
            edgeFlipMoves <- flipEdges (edgeState cubeState)
            cornerSolveMoves <- solveCorners
            return $ edgeFlipMoves ++ cornerSolveMoves
        else error "Cross and F2L must be solved before OLL"

data EdgeState = Dot | Line | Angle | EdgesOriented
    deriving (Eq, Show)

edgeState :: CubeState -> EdgeState
edgeState cubeState = case totalPieceSum (pieces cubeState :: [Edge]) of
    0 -> EdgesOriented
    2 -> if firstE (uf cubeState) == firstE (ub cubeState) 
         || firstE (ul cubeState) == firstE (ur cubeState) 
        then Line else Angle 
    4 -> Dot
    _ -> error "Number of edges oriented is not valid for any OLL case"

ollSolved :: CubeState -> Bool
ollSolved cubeState = edgesOriented cubeState && cornersOriented cubeState

edgesOriented :: CubeState -> Bool
edgesOriented cubeState = edgeState cubeState == EdgesOriented

tryEdgeFlipAlg :: [Algorithm] -> Cube Algorithm
tryEdgeFlipAlg algs = do
    cubeState <- get
    case tryAlg algs cubeState edgesOriented of
        Left errorMessage -> error $ "Failed doing edge flip in OLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

flipEdges :: EdgeState -> Cube Algorithm
flipEdges EdgesOriented = return []
flipEdges Line = tryEdgeFlipAlg (prependMoves [U] [lineFlip])
flipEdges Angle = tryEdgeFlipAlg (prependAuf [angleFlip])
flipEdges Dot = applyAlgorithm dotFlip

cornersOriented :: CubeState -> Bool
cornersOriented cubeState = totalPieceSum (pieces cubeState :: [Corner]) == 0

solveCorners :: Cube Algorithm
solveCorners = do
    cubeState <- get
    case tryAlg (prependAuf [[], sune, antisune, hOll, lOll, piOll, tOll, uOll]) cubeState cornersOriented of
        Left errorMessage -> error $ "Failed solving corners of OLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

dotFlip :: Algorithm
dotFlip = angleFlip ++ [U] ++ lineFlip

lineFlip :: Algorithm
lineFlip = [F] ++ sexy ++ [F']

angleFlip :: Algorithm
angleFlip = reverseMoveSeq lineFlip

-- Edges oriented algs

sune :: Algorithm
sune = [R, U, R', U, R, U2, R']

antisune :: Algorithm
antisune = reverseMoveSeq sune

hOll :: Algorithm
hOll = [R, U, R', U, R, U', R', U, R, U2, R']

lOll :: Algorithm
lOll = [F, R', F', L, F, R, F', L']

piOll :: Algorithm
piOll = [R, U2, R2, U', R2, U', R2, U2, R]

tOll :: Algorithm
tOll = reverseMoveSeq lOll

uOll :: Algorithm
uOll = [R2, D, R', U2, R, D', R', U2, R']
