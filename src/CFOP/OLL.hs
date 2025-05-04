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

flipEdges :: EdgeState -> Cube Algorithm
flipEdges EdgesOriented = return []
flipEdges Line = lineFlip
flipEdges Angle = angleFlip
flipEdges Dot = dotFlip

cornersOriented :: CubeState -> Bool
cornersOriented cubeState = totalPieceSum (pieces cubeState :: [Corner]) == 0

solveCorners :: Cube Algorithm
solveCorners = do
    cubeState <- get
    case tryAlg (prependAuf [[], sune, antisune, hOll, lOll, piOll, tOll, uOll]) cubeState cornersOriented of
        Left errorMessage -> error $ "Failed solving corners of OLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

dotFlip :: Cube Algorithm
dotFlip = do
    cubeState <- get
    case tryAlg dotFlipAlgs cubeState ollSolved of
        Left errorMessage -> error $ "Failed solving dot case of OLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

lineFlip :: Cube Algorithm
lineFlip = do
    cubeState <- get
    case tryAlg lineFlipAlgs cubeState ollSolved of
        Left errorMessage -> error $ "Failed solving line case of OLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

angleFlip :: Cube Algorithm
angleFlip = do
    cubeState <- get
    case tryAlg angleFlipAlgs cubeState ollSolved of
        Left errorMessage -> error $ "Failed solving angle case of OLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

-- Dot flip algs

dotFlipAlgs :: [Algorithm]
dotFlipAlgs = prependAuf 
    [ [R, U2, R2, F, R, F', U2, R', F, R, F']
    , [L, F, L', U2, R, U2, R', U2, L, F', L']
    , [F, U, R, U', R', F', U, F, R, U, R', U', F']
    , [F, U, R, U', R', F', U', F, R, U, R', U', F']
    , [R, U, R', U, R', F, R, F', U2, R', F, R, F']
    , [R, U2, R2, F, R, F', U2, R', L, F, R, F', L']
    , [L', R, B, R, B, R', B', R2, L, F, R, F']
    , [L, F, R', F', R2, L2, B, R, B', R', B', R', L]
    ]

-- Line flip algs

lineFlipAlgs :: [Algorithm]
lineFlipAlgs = prependAuf 
    [ [F, U, R, U', R2, F', R, U, R, U', R']
    , [R', F, R, U, R', F', R, F, U', F']
    , [L', B', L, R', U', R, U, L', B, L]
    , [L, F, L', R, U, R', U', L, F', L']
    , sexy ++ sledgeHammer
    , [R, U, R2, U', R', F, R, U, R, U', F']
    , [L, F', L', U', L, U, F, U', L']
    , [R', F, R, U, R', U', F', U, R]
    , F : sexy ++ [F']
    , [R', U', R', F, R, F', U, R]
    , [F, U, R, U', R', U, R, U', R', F']
    , [R, U, R', U, R, U', B, U', B', R']
    , [R', F, R, U, R, U', R2, F', R2, U', R', U, R, U, R']
    , [L, F, L', U, R, U', R', U, R, U', R', L, F', L']
    , [R, U, R', U', R', L, F, R, F', L']
    ]
-- Angle flip algs

angleFlipAlgs :: [Algorithm]
angleFlipAlgs = prependAuf 
    [ [R', F2, L, F, L', F, R]
    , [L, F2, R', F', R, F', L']
    , [L, F, R', F, R, F2, L']
    , [R', F', L, F', L', F2, R]
    , [R, U, R', U', R', F, R2, U, R', U', F']
    , [R, U, R', U, R', F, R, F', R, U2, R']
    , [L, F, R', F, R', D, R, D', R, F2, L']
    , [R2, L, F', R, F', R', F2, R, F', R, L']
    , [L, F, R', F', L', R, U, R, U', R']
    , [R, U, R', U', R, U', R', F', U', F, R, U, R']
    , [F, R', F, R2, U', R', U', R, U, R', F2]
    , [R', U', F, U, R, U', R', F', R]
    , [L, U, F', U', L', U, L, F, L']
    , [R, U2, R2, F, R, F', R, U2, R']
    , [L', U', L, U', L', U, L, U, L, F', L', F]
    , [F, R', F', R, U, R, U', R']
    , [R, U, R', U, R, U', R', U', R', F, R, F']
    , [R, U, R', U, R, U2, R', F, R, U, R', U', F']
    , [R', U', R, U', R', U2, R, F, R, U, R', U', F']
    , [F', U', L', U, L, F]
    , F : reverseSexy ++ [F']
    , [R', U', R', F, R, F', R', F, R, F', U, R]
    , [F, R, U, R', U', R, U, R', U', F']
    , [L, F', L2, B, L2, F, L2, B', L]
    , [L', B, L2, F', L2, B', L2, F, L']
    , [R', F2, L, F, L', F', L, F, L', F, R]
    , [L, F2, R', F', R, F, R', F', R, F', L']
    ]

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
