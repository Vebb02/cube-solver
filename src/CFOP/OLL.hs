{-# LANGUAGE QuasiQuotes #-}
module CFOP.OLL (oll, ollSolved) where

import Cube
import Triggers
import CubeState
import Control.Monad.State
import CFOP.Cross (crossSolved)
import CFOP.F2L (f2lSolved)
import AlgExpr

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
    case tryAlg edgesOrientedAlgs cubeState cornersOriented of
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
dotFlipAlgs = applyFourSidesAlg
    [ [algExpr|R U2 R2 F R F' U2 R' F R F'|]
    , [algExpr|L F L' U2 R U2 R' U2 L F' L'|]
    , [algExpr|F U R U' R' F' U F R U R' U' F'|]
    , [algExpr|F U R U' R' F' U' F R U R' U' F'|]
    , [algExpr|R U R' U R' F R F' U2 R' F R F'|]
    , [algExpr|R U2 R2 F R F' U2 R' L F R F' L'|]
    , [algExpr|L' R B R B R' B' R2 L F R F'|]
    , [algExpr|L F R' F' R2 L2 B R B' R' B' R' L|]
    ]

-- Line flip algs

lineFlipAlgs :: [Algorithm]
lineFlipAlgs = applyFourSidesAlg
    [ [algExpr|F U R U' R2 F' R U R U' R'|]
    , [algExpr|R' F R U R' F' R F U' F'|]
    , [algExpr|L' B' L R' U' R U L' B L|]
    , [algExpr|L F L' R U R' U' L F' L'|]
    , sexy ++ sledgeHammer
    , [algExpr|R U R2 U' R' F R U R U' F'|]
    , [algExpr|L F' L' U' L U F U' L'|]
    , [algExpr|R' F R U R' U' F' U R|]
    , [algExpr|F|] ++ sexy ++ [algExpr|F'|]
    , [algExpr|R' U' R' F R F' U R|]
    , [algExpr|F U R U' R' U R U' R' F'|]
    , [algExpr|R U R' U R U' B U' B' R'|]
    , [algExpr|R' F R U R U' R2 F' R2 U' R' U R U R'|]
    , [algExpr|r U r' U R U' R' U R U' R' r U' r'|]
    , [algExpr|R U R' U' M' U R U' r'|]
    ]
-- Angle flip algs

angleFlipAlgs :: [Algorithm]
angleFlipAlgs = applyFourSidesAlg 
    [ [algExpr|R' F2 L F L' F R|]
    , [algExpr|L F2 R' F' R F' L'|]
    , [algExpr|L F R' F R F2 L'|]
    , [algExpr|R' F' L F' L' F2 R|]
    , [algExpr|R U R' U' R' F R2 U R' U' F'|]
    , [algExpr|R U R' U R' F R F' R U2 R'|]
    , [algExpr|L F R' F R' D R D' R F2 L'|]
    , [algExpr|R2 L F' R F' R' F2 R F' R L'|]
    , [algExpr|L F R' F' L' R U R U' R'|]
    , [algExpr|R U R' U' R U' R' F' U' F R U R'|]
    , [algExpr|F R' F R2 U' R' U' R U R' F2|]
    , [algExpr|R' U' F U R U' R' F' R|]
    , [algExpr|L U F' U' L' U L F L'|]
    , [algExpr|R U2 R2 F R F' R U2 R'|]
    , [algExpr|L' U' L U' L' U L U L F' L' F|]
    , [algExpr|F R' F' R U R U' R'|]
    , [algExpr|R U R' U R U' R' U' R' F R F'|]
    , [algExpr|R U R' U R U2 R' F R U R' U' F'|]
    , [algExpr|R' U' R U' R' U2 R F R U R' U' F'|]
    , [algExpr|F' U' L' U L F|]
    , [algExpr|F|] ++ reverseSexy ++ [algExpr|F'|]
    , [algExpr|R' U' R' F R F' R' F R F' U R|]
    , [algExpr|F R U R' U' R U R' U' F'|]
    , [algExpr|L F' L2 B L2 F L2 B' L|]
    , [algExpr|L' B L2 F' L2 B' L2 F L'|]
    , [algExpr|R' F2 L F L' F' L F L' F R|]
    , [algExpr|r U2 R' U' R U R' U' R U' r'|]
    ]

-- Edges oriented algs

edgesOrientedAlgs :: [Algorithm]
edgesOrientedAlgs = [] : applyFourSidesAlg
    [sune, antisune, hOll, lOll, piOll, tOll, uOll]

sune :: Algorithm
sune = [algExpr|R U R' U R U2 R'|]

antisune :: Algorithm
antisune = reverseMoveSeq sune

hOll :: Algorithm
hOll = [algExpr|R U R' U R U' R' U R U2 R'|]

lOll :: Algorithm
lOll = [algExpr|F R' F' r U R U' r'|]

piOll :: Algorithm
piOll = [algExpr|R U2 R2 U' R2 U' R2 U2 R|]

tOll :: Algorithm
tOll = [algExpr|r U R' U' r' F R F'|]

uOll :: Algorithm
uOll = [algExpr|R2 D R' U2 R D' R' U2 R'|]
