module CFOP.OLL where

import Cube
import Triggers
import CubeState
import CubeValidator
import Control.Monad.State

oll :: Cube Algorithm
oll = do
    cubeState <- get
    edgeFlipMoves <- flipEdges (edgeState cubeState)
    cornerSolveMoves <- solveCorners
    return $ edgeFlipMoves ++ cornerSolveMoves

data EdgeState = Dot | Line | Angle | EdgesOriented
    deriving (Eq, Show)

edgeState :: CubeState -> EdgeState
edgeState cubeState = case totalEdgeSum cubeState of
    0 -> EdgesOriented
    2 -> if firstE (uf cubeState) == firstE (ub cubeState) 
         || firstE (ul cubeState) == firstE (ur cubeState) 
        then Line else Angle 
    4 -> Dot
    _ -> undefined

edgesOriented :: CubeState -> Bool
edgesOriented cubeState = edgeState cubeState == EdgesOriented

tryEdgeFlipAlg :: [Algorithm] -> Cube Algorithm
tryEdgeFlipAlg algs = tryAlg algs edgesOriented

flipEdges :: EdgeState -> Cube Algorithm
flipEdges EdgesOriented = return []
flipEdges Line = tryEdgeFlipAlg (prependMoves [Move U Normal] [lineFlip])
flipEdges Angle = tryEdgeFlipAlg (prependAuf [angleFlip])
flipEdges Dot = applyAlgorithm dotFlip

cornersOriented :: CubeState -> Bool
cornersOriented cubeState = totalCornerSum cubeState == 0

solveCorners :: Cube Algorithm
solveCorners = do 
    tryAlg (prependAuf [[], sune, antisune, hOll, lOll, piOll, tOll, uOll]) cornersOriented

dotFlip :: Algorithm
dotFlip = angleFlip ++ [Move U Normal] ++ lineFlip

lineFlip :: Algorithm
lineFlip = [Move F Normal] ++ sexy ++ [Move F Prime]

angleFlip :: Algorithm
angleFlip = reverseMoveSeq lineFlip

sune :: Algorithm
sune =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Two
    , Move R Prime
    ]

antisune :: Algorithm
antisune = reverseMoveSeq sune

hOll :: Algorithm
hOll =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Two
    , Move R Prime
    ]

lOll :: Algorithm
lOll = 
    [ Move F Normal
    , Move R Prime
    , Move F Prime
    , Move L Normal
    , Move F Normal
    , Move R Normal
    , Move F Prime
    , Move L Prime
    ]

piOll :: Algorithm
piOll = 
    [ Move R Normal
    , Move U Two
    , Move R Two
    , Move U Prime
    , Move R Two
    , Move U Prime
    , Move R Two
    , Move U Two
    , Move R Normal
    ]

tOll :: Algorithm
tOll = reverseMoveSeq lOll

uOll :: Algorithm
uOll = 
    [ Move R Two
    , Move D Normal
    , Move R Prime
    , Move U Two
    , Move D Prime
    , Move R Prime
    , Move U Two
    , Move R Prime
    ]