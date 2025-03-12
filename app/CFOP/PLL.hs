module CFOP.PLL where

import Control.Monad.State
import CubeState
import Cube
import Triggers

data CornerPair = CornerPair (CubeState -> Corner) (CubeState -> Corner) 
data PLLCategory = EdgesOnly | AdjecentCornerSwap | DiagonalCornerSwap
    deriving (Eq, Show)

cornerSwapType :: CubeState -> PLLCategory
cornerSwapType cubeState =
    let headlightCount
            = headlights (ufl cubeState) (urf cubeState)
            + headlights (urf cubeState) (ubr cubeState)
            + headlights (ubr cubeState) (ulb cubeState)
            + headlights (ulb cubeState) (ufl cubeState)
    in case headlightCount of
        0 -> DiagonalCornerSwap
        1 -> AdjecentCornerSwap
        4 -> EdgesOnly
        _ -> undefined
        

-- Left corner is first paramter, right corner is second paramter
headlights :: Corner -> Corner -> Int
headlights (Corner _ c1 _) (Corner _ _ c2) =  
    if c1 == c2
    then 1
    else 0

pll :: Cube [Move]
pll = do
    cubeState <- get
    if isPllSolved cubeState
    then return []
    else solvePllByCategory (cornerSwapType cubeState)

isPllSolved :: CubeState -> Bool
isPllSolved cubeState =
       equalPllSide (ufl cubeState) (uf cubeState) (urf cubeState)
    && equalPllSide (urf cubeState) (ur cubeState) (ubr cubeState)
    && equalPllSide (ubr cubeState) (ub cubeState) (ulb cubeState)
    && equalPllSide (ulb cubeState) (ul cubeState) (ufl cubeState)

equalPllSide :: Corner -> Edge -> Corner -> Bool
equalPllSide (Corner _ c1 _) (Edge _ c2) (Corner _ _ c3) = c1 == c2 && c1 == c3

solvePllByCategory :: PLLCategory -> Cube [Move]
solvePllByCategory EdgesOnly = tryPllAlg (prependAuf [hPerm, zPerm, uaPerm, ubPerm])
solvePllByCategory AdjecentCornerSwap = tryPllAlg (prependAuf [tPerm, jaPerm, jbPerm, fPerm, raPerm, rbPerm, aaPerm, abPerm, gaPerm, gbPerm, gcPerm, gdPerm])
solvePllByCategory DiagonalCornerSwap = tryPllAlg (prependAuf [yPerm, vPerm, naPerm, nbPerm, ePerm])

prependAuf :: [[Move]] -> [[Move]]
prependAuf = prependMoves aufMoves where
    prependMoves (x:xs) algorithms = prependMoves xs algorithms ++ map (x:) algorithms
    prependMoves [] algorithms = algorithms

tryPllAlg :: [[Move]] -> Cube [Move]
tryPllAlg [] = undefined
tryPllAlg (x:xs) = do
    _ <- applyAlgorithm x
    cubeState <- get
    if isPllSolved cubeState
    then return x
    else do
        _ <- applyAlgorithm (reverseMoveSeq x)
        tryPllAlg xs

-- Edges only

hPerm :: [Move]
hPerm =
    [ Move R Two
    , Move U Two
    , Move R Normal
    , Move U Two
    , Move R Two
    , Move U Two
    , Move R Two
    , Move U Two
    , Move R Normal
    , Move U Two
    , Move R Two
    ]

zPerm :: [Move]
zPerm =
    [ Move R Prime
    , Move U Prime
    , Move R Normal
    , Move U Prime
    , Move R Normal
    , Move U Normal
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Normal
    , Move R Two
    , Move U Prime
    , Move R Prime
    ]

uaPerm :: [Move]
uaPerm =
    [ Move R Normal
    , Move U Prime
    , Move R Normal
    , Move U Normal
    , Move R Normal
    , Move U Normal
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move U Prime
    , Move R Two
    ]

ubPerm :: [Move]
ubPerm =
    [ Move R Two
    , Move U Normal
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Prime
    , Move R Prime
    , Move U Prime
    , Move R Prime
    , Move U Normal
    , Move R Prime
    ]

-- Adjacent corner swap
tPerm :: [Move]
tPerm =
    sexy ++
    [ Move R Prime
    , Move F Normal
    , Move R Two
    , Move U Prime
    , Move R Prime
    , Move U Prime
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move F Prime
    ]

jaPerm :: [Move]
jaPerm =
    [ Move R Normal
    , Move U Prime
    , Move L Prime
    , Move U Normal
    , Move R Prime
    , Move U Two
    , Move L Normal
    , Move U Prime
    , Move L Prime
    , Move U Two
    , Move L Normal
    ]

jbPerm :: [Move]
jbPerm =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move F Prime
    ]
    ++ sexy ++
    [ Move R Prime
    , Move F Normal
    , Move R Two
    , Move U Prime
    , Move R Prime
    ]

fPerm :: [Move]
fPerm =
    [ Move R Prime
    , Move U Prime
    , Move F Prime
    ]
    ++ sexy ++
    [ Move R Prime
    , Move F Normal
    , Move R Two
    , Move U Prime
    , Move R Prime
    , Move U Prime
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Normal
    , Move R Normal
    ]

aaPerm :: [Move]
aaPerm =
    [ Move R Prime
    , Move F Normal
    , Move R Prime
    , Move B Two
    , Move R Normal
    , Move F Prime
    , Move R Prime
    , Move B Two
    , Move R Two
    ]

abPerm :: [Move]
abPerm = reverseMoveSeq aaPerm

raPerm :: [Move]
raPerm =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move F Prime
    , Move R Normal
    , Move U Two
    , Move R Prime
    , Move U Two
    , Move R Prime
    , Move F Normal
    , Move R Normal
    , Move U Normal
    , Move R Normal
    , Move U Two
    , Move R Prime
    ]

rbPerm :: [Move]
rbPerm =
    [ Move R Two
    , Move F Normal
    , Move R Normal
    , Move U Normal
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move F Prime
    , Move R Normal
    , Move U Two
    , Move R Prime
    , Move U Two
    , Move R Normal
    ]

gaPerm :: [Move]
gaPerm =
    [ Move R Two
    , Move U Normal
    , Move R Prime
    , Move U Normal
    , Move R Prime
    , Move U Prime
    , Move R Normal
    , Move U Prime
    , Move R Two
    , Move U Prime
    , Move D Normal
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move D Prime
    ]

gbPerm :: [Move]
gbPerm = reverseMoveSeq gaPerm

gcPerm :: [Move]
gcPerm = map reverseMove gaPerm 

gdPerm :: [Move]
gdPerm = reverseMoveSeq gcPerm

-- Diagonal corner swap

yPerm :: [Move]
yPerm =
    [ Move F Normal
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move U Prime
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move F Prime
    ]
    ++ sexy
    ++ sledgeHammer

vPerm :: [Move]
vPerm =
    [ Move R Normal
    , Move U Prime
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move D Normal
    , Move R Normal
    , Move D Prime
    , Move R Normal
    , Move U Prime
    , Move D Normal
    , Move R Two
    , Move U Normal
    , Move R Two
    , Move D Prime
    , Move R Two
    ]

ePerm :: [Move]
ePerm =
    [ Move R Prime
    , Move U Prime
    , Move R Prime
    , Move D Prime
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move D Normal
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move D Prime
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move D Normal
    , Move R Two 
    ]

naPerm :: [Move]
naPerm =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Normal
    ]
    ++ jbPerm ++
    [ Move U Two
    , Move R Normal
    , Move U Prime
    , Move R Prime
    ]

nbPerm :: [Move]
nbPerm =
    [ Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move F Prime
    , Move U Prime
    , Move F Normal
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move F Normal
    , Move R Prime
    , Move F Prime
    , Move R Normal
    , Move U Prime
    , Move R Normal
    ]