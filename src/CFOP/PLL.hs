module CFOP.PLL (pll, pllSolved) where

import Control.Monad.State
import CubeState
import Cube
import Triggers
import CFOP.Cross (crossSolved)
import CFOP.F2L (f2lSolved)
import CFOP.OLL (ollSolved)

data PLLCategory = EdgesOnly | AdjecentCornerSwap | DiagonalCornerSwap
    deriving (Eq, Show)

facePairs :: [(CubeState -> Corner, CubeState -> Corner)]
facePairs = [(ufl, urf), (urf, ubr), (ubr, ulb), (ulb, ufl)]

sides :: [(CubeState -> Corner, CubeState -> Edge, CubeState -> Corner)]
sides = zipWith (\(left, right) mid -> (left, mid, right)) facePairs [uf, ur, ub, ul]

cornerSwapType :: CubeState -> PLLCategory
cornerSwapType cubeState =
    let headlightCount = sum $ map (\(f1, f2) -> headlights (f1 cubeState) (f2 cubeState)) facePairs
    in case headlightCount of
        0 -> DiagonalCornerSwap
        1 -> AdjecentCornerSwap
        4 -> EdgesOnly
        _ -> error "Headlights are not valid for any PLL case"
        

-- Left corner is first paramter, right corner is second paramter
headlights :: Corner -> Corner -> Int
headlights (Corner _ c1 _) (Corner _ _ c2) =  
    if c1 == c2
    then 1
    else 0

pll :: Cube Algorithm
pll = do
    cubeState <- get
    if all (\validator -> validator cubeState) [crossSolved, f2lSolved, ollSolved]
        then
            if pllSolved cubeState
                then return []
                else solvePllByCategory (cornerSwapType cubeState)
        else error "Cross, F2L and OLL must solved before PLL"

pllSolved :: CubeState -> Bool
pllSolved cubeState = all 
    (\(left, mid, right) -> 
        equalPllSide 
            (left cubeState) 
            (mid cubeState) 
            (right cubeState)
    ) 
    sides

equalPllSide :: Corner -> Edge -> Corner -> Bool
equalPllSide (Corner _ c1 _) (Edge _ c2) (Corner _ _ c3) = c1 == c2 && c1 == c3

solvePllByCategory :: PLLCategory -> Cube Algorithm
solvePllByCategory EdgesOnly = tryPllAlg (prependAuf edgesOnlyAlgs)
solvePllByCategory AdjecentCornerSwap = tryPllAlg (prependAuf adjecentCornerSwapAlgs)
solvePllByCategory DiagonalCornerSwap = tryPllAlg (prependAuf diagonalCornerSwapAlgs)

tryPllAlg :: [Algorithm] -> Cube Algorithm
tryPllAlg algs = do
    cubeState <- get
    case tryAlg algs cubeState pllSolved of
        Left errorMessage -> error $ "Failed solving PLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

-- Edges only

edgesOnlyAlgs :: [Algorithm]
edgesOnlyAlgs = [hPerm, zPerm, uaPerm, ubPerm]

hPerm :: Algorithm
hPerm =
    map (uncurry Move)
    [ (R, Two)
    , (U, Two)
    , (R, Normal)
    , (U, Two)
    , (R, Two)
    , (U, Two)
    , (R, Two)
    , (U, Two)
    , (R, Normal)
    , (U, Two)
    , (R, Two)
    ]

zPerm :: Algorithm
zPerm =
    map (uncurry Move)
    [ (R, Prime)
    , (U, Prime)
    , (R, Normal)
    , (U, Prime)
    , (R, Normal)
    , (U, Normal)
    , (R, Normal)
    , (U, Prime)
    , (R, Prime)
    , (U, Normal)
    , (R, Normal)
    , (U, Normal)
    , (R, Two)
    , (U, Prime)
    , (R, Prime)
    ]

uaPerm :: Algorithm
uaPerm =
    map (uncurry Move)
    [ (R, Normal)
    , (U, Prime)
    , (R, Normal)
    , (U, Normal)
    , (R, Normal)
    , (U, Normal)
    , (R, Normal)
    , (U, Prime)
    , (R, Prime)
    , (U, Prime)
    , (R, Two)
    ]

ubPerm :: Algorithm
ubPerm =
    map (uncurry Move)
    [ (R, Two)
    , (U, Normal)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (U, Prime)
    , (R, Prime)
    , (U, Prime)
    , (R, Prime)
    , (U, Normal)
    , (R, Prime)
    ]

-- Adjacent corner swap

adjecentCornerSwapAlgs :: [Algorithm]
adjecentCornerSwapAlgs = 
    [ tPerm
    , jaPerm
    , jbPerm
    , fPerm
    , raPerm
    , rbPerm
    , aaPerm
    , abPerm
    , gaPerm
    , gbPerm
    , gcPerm
    , gdPerm
    ]

tPerm :: Algorithm
tPerm =
    sexy ++
    map (uncurry Move)
    [ (R, Prime)
    , (F, Normal)
    , (R, Two)
    , (U, Prime)
    , (R, Prime)
    , (U, Prime)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (F, Prime)
    ]

jaPerm :: Algorithm
jaPerm =
    map (uncurry Move)
    [ (R, Normal)
    , (U, Prime)
    , (L, Prime)
    , (U, Normal)
    , (R, Prime)
    , (U, Two)
    , (L, Normal)
    , (U, Prime)
    , (L, Prime)
    , (U, Two)
    , (L, Normal)
    ]

jbPerm :: Algorithm
jbPerm =
    map (uncurry Move)
    [ (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (F, Prime)
    ]
    ++ sexy ++
    map (uncurry Move)
    [ (R, Prime)
    , (F, Normal)
    , (R, Two)
    , (U, Prime)
    , (R, Prime)
    ]

fPerm :: Algorithm
fPerm =
    map (uncurry Move)
    [ (R, Prime)
    , (U, Prime)
    , (F, Prime)
    ]
    ++ sexy ++
    map (uncurry Move)
    [ (R, Prime)
    , (F, Normal)
    , (R, Two)
    , (U, Prime)
    , (R, Prime)
    , (U, Prime)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (U, Normal)
    , (R, Normal)
    ]

aaPerm :: Algorithm
aaPerm =
    map (uncurry Move)
    [ (R, Prime)
    , (F, Normal)
    , (R, Prime)
    , (B, Two)
    , (R, Normal)
    , (F, Prime)
    , (R, Prime)
    , (B, Two)
    , (R, Two)
    ]

abPerm :: Algorithm
abPerm = reverseMoveSeq aaPerm

raPerm :: Algorithm
raPerm =
    map (uncurry Move)
    [ (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (F, Prime)
    , (R, Normal)
    , (U, Two)
    , (R, Prime)
    , (U, Two)
    , (R, Prime)
    , (F, Normal)
    , (R, Normal)
    , (U, Normal)
    , (R, Normal)
    , (U, Two)
    , (R, Prime)
    ]

rbPerm :: Algorithm
rbPerm =
    map (uncurry Move)
    [ (R, Two)
    , (F, Normal)
    , (R, Normal)
    , (U, Normal)
    , (R, Normal)
    , (U, Prime)
    , (R, Prime)
    , (F, Prime)
    , (R, Normal)
    , (U, Two)
    , (R, Prime)
    , (U, Two)
    , (R, Normal)
    ]

gaPerm :: Algorithm
gaPerm =
    map (uncurry Move)
    [ (R, Two)
    , (U, Normal)
    , (R, Prime)
    , (U, Normal)
    , (R, Prime)
    , (U, Prime)
    , (R, Normal)
    , (U, Prime)
    , (R, Two)
    , (U, Prime)
    , (D, Normal)
    , (R, Prime)
    , (U, Normal)
    , (R, Normal)
    , (D, Prime)
    ]

gbPerm :: Algorithm
gbPerm = reverseMoveSeq gaPerm

gcPerm :: Algorithm
gcPerm = map reverseMove gaPerm 

gdPerm :: Algorithm
gdPerm = reverseMoveSeq gcPerm

-- Diagonal corner swap

diagonalCornerSwapAlgs :: [Algorithm]
diagonalCornerSwapAlgs = [yPerm, vPerm, naPerm, nbPerm, ePerm]

yPerm :: Algorithm
yPerm =
    map (uncurry Move)
    [ (F, Normal)
    , (R, Normal)
    , (U, Prime)
    , (R, Prime)
    , (U, Prime)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (F, Prime)
    ]
    ++ sexy
    ++ sledgeHammer

vPerm :: Algorithm
vPerm =
    map (uncurry Move)
    [ (R, Normal)
    , (U, Prime)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (D, Normal)
    , (R, Normal)
    , (D, Prime)
    , (R, Normal)
    , (U, Prime)
    , (D, Normal)
    , (R, Two)
    , (U, Normal)
    , (R, Two)
    , (D, Prime)
    , (R, Two)
    ]

ePerm :: Algorithm
ePerm =
    map (uncurry Move)
    [ (R, Prime)
    , (U, Prime)
    , (R, Prime)
    , (D, Prime)
    , (R, Normal)
    , (U, Prime)
    , (R, Prime)
    , (D, Normal)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (D, Prime)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (D, Normal)
    , (R, Two) 
    ]

naPerm :: Algorithm
naPerm =
    map (uncurry Move)
    [ (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (U, Normal)
    ]
    ++ jbPerm ++
    map (uncurry Move)
    [ (U, Two)
    , (R, Normal)
    , (U, Prime)
    , (R, Prime)
    ]

nbPerm :: Algorithm
nbPerm =
    map (uncurry Move)
    [ (R, Prime)
    , (U, Normal)
    , (R, Normal)
    , (U, Prime)
    , (R, Prime)
    , (F, Prime)
    , (U, Prime)
    , (F, Normal)
    , (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (F, Normal)
    , (R, Prime)
    , (F, Prime)
    , (R, Normal)
    , (U, Prime)
    , (R, Normal)
    ]
