{-# LANGUAGE QuasiQuotes #-}
module CFOP.PLL (pll, pllSolved) where

import Control.Monad.State
import CubeState
import Cube
import Triggers
import CFOP.Cross (crossSolved)
import CFOP.F2L (f2lSolved)
import CFOP.OLL (ollSolved)
import AlgExpr

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
solvePllByCategory EdgesOnly = tryPllAlg edgesOnlyAlgs
solvePllByCategory AdjecentCornerSwap = tryPllAlg adjecentCornerSwapAlgs
solvePllByCategory DiagonalCornerSwap = tryPllAlg diagonalCornerSwapAlgs

tryPllAlg :: [Algorithm] -> Cube Algorithm
tryPllAlg algs = do
    cubeState <- get
    case tryAlg algs cubeState pllSolved of
        Left errorMessage -> error $ "Failed solving PLL: " ++ errorMessage 
        Right alg -> applyAlgorithm alg

-- Edges only

edgesOnlyAlgs :: [Algorithm]
edgesOnlyAlgs = applyFourSidesAlg [hPerm, zPerm, uaPerm, ubPerm]

hPerm :: Algorithm
hPerm = [algExpr|R2 U2 R U2 R2 U2 R2 U2 R U2 R2|]

zPerm :: Algorithm
zPerm = [algExpr|R' U' R U' R U R U' R' U R U R2 U' R'|]

uaPerm :: Algorithm
uaPerm = [algExpr|R U' R U R U R U' R' U' R2|]

ubPerm :: Algorithm
ubPerm = reverseMoveSeq uaPerm

-- Adjacent corner swap

adjecentCornerSwapAlgs :: [Algorithm]
adjecentCornerSwapAlgs = applyFourSidesAlg
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
    sexy ++ [algExpr|R' F R2 U' R' U' R U R' F'|]

jaPerm :: Algorithm
jaPerm = [algExpr|R U' L' U R' U2 L U' L' U2 L|]

jbPerm :: Algorithm
jbPerm = [algExpr|R U R' F'|] ++ sexy ++ [algExpr|R' F R2 U' R'|]

fPerm :: Algorithm
fPerm = [algExpr|R' U' F'|] ++ init tPerm ++ [algExpr|U R|]

aaPerm :: Algorithm
aaPerm = [algExpr|R' F R' B2 R F' R' B2 R2|]

abPerm :: Algorithm
abPerm = reverseMoveSeq aaPerm

raPerm :: Algorithm
raPerm = [algExpr|R U R' F' R U2 R' U2 R' F R U R U2 R'|]

rbPerm :: Algorithm
rbPerm = [algExpr|R2 F R U R U' R' F' R U2 R' U2 R|]

gaPerm :: Algorithm
gaPerm = [algExpr|R2 U R' U R' U' R U' R2 U' D R' U R D'|]

gbPerm :: Algorithm
gbPerm = reverseMoveSeq gaPerm

gcPerm :: Algorithm
gcPerm = map reverseMove gaPerm 

gdPerm :: Algorithm
gdPerm = reverseMoveSeq gcPerm

-- Diagonal corner swap

diagonalCornerSwapAlgs :: [Algorithm]
diagonalCornerSwapAlgs = applyFourSidesAlg [yPerm, vPerm, naPerm, nbPerm, ePerm]

yPerm :: Algorithm
yPerm = [algExpr|F R U' R' U' R U R' F'|] ++ sexy ++ sledgeHammer

vPerm :: Algorithm
vPerm = [algExpr|R U' R U R' D R D' R U' D R2 U R2 D' R2|]

ePerm :: Algorithm
ePerm = [algExpr|R' U' R' D' R U' R' D R U R' D' R U R' D R2|]

naPerm :: Algorithm
naPerm = [algExpr|R U R' U|] ++ jbPerm ++ [algExpr|U2 R U' R'|]

nbPerm :: Algorithm
nbPerm = [algExpr|R' U R U' R' F' U' F R U R' F R' F' R U' R|]
