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
tryPllAlg algs = tryAlg algs pllSolved

-- Edges only

edgesOnlyAlgs :: [Algorithm]
edgesOnlyAlgs = [hPerm, zPerm, uaPerm, ubPerm]

hPerm :: Algorithm
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

zPerm :: Algorithm
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

uaPerm :: Algorithm
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

ubPerm :: Algorithm
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

jaPerm :: Algorithm
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

jbPerm :: Algorithm
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

fPerm :: Algorithm
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

aaPerm :: Algorithm
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

abPerm :: Algorithm
abPerm = reverseMoveSeq aaPerm

raPerm :: Algorithm
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

rbPerm :: Algorithm
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

gaPerm :: Algorithm
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

vPerm :: Algorithm
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

ePerm :: Algorithm
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

naPerm :: Algorithm
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

nbPerm :: Algorithm
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
