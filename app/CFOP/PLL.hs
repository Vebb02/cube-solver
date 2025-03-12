module CFOP.PLL where

import CubeState
import Cube

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
    undefined


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
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Prime
    , Move R Prime
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
    , Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Prime
    , Move R Prime
    , Move F Normal
    , Move R Two
    , Move U Prime
    , Move R Prime
    ]

-- Diagonal corner swap
