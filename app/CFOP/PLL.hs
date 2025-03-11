module CFOP.PLL where

import CubeState

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
