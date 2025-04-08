module CubeValidator 
    ( validateCubeState
    , edgeSum
    , totalEdgeSum
    , totalCornerSum
    ) 
    where

import CubeState

validateCubeState :: CubeState -> Bool
validateCubeState cubeState = 
    all (\validator -> validator cubeState)
        [ validatePieceQuantity
        , validateCenters
        , validateEdgeOrientation
        , validateCornerRotation
        , validateEdgeAndCornerSwap
        ]

validateCornerRotation :: CubeState -> Bool
validateCornerRotation cubeState = totalCornerSum cubeState `mod` 3 == 0

totalCornerSum :: CubeState -> Int
totalCornerSum cubeState = sum $ map cornerSum $ pieces cubeState

cornerSum :: Corner -> Int
cornerSum (Corner Yellow _ _) = 0
cornerSum (Corner White _ _) = 0
cornerSum (Corner _ Yellow  _) = 1
cornerSum (Corner _ White _) = 1
cornerSum (Corner _ _ Yellow) = 2
cornerSum (Corner _ _ White) = 2
cornerSum _ = error "Invalid corner on cube"

validateEdgeOrientation :: CubeState -> Bool
validateEdgeOrientation cubeState = even $ totalEdgeSum cubeState

totalEdgeSum :: CubeState -> Int
totalEdgeSum cubeState = sum $ map edgeSum $ pieces cubeState

edgeSum :: Edge -> Int
edgeSum (Edge White _) = 0
edgeSum (Edge Yellow _) = 0
edgeSum (Edge _ White) = 1
edgeSum (Edge _ Yellow) = 1
edgeSum (Edge Green _) = 0
edgeSum (Edge Blue _) = 0
edgeSum (Edge _ Green) = 1
edgeSum (Edge _ Blue) = 1
edgeSum _ = error "Invalid edge on cube"

validateCenters :: CubeState -> Bool
validateCenters cubeState =
    all (\(center, color) -> center cubeState == Center color)
        (zip [f, b, u, d, r, l] [Green, Blue, White, Yellow, Red, Orange])

validatePieceQuantity :: CubeState -> Bool
validatePieceQuantity cubeState = 
       validatePieceQuantity' (pieces cubeState :: [Edge]) []
    && validatePieceQuantity' (pieces cubeState :: [Corner]) []

validatePieceQuantity' :: (Piece a) => [a] -> [a] -> Bool
validatePieceQuantity' (x:xs) seen =
       pieceInList x (pieces solvedCube)
    && not (pieceInList x seen)
    && validatePieceQuantity' xs (x:seen)
validatePieceQuantity' [] _ = True

validateEdgeAndCornerSwap :: CubeState -> Bool
validateEdgeAndCornerSwap cubeState = 
    (edgeSwapCount cubeState `mod` 2) == (cornerSwapCount cubeState `mod` 2)

edgeSwapCount :: CubeState -> Int
edgeSwapCount cubeState = 
    pieceSwapCount cubeState (pieces cubeState :: [Edge]) []

cornerSwapCount :: CubeState -> Int
cornerSwapCount cubeState = 
    pieceSwapCount cubeState (pieces cubeState :: [Corner]) []

pieceSwapCount :: (Piece a) => CubeState -> [a] -> [a] -> Int
pieceSwapCount cubeState (x:xs) seen = 
    if x `pieceInList` seen
        then pieceSwapCount cubeState xs seen
        else
            let pCycle = pieceCycle cubeState x x
            in pieceSwapCount cubeState xs ([x] ++ pCycle ++ seen) + length pCycle
pieceSwapCount _ [] _ = 0

pieceCycle :: (Piece a) => CubeState -> a -> a -> [a]
pieceCycle cubeState source curr = 
    let nextPiece = findPieceOnCube solvedCube curr cubeState in
    if nextPiece `pieceEquivalent` source 
        then []
        else nextPiece : pieceCycle cubeState source nextPiece
