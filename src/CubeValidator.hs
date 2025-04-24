module CubeValidator (validateCubeState) where

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
validateCornerRotation cubeState = totalPieceSum (pieces cubeState :: [Corner]) `mod` 3 == 0

validateEdgeOrientation :: CubeState -> Bool
validateEdgeOrientation cubeState = even $ totalPieceSum (pieces cubeState :: [Edge])

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
