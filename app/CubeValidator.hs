module CubeValidator where

import CubeState

validateCubeState :: CubeState -> Bool
validateCubeState cubeState = validatePieceQuantity cubeState
                           && validateCenters cubeState
                           && validateEdgeOrientation cubeState
                           && validateCornerRotation cubeState
                           && validateEdgeAndCornerSwap cubeState

validateCornerRotation :: CubeState -> Bool
validateCornerRotation cubeState = totalCornerSum cubeState `mod` 3 == 0

totalCornerSum :: CubeState -> Int
totalCornerSum cubeState = sum $ map cornerSum $ cubeCorners cubeState

cubeCorners :: CubeState -> [Corner]
cubeCorners cs = [ urf cs
                 , ubr cs
                 , ulb cs
                 , ufl cs
                 , dfr cs
                 , dlf cs
                 , dbl cs
                 , drb cs
                 ]

cornerSum :: Corner -> Int
cornerSum (Corner Yellow _ _) = 0
cornerSum (Corner White _ _) = 0
cornerSum (Corner _ Yellow  _) = 1
cornerSum (Corner _ White _) = 1
cornerSum (Corner _ _ Yellow) = 2
cornerSum (Corner _ _ White) = 2
cornerSum _ = undefined

validateEdgeOrientation :: CubeState -> Bool
validateEdgeOrientation cubeState = even $ totalEdgeSum cubeState

totalEdgeSum :: CubeState -> Int
totalEdgeSum cubeState = sum $ map edgeSum $ cubeEdges cubeState

getCubeEdges :: [CubeState -> Edge]
getCubeEdges = [ uf
               , ur
               , ub
               , ul
               , df
               , dl
               , db
               , dr
               , fr
               , fl
               , br
               , bl
               ]

cubeEdges :: CubeState -> [Edge]
cubeEdges cs = map (\x -> x cs) getCubeEdges

edgeSum :: Edge -> Int
edgeSum (Edge White _) = 0
edgeSum (Edge Yellow _) = 0
edgeSum (Edge _ White) = 1
edgeSum (Edge _ Yellow) = 1
edgeSum (Edge Green _) = 0
edgeSum (Edge Blue _) = 0
edgeSum (Edge _ Green) = 1
edgeSum (Edge _ Blue) = 1
edgeSum _ = undefined

validateCenters :: CubeState -> Bool
validateCenters cubeState =
       f cubeState == Center Green
    && b cubeState == Center Blue
    && u cubeState == Center White
    && d cubeState == Center Yellow
    && r cubeState == Center Red
    && l cubeState == Center Orange

validatePieceQuantity :: CubeState -> Bool
validatePieceQuantity cubeState = validateEdgeQuantity (cubeEdges cubeState) []
                               && validateCornerQuantity (cubeCorners cubeState) []

validateEdgeQuantity :: [Edge] -> [Edge] -> Bool
validateEdgeQuantity (x:xs) seen = edgeInList x (cubeEdges solvedCube)
                                && not (edgeInList x seen)
                                && validateEdgeQuantity xs (x:seen)
validateEdgeQuantity [] _ = True

edgeInList :: Edge -> [Edge] -> Bool
edgeInList edge (x:xs) = edge `edgeEquivalent` x
                      || edgeInList edge xs
edgeInList _ [] = False

edgeEquivalent :: Edge -> Edge -> Bool
edgeEquivalent e1 e2 = e1 == e2
                    || e1 == flipEdge e2

validateCornerQuantity :: [Corner] -> [Corner] -> Bool
validateCornerQuantity (x:xs) seen = cornerInList x (cubeCorners solvedCube)
                                  && not (cornerInList x seen)
                                  && validateCornerQuantity xs (x:seen)
validateCornerQuantity [] _ = True

cornerInList :: Corner -> [Corner] -> Bool
cornerInList corner (x:xs) = corner `cornerEquivalent` x
                          || cornerInList corner xs
cornerInList _ [] = False

cornerEquivalent :: Corner -> Corner -> Bool
cornerEquivalent c1 c2 = c1 == c2
                      || c1 == twistCorner c2
                      || c1 == twistCorner (twistCorner c2)

validateEdgeAndCornerSwap :: CubeState -> Bool
validateEdgeAndCornerSwap cubeState = (edgeSwapCount cubeState (cubeEdges cubeState) [] `mod` 2) == (cornerSwapCount cubeState (cubeCorners cubeState) [] `mod` 2)

edgeSwapCount :: CubeState -> [Edge] -> [Edge] -> Int
edgeSwapCount cubeState (x:xs) seen = 
    if x `edgeInList` seen
    then edgeSwapCount cubeState xs (x:seen)
    else let edgeCycle = edgeSwapCycleCount cubeState x x in edgeSwapCount cubeState xs ([x] ++ edgeCycle ++ seen) + length edgeCycle
edgeSwapCount _ [] _ = 0

edgeSwapCycleCount :: CubeState -> Edge -> Edge -> [Edge]
edgeSwapCycleCount cubeState source curr = let nextEdge = nextEdgeCycle cubeState curr in
    if nextEdge `edgeEquivalent` source then [] else nextEdge : edgeSwapCycleCount cubeState source nextEdge

nextEdgeCycle :: CubeState -> Edge -> Edge
nextEdgeCycle cubeState e
    | e `edgeEquivalent` uf solvedCube = uf cubeState
    | e `edgeEquivalent` ur solvedCube = ur cubeState
    | e `edgeEquivalent` ub solvedCube = ub cubeState
    | e `edgeEquivalent` ul solvedCube = ul cubeState
    | e `edgeEquivalent` df solvedCube = df cubeState
    | e `edgeEquivalent` dl solvedCube = dl cubeState
    | e `edgeEquivalent` db solvedCube = db cubeState
    | e `edgeEquivalent` dr solvedCube = dr cubeState
    | e `edgeEquivalent` fr solvedCube = fr cubeState
    | e `edgeEquivalent` fl solvedCube = fl cubeState
    | e `edgeEquivalent` br solvedCube = br cubeState
    | e `edgeEquivalent` bl solvedCube = bl cubeState
    | otherwise = undefined

cornerSwapCount :: CubeState -> [Corner] -> [Corner] -> Int
cornerSwapCount cubeState (x:xs) seen =
    if x `cornerInList` seen 
    then cornerSwapCount cubeState xs (x:seen)
    else let cornerCycle = cornerSwapCycleCount cubeState x x in cornerSwapCount cubeState xs ([x] ++ cornerCycle ++ seen) + length cornerCycle
cornerSwapCount _ [] _ = 0

cornerSwapCycleCount :: CubeState -> Corner -> Corner -> [Corner]
cornerSwapCycleCount cubeState source curr = let nextCorner = nextCornerCycle cubeState curr in
    if nextCorner `cornerEquivalent` source then [] else nextCorner : cornerSwapCycleCount cubeState source nextCorner

nextCornerCycle :: CubeState -> Corner -> Corner
nextCornerCycle cubeState c
    | c `cornerEquivalent` urf solvedCube = urf cubeState
    | c `cornerEquivalent` ubr solvedCube = ubr cubeState
    | c `cornerEquivalent` ulb solvedCube = ulb cubeState
    | c `cornerEquivalent` ufl solvedCube = ufl cubeState
    | c `cornerEquivalent` dfr solvedCube = dfr cubeState
    | c `cornerEquivalent` dlf solvedCube = dlf cubeState
    | c `cornerEquivalent` dbl solvedCube = dbl cubeState
    | c `cornerEquivalent` drb solvedCube = drb cubeState
    | otherwise = undefined
