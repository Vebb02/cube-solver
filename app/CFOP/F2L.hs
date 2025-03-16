module CFOP.F2L where

import Cube
import CubeState
import Control.Monad.State
import CubeValidator
import Data.Maybe

f2l :: Cube Algorithm
f2l = do
    cubeState <- get
    if f2lSolved cubeState
        then return []
        else solveF2l [BL, BR, FL, FR]

type F2LPair = (Edge, Corner)

data F2LSlot = FL | FR | BL | BR
    deriving (Eq, Show)

allSlots :: [(F2LSlot, CubeState -> Edge, CubeState -> Corner)]
allSlots = [(FL, fl, dlf), (FR, fr, dfr), (BL, bl, dbl), (BR, br, drb)]

allPairs :: [F2LSlot]
allPairs = map (\(x,_,_)-> x) allSlots

allEdges :: [CubeState -> Edge]
allEdges = map (\(_,x,_)-> x) allSlots

allCorners :: [CubeState -> Corner]
allCorners = map (\(_,_,x)-> x) allSlots

f2lSlot :: CubeState -> F2LSlot -> F2LPair
f2lSlot cubeState FL = (fl cubeState, dlf cubeState)
f2lSlot cubeState FR = (fr cubeState, dfr cubeState)
f2lSlot cubeState BL = (bl cubeState, dbl cubeState)
f2lSlot cubeState BR = (br cubeState, drb cubeState)

f2lSolved :: CubeState -> Bool
f2lSolved cubeState = all (\slot -> f2lSlot cubeState slot == f2lSlot solvedCube slot) allPairs

solveF2l :: [F2LSlot] -> Cube Algorithm
solveF2l (x:xs) = do
    pair <- solveF2lPair x
    rest <- solveF2l xs
    return $ pair ++ rest
solveF2l [] = return []

solveF2lPair :: F2LSlot -> Cube Algorithm
solveF2lPair slot = do
    -- orientEdgeAndCorner slot
    orientMoves <- orientEdgeAndCorner slot
    cubeState <- get
    if f2lSolved cubeState
    then return orientMoves
    else do
        pairMoves <-  putPairInSlot slot
        return $ orientMoves ++ pairMoves

orientEdgeAndCorner :: F2LSlot -> Cube Algorithm
orientEdgeAndCorner slot = do
    cubeState <- get
    let eSlot = edgeSlot cubeState (fst $ f2lSlot solvedCube slot)
    let cSlot = cornerSlot cubeState (snd $ f2lSlot solvedCube slot)
    fixOrientation slot eSlot cSlot

fixOrientation :: F2LSlot -> Maybe F2LSlot -> Maybe F2LSlot -> Cube Algorithm
fixOrientation _ Nothing Nothing = return []
fixOrientation slot (Just eSlot) Nothing = do
    cubeState <- get
    let solvedEdge = fst $ f2lSlot solvedCube slot
    let edge = edgeInEdgeList (cubeEdges cubeState) solvedEdge
    let corner = snd $ f2lSlot solvedCube slot
    if eSlot == slot && edgeSum (fst $ f2lSlot cubeState slot) == 0
        then return []
        else tryAlg (orientationMoves eSlot edge) (\cs-> isNothing (cornerSlot cs corner))
fixOrientation slot Nothing (Just cSlot) = do
    let edge = fst $ f2lSlot solvedCube slot
    if cSlot == slot then return []
        else tryAlg (orientationMoves cSlot edge) (\cs-> isNothing (edgeSlot cs edge))
fixOrientation slot (Just eSlot) (Just cSlot) = do
    cubeState <- get
    let solvedEdge = fst $ f2lSlot solvedCube slot
    let edge = edgeInEdgeList (cubeEdges cubeState) solvedEdge
    if eSlot == cSlot
        then applyAlgorithm [slotToMove eSlot, Move U Normal, reverseMove $ slotToMove eSlot]
        else if eSlot == slot && edgeSum edge == 0
            then applyAlgorithm [slotToMove cSlot, Move U Normal, reverseMove $ slotToMove cSlot]
            else do
            moves <- applyAlgorithm [edgeOrientationMove eSlot edge, Move U Normal, reverseMove $ edgeOrientationMove eSlot edge]
            restMoves <- fixOrientation slot Nothing (Just cSlot)
            return $ moves ++ restMoves

orientationMoves :: F2LSlot -> Edge -> [Algorithm]
orientationMoves slot edge = let m = edgeOrientationMove slot edge in [[m, aufMove, reverseMove m] | aufMove <- reverseMoveSeq aufMoves]

slotToMove :: F2LSlot -> Move
slotToMove FL = Move L Prime
slotToMove FR = Move R Normal
slotToMove BL = Move L Normal
slotToMove BR = Move R Prime

edgeOrientationMove :: F2LSlot -> Edge -> Move
edgeOrientationMove slot edge = if edgeSum edge == 0 then slotToMove slot else slotToNonOrientedMove slot

slotToNonOrientedMove :: F2LSlot -> Move
slotToNonOrientedMove FL = Move F Normal
slotToNonOrientedMove FR = Move F Prime
slotToNonOrientedMove BL = Move B Prime
slotToNonOrientedMove BR = Move B Normal

edgeSlot :: CubeState -> Edge -> Maybe F2LSlot
edgeSlot cubeState edge = pieceSlot (zip allPairs allEdges) cubeState edge edgeEquivalent

cornerSlot :: CubeState -> Corner -> Maybe F2LSlot
cornerSlot cubeState corner = pieceSlot (zip allPairs allCorners) cubeState corner cornerEquivalent

pieceSlot :: [(F2LSlot, CubeState -> a)] -> CubeState -> a -> (a -> a -> Bool) -> Maybe F2LSlot
pieceSlot ((slot, getPiece):xs) cubeState piece equivalent =
    if getPiece cubeState `equivalent` piece
    then Just slot
    else pieceSlot xs cubeState piece equivalent
pieceSlot [] _ _ _ = Nothing

edgeInEdgeList :: [Edge] -> Edge -> Edge
edgeInEdgeList (x:xs) edge = if edge `edgeEquivalent` x then x else edgeInEdgeList xs edge
edgeInEdgeList [] _ = undefined

putPairInSlot :: F2LSlot -> Cube Algorithm
putPairInSlot slot = do
    cubeState <- get
    let solvedEdge = fst $ f2lSlot solvedCube slot
    let edge = edgeInEdgeList (cubeEdges cubeState) solvedEdge
    let sideMove = edgeOrientationMove slot edge
    solveSlot slot sideMove (map (\(currAlg, currState) -> let (resultAlg, resultState) = runState (applyAlgorithm [reverseMove sideMove]) currState in (currAlg ++ resultAlg, resultState)) (branchWithU (runState (applyAlgorithm [sideMove]) cubeState)) ++ createNewStates sideMove [([], cubeState)])

solveSlot :: F2LSlot -> Move -> [(Algorithm, CubeState)] -> Cube Algorithm
solveSlot _ _ [] = undefined
solveSlot slot sideMove states = do
    case validAlgorithm slot states of
        Nothing -> solveSlot slot sideMove (createNewStates sideMove states)
        Just (alg, _) -> do
            applyAlgorithm alg

createNewStates :: Move -> [(Algorithm, CubeState)] -> [(Algorithm, CubeState)]
createNewStates sideMove (x:xs) = if length xs > 20000 then error $ "Length too long: " ++ show (length xs) else getStatesFromState sideMove x ++ createNewStates sideMove xs
createNewStates _ [] = []

getStatesFromState :: Move -> (Algorithm, CubeState) -> [(Algorithm, CubeState)]
getStatesFromState sideMove (alg, cubeState) = do
    let step1 = branchWithU (alg, cubeState)
    let step2 = map (\(currAlg, currState) -> let (resultAlg, resultState) = runState (applyAlgorithm [sideMove]) currState in (currAlg ++ resultAlg, resultState)) step1
    let step3 = foldr (\x acc -> branchWithU x ++ acc) [] step2
    let step4 = map (\(currAlg, currState) -> let (resultAlg, resultState) = runState (applyAlgorithm [reverseMove sideMove]) currState in (currAlg ++ resultAlg, resultState)) step3
    step4

branchWithU :: (Algorithm, CubeState) -> [(Algorithm, CubeState)]
branchWithU = branchWithMoves [Move U Normal, Move U Prime, Move U Two]

branchWithMoves :: [Move] -> (Algorithm, CubeState) -> [(Algorithm, CubeState)]
branchWithMoves (x:xs) (alg, cubeState) = let (_, resultState) = runState (applyAlgorithm [x]) cubeState 
    in (alg ++ [x], resultState) : branchWithMoves xs (alg, cubeState)
branchWithMoves [] _ = []

validAlgorithm :: F2LSlot -> [(Algorithm, CubeState)] -> Maybe (Algorithm, CubeState)
validAlgorithm _ [] = Nothing
validAlgorithm slot ((alg, cubeState):xs) =
    if f2lSlot cubeState slot == f2lSlot solvedCube slot
        then Just (alg, cubeState)
        else validAlgorithm slot xs
