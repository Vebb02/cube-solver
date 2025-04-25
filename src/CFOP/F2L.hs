module CFOP.F2L (f2l, f2lSolved) where

import Cube
import CubeState
import Control.Monad.State
import Data.Maybe
import Data.List (permutations)
import CFOP.Cross

f2l :: Cube Algorithm
f2l = do
    cubeState <- get
    if crossSolved cubeState then
        if f2lSolved cubeState
            then return []
            else applyAlgorithm (bestPossibleF2L cubeState)
        else error "Cross needs to be solved before F2L"

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

bestPossibleF2L :: CubeState -> Algorithm
bestPossibleF2L cubeState = bestPossibleSolution cubeState solveF2l (permutations [BL, BR, FL, FR]) 

solveF2l :: [F2LSlot] -> Cube Algorithm
solveF2l (x:xs) = do
    pair <- solveF2lPair x
    rest <- solveF2l xs
    return $ pair ++ rest
solveF2l [] = return []

solveF2lPair :: F2LSlot -> Cube Algorithm
solveF2lPair slot = do
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
    let edge = edgeInEdgeList cubeState solvedEdge
    let corner = snd $ f2lSlot solvedCube slot
    if eSlot == slot && pieceSum (fst $ f2lSlot cubeState slot) == 0
        then return []
        else case tryAlg (orientationMoves eSlot edge) cubeState (\cs-> isNothing (cornerSlot cs corner)) of
            Left errorMessage -> error $ "Failed doing F2L: " ++ errorMessage 
            Right alg -> applyAlgorithm alg
fixOrientation slot Nothing (Just cSlot) = do
    let edge = fst $ f2lSlot solvedCube slot
    if cSlot == slot
        then return []
        else do
            cubeState <- get
            case tryAlg (orientationMoves cSlot edge) cubeState (\cs-> isNothing (edgeSlot cs edge)) of
                Left errorMessage -> error $ "Failed doing F2L: " ++ errorMessage 
                Right alg -> applyAlgorithm alg
fixOrientation slot (Just eSlot) (Just cSlot) = do
    cubeState <- get
    let solvedEdge = fst $ f2lSlot solvedCube slot
    let edge = edgeInEdgeList cubeState solvedEdge
    if eSlot == cSlot
        then applyAlgorithm [slotToMove eSlot, Move U Normal, reverseMove $ slotToMove eSlot]
        else if eSlot == slot && pieceSum edge == 0
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
edgeOrientationMove slot edge = if pieceSum edge == 0 then slotToMove slot else slotToNonOrientedMove slot

slotToNonOrientedMove :: F2LSlot -> Move
slotToNonOrientedMove FL = Move F Normal
slotToNonOrientedMove FR = Move F Prime
slotToNonOrientedMove BL = Move B Prime
slotToNonOrientedMove BR = Move B Normal

edgeSlot :: CubeState -> Edge -> Maybe F2LSlot
edgeSlot = pieceSlot (zip allPairs allEdges)

cornerSlot :: CubeState -> Corner -> Maybe F2LSlot
cornerSlot = pieceSlot (zip allPairs allCorners)

pieceSlot :: (Piece a) => [(F2LSlot, CubeState -> a)] -> CubeState -> a -> Maybe F2LSlot
pieceSlot ((slot, getPiece):xs) cubeState piece =
    if getPiece cubeState `pieceEquivalent` piece
        then Just slot
        else pieceSlot xs cubeState piece
pieceSlot [] _ _ = Nothing

edgeInEdgeList :: CubeState -> Edge -> Edge
edgeInEdgeList cubeState edge = findPieceOnCube cubeState edge cubeState

putPairInSlot :: F2LSlot -> Cube Algorithm
putPairInSlot slot = do
    cubeState <- get
    let solvedEdge = fst $ f2lSlot solvedCube slot
    let edge = edgeInEdgeList cubeState solvedEdge
    let sideMove = edgeOrientationMove slot edge
    solveSlot slot sideMove (map 
        (\(currAlg, currState) -> 
            let (resultAlg, resultState) = runState (applyAlgorithm [reverseMove sideMove]) currState 
            in (currAlg ++ resultAlg, resultState))
        (branchWithU (runState (applyAlgorithm [sideMove]) cubeState)) ++ createNewStates sideMove [([], cubeState)])

solveSlot :: F2LSlot -> Move -> [(Algorithm, CubeState)] -> Cube Algorithm
solveSlot _ _ [] = error "The given states cannot be empty"
solveSlot slot sideMove states =
    case validAlgorithm slot states of
        Nothing -> solveSlot slot sideMove (createNewStates sideMove states)
        Just (alg, _) -> applyAlgorithm alg

createNewStates :: Move -> [(Algorithm, CubeState)] -> [(Algorithm, CubeState)]
createNewStates sideMove (x:xs) = 
    if length xs > 20000
        then error $ "Too many states created: " ++ show (length xs) 
        else getStatesFromState sideMove x ++ createNewStates sideMove xs
createNewStates _ [] = []

getStatesFromState :: Move -> (Algorithm, CubeState) -> [(Algorithm, CubeState)]
getStatesFromState sideMove (alg, cubeState) = do
    let step1 = branchWithU (alg, cubeState)
    let step2 = map (\(currAlg, currState) -> 
            let (resultAlg, resultState) = runState (applyAlgorithm [sideMove]) currState 
            in (currAlg ++ resultAlg, resultState)) step1
    let step3 = foldr (\x acc -> branchWithU x ++ acc) [] step2
    let step4 = map (\(currAlg, currState) -> 
            let (resultAlg, resultState) = runState (applyAlgorithm [reverseMove sideMove]) currState 
            in (currAlg ++ resultAlg, resultState)) step3
    step4

branchWithU :: (Algorithm, CubeState) -> [(Algorithm, CubeState)]
branchWithU = branchWithMoves [Move U Normal, Move U Prime, Move U Two]

branchWithMoves :: [Move] -> (Algorithm, CubeState) -> [(Algorithm, CubeState)]
branchWithMoves (x:xs) (alg, cubeState) = 
    let (_, resultState) = runState (applyAlgorithm [x]) cubeState 
    in (alg ++ [x], resultState) : branchWithMoves xs (alg, cubeState)
branchWithMoves [] _ = []

validAlgorithm :: F2LSlot -> [(Algorithm, CubeState)] -> Maybe (Algorithm, CubeState)
validAlgorithm _ [] = Nothing
validAlgorithm slot ((alg, cubeState):xs) =
    if f2lSlot cubeState slot == f2lSlot solvedCube slot
        then Just (alg, cubeState)
        else validAlgorithm slot xs
