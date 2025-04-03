module Cube where

import CubeState

import Control.Monad.State
import Data.List (minimumBy)

type Cube a = State CubeState a

data MoveDirection = Normal | Prime | Two
    deriving (Eq, Show)

combineMoveDirection :: MoveDirection -> MoveDirection -> Maybe MoveDirection
combineMoveDirection Normal Prime = Nothing
combineMoveDirection Prime Normal = Nothing
combineMoveDirection Two Two = Nothing
combineMoveDirection Normal Normal = Just Two
combineMoveDirection Prime Prime = Just Two
combineMoveDirection Normal Two = Just Prime
combineMoveDirection Two Normal = Just Prime
combineMoveDirection Prime Two = Just Normal
combineMoveDirection Two Prime = Just Normal


data MoveFace = F
          | R
          | U
          | B
          | L
          | D
    deriving (Eq, Show)

data Move = Move MoveFace MoveDirection
    deriving (Eq)

type Algorithm = [Move]

instance Show Move where
    show (Move face Normal) = show face
    show (Move face Prime) = show face ++ "'"
    show (Move face Two) = show face ++ "2"


move :: Move -> Cube ()
move (Move F Normal) = 
    modify (\x -> x 
    { uf = flipEdge $ fl x
    , fr = flipEdge $ uf x
    , df = flipEdge $ fr x
    , fl = flipEdge $ df x
    , urf = twistCorner $ ufl x
    , dfr = twistCorner $ twistCorner $ urf x
    , dlf = twistCorner $ dfr x
    , ufl = twistCorner $ twistCorner $ dlf x
    })
move (Move R Normal) = 
    modify (\x -> x
    { ur = fr x
    , br = ur x
    , dr = br x
    , fr = dr x
    , ubr = twistCorner $ urf x
    , drb = twistCorner $ twistCorner $ ubr x
    , dfr = twistCorner $ drb x
    , urf = twistCorner $ twistCorner $ dfr x
    })
move (Move U Normal) = 
    modify (\x -> x
    { uf = ur x
    , ur = ub x
    , ub = ul x
    , ul = uf x
    , urf = ubr x
    , ubr = ulb x
    , ulb = ufl x
    , ufl = urf x
    })
move (Move B Normal) = 
    modify (\x -> x
    { ub = flipEdge $ br x
    , br = flipEdge $ db x
    , db = flipEdge $ bl x
    , bl = flipEdge $ ub x
    , ulb = twistCorner $ ubr x
    , dbl = twistCorner $ twistCorner $ ulb x
    , drb = twistCorner $ dbl x
    , ubr = twistCorner $ twistCorner $ drb x
    })
move (Move L Normal) = 
    modify (\x -> x 
    { ul = bl x
    , fl = ul x
    , dl = fl x
    , bl = dl x
    , ufl = twistCorner $ ulb x
    , dlf = twistCorner $ twistCorner $ ufl x
    , dbl = twistCorner $ dlf x
    , ulb = twistCorner $ twistCorner $ dbl x
    })
move (Move D Normal) = 
    modify (\x -> x 
    { df = dl x
    , dl = db x
    , db = dr x
    , dr = df x
    , dlf = dbl x
    , dbl = drb x
    , drb = dfr x
    , dfr = dlf x
    })
move (Move x Prime) = do
    move (Move x Normal)
    move (Move x Normal)
    move (Move x Normal)
move (Move x Two) = do
    move (Move x Normal)
    move (Move x Normal)

showCube :: Cube () -> CubeState -> String
showCube c initialC = show $ execState c initialC

applyAlgorithm :: Algorithm -> Cube Algorithm
applyAlgorithm [] = return []
applyAlgorithm (x:xs) = do
    move x
    result <- applyAlgorithm xs
    return (x : result)

undoMove :: Move -> Cube ()
undoMove x = move $ reverseMove x

reverseMove :: Move -> Move
reverseMove (Move face Normal) = Move face Prime
reverseMove (Move face Prime) = Move face Normal
reverseMove (Move face Two) = Move face Two

reverseMoveSeq :: Algorithm -> Algorithm
reverseMoveSeq moves = reverse $ map reverseMove moves

aufMoves :: Algorithm
aufMoves = [Move U Two, Move U Prime, Move U Normal]

prependAuf :: [Algorithm] -> [Algorithm]
prependAuf = prependMoves aufMoves

prependMoves :: [Move] -> [Algorithm] -> [Algorithm]
prependMoves (x:xs) algorithms = prependMoves xs algorithms ++ map (x:) algorithms
prependMoves [] algorithms = algorithms

tryAlg :: [Algorithm] -> (CubeState -> Bool) -> Cube Algorithm
tryAlg [] _ = do
    cubeState <- get
    error $ "\n" ++ show cubeState
tryAlg (x:xs) stateCondition = do
    _ <- applyAlgorithm x
    cubeState <- get
    if stateCondition cubeState
    then return x
    else do
        _ <- applyAlgorithm (reverseMoveSeq x)
        tryAlg xs stateCondition

removeCancellingMoves :: Algorithm -> Algorithm -> Algorithm
removeCancellingMoves [] seen = reverse seen
removeCancellingMoves (x:xs) (y:ys) = removeCancellingMoves xs (combineMoves x y ++ ys)
removeCancellingMoves (x:xs) [] = removeCancellingMoves xs [x]

combineMoves :: Move -> Move -> Algorithm
combineMoves m0@(Move face0 dir0) m1@(Move face1 dir1) = 
    if face0 /= face1 
        then [m0, m1]
        else let newDir = dir0 `combineMoveDirection` dir1 in
            case newDir of
                Nothing -> []
                Just dir -> [Move face0 dir]

bestPossibleSolution :: CubeState -> ([a] -> Cube Algorithm) -> [[a]] -> Algorithm
bestPossibleSolution cubeState method orderings = 
    minimumBy (\x y -> if length x < length y then LT else GT) 
        $ map (\x -> evalState (method x) cubeState) orderings
