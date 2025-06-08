{-# Language DeriveDataTypeable #-}
module Cube where

import CubeState

import Control.Monad.State
import Data.List (minimumBy)
import Data.Data

type Cube a = State CubeState a

data MoveDirection = Normal | Prime | Two
    deriving (Eq, Show, Data, Typeable)

data MoveFace = 
      FFace
    | RFace
    | UFace
    | BFace
    | LFace
    | DFace
    deriving (Eq, Data, Typeable)

instance Show MoveFace where
    show FFace = "F"
    show RFace = "R"
    show UFace = "U"
    show BFace = "B"
    show LFace = "L"
    show DFace = "D"

data Move = Move MoveFace MoveDirection
    deriving (Eq, Data, Typeable)

type Algorithm = [Move]

data RotationDirection = X | Y | Z
    deriving (Eq, Show)

data Rotation = Rotation RotationDirection MoveDirection
    deriving (Eq, Show)

data Slice = M | E | S
    deriving (Eq, Show)

data SliceMove = SliceMove Slice MoveDirection
    deriving (Eq, Show)

data WideMove = WideMove MoveFace MoveDirection
    deriving (Eq, Show)

showAlg :: Algorithm -> String
showAlg [] = ""
showAlg [x] = show x
showAlg (x:xs) = show x ++ " " ++ showAlg xs

instance Show Move where
    show (Move face Normal) = show face
    show (Move face Prime) = show face ++ "'"
    show (Move face Two) = show face ++ "2"


move :: Move -> Cube ()
move (Move FFace Normal) = 
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
move (Move RFace Normal) = 
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
move (Move UFace Normal) = 
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
move (Move BFace Normal) = 
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
move (Move LFace Normal) = 
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
move (Move DFace Normal) = 
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

applyAlgorithm :: Algorithm -> Cube Algorithm
applyAlgorithm [] = return []
applyAlgorithm (x:xs) = do
    move x
    result <- applyAlgorithm xs
    return (x : result)

undoMove :: Move -> Cube ()
undoMove = move . reverseMove

reverseMove :: Move -> Move
reverseMove (Move face md) = Move face (reverseMoveDirection md)

reverseMoveDirection :: MoveDirection -> MoveDirection
reverseMoveDirection Normal = Prime
reverseMoveDirection Prime = Normal
reverseMoveDirection Two = Two


reverseMoveSeq :: Algorithm -> Algorithm
reverseMoveSeq = reverse . map reverseMove

aufMoves :: Algorithm
aufMoves = map (Move UFace) [Normal, Prime, Two]

applyFourSidesAlg :: [Algorithm] -> [Algorithm]
applyFourSidesAlg = foldr (\x acc -> fourSidesAlg x ++ acc) []

fourSidesAlg :: Algorithm -> [Algorithm]
fourSidesAlg alg = 
    let onceRotated   = rotateAlg alg
        twiceRotated  = rotateAlg onceRotated
        thriceRotated = rotateAlg twiceRotated
    in 
        [ alg
        , onceRotated
        , twiceRotated
        , thriceRotated
        ]

rotateAlg :: Algorithm -> Algorithm
rotateAlg = map rotateFace

rotateFace :: Move -> Move
rotateFace (Move face dir) = Move (newFace face) dir where
    newFace :: MoveFace -> MoveFace
    newFace FFace = RFace
    newFace RFace = BFace
    newFace BFace = LFace
    newFace LFace = FFace
    newFace xFace = xFace

tryAlg :: [Algorithm] -> CubeState -> (CubeState -> Bool) -> Either String Algorithm
tryAlg [] cubeState _  = Left $ "Could not solve\n" ++ show cubeState
tryAlg (x:xs) cubeState stateCondition = do
    let newState = execState (applyAlgorithm x) cubeState
    if stateCondition newState
    then return x
    else tryAlg xs cubeState stateCondition

removeCancellingMoves :: Algorithm -> Algorithm -> Algorithm
removeCancellingMoves [] seen = reverse seen
removeCancellingMoves (x:xs) (y:ys) = removeCancellingMoves xs (combineMoves x y ++ ys)
removeCancellingMoves (x:xs) [] = removeCancellingMoves xs [x]

combineMoveDirection :: MoveDirection -> MoveDirection -> Maybe MoveDirection
combineMoveDirection m1 m2 = moveSumToMoveDirection $ moveDirVal m1 + moveDirVal m2

moveDirVal :: MoveDirection -> Int
moveDirVal Normal = 1
moveDirVal Prime = -1
moveDirVal Two = 2

moveSumToMoveDirection :: Int -> Maybe MoveDirection
moveSumToMoveDirection moveSum = case (moveSum + 4) `mod` 4 of
    0 -> Nothing
    1 -> Just Normal
    2 -> Just Two
    3 -> Just Prime
    x -> error $ "Invalid move sum " ++ " " ++ show x

combineMoves :: Move -> Move -> Algorithm
combineMoves m0@(Move face0 dir0) m1@(Move face1 dir1) = 
    if face0 /= face1 
        then [m0, m1]
        else case dir0 `combineMoveDirection` dir1 of
            Nothing -> []
            Just dir -> [Move face0 dir]

bestPossibleSolution :: CubeState -> ([a] -> Cube Algorithm) -> [[a]] -> Algorithm
bestPossibleSolution cubeState method orderings = 
    minimumBy (\x y -> if length x < length y then LT else GT) 
        $ map (\x -> evalState (method x) cubeState) orderings

applyRotations :: Move -> [Rotation] -> Move
applyRotations = foldr applyRotation

applyRotation :: Rotation -> Move -> Move
applyRotation (Rotation X Normal) (Move FFace dir) = Move DFace dir
applyRotation (Rotation X Normal) (Move RFace dir) = Move RFace dir
applyRotation (Rotation X Normal) (Move UFace dir) = Move FFace dir
applyRotation (Rotation X Normal) (Move BFace dir) = Move UFace dir
applyRotation (Rotation X Normal) (Move LFace dir) = Move LFace dir
applyRotation (Rotation X Normal) (Move DFace dir) = Move BFace dir
applyRotation (Rotation Y Normal) (Move FFace dir) = Move RFace dir
applyRotation (Rotation Y Normal) (Move RFace dir) = Move BFace dir
applyRotation (Rotation Y Normal) (Move UFace dir) = Move UFace dir
applyRotation (Rotation Y Normal) (Move BFace dir) = Move LFace dir
applyRotation (Rotation Y Normal) (Move LFace dir) = Move FFace dir
applyRotation (Rotation Y Normal) (Move DFace dir) = Move DFace dir
applyRotation (Rotation Z Normal) (Move FFace dir) = Move FFace dir
applyRotation (Rotation Z Normal) (Move RFace dir) = Move UFace dir
applyRotation (Rotation Z Normal) (Move UFace dir) = Move LFace dir
applyRotation (Rotation Z Normal) (Move BFace dir) = Move BFace dir
applyRotation (Rotation Z Normal) (Move LFace dir) = Move DFace dir
applyRotation (Rotation Z Normal) (Move DFace dir) = Move RFace dir
applyRotation (Rotation rotationDir Prime) m = applyRotation (Rotation rotationDir Normal) $ applyRotation (Rotation rotationDir Normal) $ applyRotation (Rotation rotationDir Normal) m
applyRotation (Rotation rotationDir Two) m = applyRotation (Rotation rotationDir Normal) $ applyRotation (Rotation rotationDir Normal) m

rotationAndAlgFromSliceMove :: SliceMove -> (Rotation, Algorithm)
rotationAndAlgFromSliceMove (SliceMove M dir) =
    (Rotation X (reverseMoveDirection dir), [Move RFace dir, Move LFace (reverseMoveDirection dir)])
rotationAndAlgFromSliceMove (SliceMove E dir) =
    (Rotation Y (reverseMoveDirection dir), [Move UFace dir, Move DFace (reverseMoveDirection dir)])
rotationAndAlgFromSliceMove (SliceMove S dir) =
    (Rotation Z (reverseMoveDirection dir), [Move FFace dir, Move BFace (reverseMoveDirection dir)])

rotationAndMoveFromWideMove :: WideMove -> (Rotation, Move)
rotationAndMoveFromWideMove (WideMove FFace dir) = (Rotation Z dir, Move BFace dir)
rotationAndMoveFromWideMove (WideMove RFace dir) = (Rotation X dir, Move LFace dir)
rotationAndMoveFromWideMove (WideMove UFace dir) = (Rotation Y dir, Move DFace dir)
rotationAndMoveFromWideMove (WideMove BFace dir) = (Rotation Z (reverseMoveDirection dir), Move FFace dir)
rotationAndMoveFromWideMove (WideMove LFace dir) = (Rotation X (reverseMoveDirection dir), Move RFace dir)
rotationAndMoveFromWideMove (WideMove DFace dir) = (Rotation Y (reverseMoveDirection dir), Move UFace dir)
