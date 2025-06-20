module CubeState where

data CubeColor = 
      White
    | Yellow
    | Green
    | Blue
    | Red
    | Orange
    deriving (Eq)

instance Show CubeColor where
    show White = "W"
    show Yellow = "Y"
    show Green = "G"
    show Blue = "B"
    show Red = "R"
    show Orange = "O"

newtype Center = Center CubeColor
    deriving (Eq)

instance Show Center where
    show (Center color) = show color

centerColor :: Center -> CubeColor
centerColor (Center c) = c

data Edge = Edge
    { firstE  :: CubeColor
    , secondE :: CubeColor
    }
    deriving (Eq, Show)

showFirstE :: Edge -> String
showFirstE edge = show $ firstE edge

showSecondE :: Edge -> String
showSecondE edge = show $ secondE edge

flipEdge :: Edge -> Edge
flipEdge (Edge c1 c2) = Edge c2 c1

data Corner = Corner
    { firstC  :: CubeColor
    , secondC :: CubeColor
    , thirdC  :: CubeColor
    }
    deriving (Eq, Show)

showFirstC :: Corner -> String
showFirstC corner = show $ firstC corner

showSecondC :: Corner -> String
showSecondC corner = show $ secondC corner

showThirdC :: Corner -> String
showThirdC corner = show $ thirdC corner

twistCorner :: Corner -> Corner
twistCorner (Corner c1 c2 c3) = Corner c3 c1 c2

data CubeState = CubeState
    { f   :: Center
    , r   :: Center
    , u   :: Center
    , b   :: Center
    , l   :: Center
    , d   :: Center
    , uf  :: Edge
    , ur  :: Edge
    , ub  :: Edge
    , ul  :: Edge
    , df  :: Edge
    , dl  :: Edge
    , db  :: Edge
    , dr  :: Edge
    , fr  :: Edge
    , fl  :: Edge
    , br  :: Edge
    , bl  :: Edge
    , urf :: Corner
    , ubr :: Corner
    , ulb :: Corner
    , ufl :: Corner
    , dfr :: Corner
    , dlf :: Corner
    , dbl :: Corner
    , drb :: Corner
    }
    deriving (Eq)

instance Show CubeState where
    show cube =
             "   " ++ showFirstC (ulb cube) ++ showFirstE (ub cube) ++ showFirstC (ubr cube)
        ++ "\n   " ++ showFirstE (ul cube)  ++ show       (u cube)  ++ showFirstE (ur cube)
        ++ "\n   " ++ showFirstC (ufl cube) ++ showFirstE (uf cube) ++ showFirstC (urf cube)

        ++ "\n"
        ++ showSecondC (ulb cube) ++ showSecondE (ul cube) ++ showThirdC (ufl cube)
        ++ showSecondC (ufl cube) ++ showSecondE (uf cube) ++ showThirdC (urf cube)
        ++ showSecondC (urf cube) ++ showSecondE (ur cube) ++ showThirdC (ubr cube)
        ++ showSecondC (ubr cube) ++ showSecondE (ub cube) ++ showThirdC (ulb cube)

        ++ "\n"
        ++ showSecondE (bl cube) ++ show (l cube) ++ showSecondE (fl cube)
        ++ showFirstE  (fl cube) ++ show (f cube) ++ showFirstE  (fr cube)
        ++ showSecondE (fr cube) ++ show (r cube) ++ showSecondE (br cube)
        ++ showFirstE  (br cube) ++ show (b cube) ++ showFirstE  (bl cube)

        ++ "\n"
        ++ showThirdC (dbl cube) ++ showSecondE (dl cube) ++ showSecondC (dlf cube)
        ++ showThirdC (dlf cube) ++ showSecondE (df cube) ++ showSecondC (dfr cube)
        ++ showThirdC (dfr cube) ++ showSecondE (dr cube) ++ showSecondC (drb cube)
        ++ showThirdC (drb cube) ++ showSecondE (db cube) ++ showSecondC (dbl cube)

        ++ "\n   " ++ showFirstC (dlf cube) ++ showFirstE (df cube) ++ showFirstC (dfr cube)
        ++ "\n   " ++ showFirstE (dl cube)  ++ show       (d cube)  ++ showFirstE (dr cube)
        ++ "\n   " ++ showFirstC (dbl cube) ++ showFirstE (db cube) ++ showFirstC (drb cube)

solvedCube :: CubeState
solvedCube = CubeState
    { f = Center Green
    , r = Center Red
    , u = Center White
    , b = Center Blue
    , l = Center Orange
    , d = Center Yellow
    , uf = Edge White Green
    , ur = Edge White Red
    , ub = Edge White Blue
    , ul = Edge White Orange
    , df = Edge Yellow Green
    , dl = Edge Yellow Orange
    , db = Edge Yellow Blue
    , dr = Edge Yellow Red
    , fr = Edge Green Red
    , fl = Edge Green Orange
    , br = Edge Blue Red
    , bl = Edge Blue Orange
    , urf = Corner White Red Green
    , ubr = Corner White Blue Red
    , ulb = Corner White Orange Blue
    , ufl = Corner White Green Orange
    , dfr = Corner Yellow Green Red
    , dlf = Corner Yellow Orange Green
    , dbl = Corner Yellow Blue Orange
    , drb = Corner Yellow Red Blue
    }

cubeSolved :: CubeState -> Bool
cubeSolved = (solvedCube ==) 

class Piece a where
    pieceOrientations :: a -> [a]
    pieceEquivalent :: a -> a -> Bool
    pieceInList :: a -> [a] -> Bool
    pieceInList edge = any (pieceEquivalent edge)
    getPieces :: [CubeState -> a]
    pieces :: CubeState -> [a]
    pieces cubeState = map (\x -> x cubeState) getPieces
    findPieceOnCube :: CubeState -> a -> (CubeState -> a)
    findPieceOnCube cubeState piece = searchForPiece getPieces where
        searchForPiece [] = error "Could not find piece"
        searchForPiece (x:xs) =
            if x cubeState `pieceEquivalent` piece
                then x
                else searchForPiece xs
    pieceSum :: a -> Int
    totalPieceSum :: [a] -> Int
    totalPieceSum = foldr ((+) . pieceSum) 0

instance Piece Edge where
    pieceOrientations e = [e, flipEdge e]
    pieceEquivalent e1 e2 = e1 `elem` pieceOrientations e2
    getPieces =
        [ uf
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
    pieceSum (Edge White _) = 0
    pieceSum (Edge Yellow _) = 0
    pieceSum (Edge _ White) = 1
    pieceSum (Edge _ Yellow) = 1
    pieceSum (Edge Green _) = 0
    pieceSum (Edge Blue _) = 0
    pieceSum (Edge _ Green) = 1
    pieceSum (Edge _ Blue) = 1
    pieceSum _ = error "Invalid edge on cube"

instance Piece Corner where
    pieceOrientations c = [c, twistCorner c, twistCorner $ twistCorner c]
    pieceEquivalent c1 c2 = c1 `elem` pieceOrientations c2
    getPieces =
        [ urf
        , ubr
        , ulb
        , ufl
        , dfr
        , dlf
        , dbl
        , drb
        ]
    pieceSum (Corner Yellow _ _) = 0
    pieceSum (Corner White _ _) = 0
    pieceSum (Corner _ Yellow  _) = 1
    pieceSum (Corner _ White _) = 1
    pieceSum (Corner _ _ Yellow) = 2
    pieceSum (Corner _ _ White) = 2
    pieceSum _ = error "Invalid corner on cube"
