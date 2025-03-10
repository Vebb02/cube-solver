module CubeState where

data Color = White
           | Yellow
           | Green
           | Blue
           | Red
           | Orange
    deriving (Eq)

instance Show Color where
    show White = "W"
    show Yellow = "Y"
    show Green = "G"
    show Blue = "B"
    show Red = "R"
    show Orange = "O"

newtype Center = Center Color
    deriving (Eq)

instance Show Center where
    show (Center color) = show color

data Edge = Edge
    { firstE  :: Color
    , secondE :: Color
    }
    deriving (Eq, Show)

showFirstE :: Edge -> String
showFirstE edge = show $ firstE edge

showSecondE :: Edge -> String
showSecondE edge = show $ secondE edge

flipEdge :: Edge -> Edge
flipEdge (Edge c1 c2) = Edge c2 c1

data Corner = Corner 
    { firstC  :: Color
    , secondC :: Color
    , thirdC  :: Color
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
                   "    " ++ showFirstC (ulb cube) ++ showFirstE (ub cube) ++ showFirstC (ubr cube)
              ++ "\n    " ++ showFirstE (ul cube)  ++ show       (u cube)  ++ showFirstE (ur cube)
              ++ "\n    " ++ showFirstC (ufl cube) ++ showFirstE (uf cube) ++ showFirstC (urf cube)
              
              ++ "\n"
              ++ showSecondC (ulb cube) ++ showSecondE (ul cube) ++ showThirdC (ufl cube) ++ " "
              ++ showSecondC (ufl cube) ++ showSecondE (uf cube) ++ showThirdC (urf cube) ++ " "
              ++ showSecondC (urf cube) ++ showSecondE (ur cube) ++ showThirdC (ubr cube) ++ " "
              ++ showSecondC (ubr cube) ++ showSecondE (ub cube) ++ showThirdC (ulb cube)
              
              ++ "\n"
              ++ showSecondE (bl cube) ++ show (l cube) ++ showSecondE (fl cube) ++ " "
              ++ showFirstE  (fl cube) ++ show (f cube) ++ showFirstE  (fr cube) ++ " "
              ++ showSecondE (fr cube) ++ show (r cube) ++ showSecondE (br cube) ++ " "
              ++ showFirstE  (br cube) ++ show (b cube) ++ showFirstE  (bl cube)
              
              ++ "\n"
              ++ showThirdC (dbl cube) ++ showSecondE (dl cube) ++ showSecondC (dlf cube) ++ " "
              ++ showThirdC (dlf cube) ++ showSecondE (df cube) ++ showSecondC (dfr cube) ++ " "
              ++ showThirdC (dfr cube) ++ showSecondE (dr cube) ++ showSecondC (drb cube) ++ " "
              ++ showThirdC (drb cube) ++ showSecondE (db cube) ++ showSecondC (dbl cube)

              ++ "\n    " ++ showFirstC (dlf cube) ++ showFirstE (df cube) ++ showFirstC (dfr cube)
              ++ "\n    " ++ showFirstE (dl cube)  ++ show       (d cube)  ++ showFirstE (dr cube)
              ++ "\n    " ++ showFirstC (dbl cube) ++ showFirstE (db cube) ++ showFirstC (drb cube)


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


-- FIX: Corner and edge swap validation
validateCubeState :: CubeState -> Bool
validateCubeState cubeState = validatePieceQuantity cubeState
                           && validateCenters cubeState 
                           && validateEdgeOrientation cubeState
                           && validateCornerRotation cubeState

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

cubeEdges :: CubeState -> [Edge]
cubeEdges cs = [ uf cs
               , ur cs
               , ub cs
               , ul cs
               , df cs
               , dl cs
               , db cs
               , dr cs
               , fr cs
               , fl cs
               , br cs
               , bl cs
               ]

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
validatePieceQuantity cubeState = validateEdgeQuantity (cubeEdges cubeState) [] && validateCornerQuantity (cubeCorners cubeState) []

validateEdgeQuantity :: [Edge] -> [Edge] -> Bool
validateEdgeQuantity (x:xs) seen = edgeInList x (cubeEdges solvedCube) && not (edgeInList x seen) && validateEdgeQuantity xs (x:seen)
validateEdgeQuantity [] _ = True

edgeInList :: Edge -> [Edge] -> Bool
edgeInList edge (x:xs) = edge `edgeEquivalent` x || edgeInList edge xs
edgeInList _ [] = False

edgeEquivalent :: Edge -> Edge -> Bool
edgeEquivalent e1 e2 = e1 == e2 || e1 == flipEdge e2

validateCornerQuantity :: [Corner] -> [Corner] -> Bool
validateCornerQuantity (x:xs) seen = cornerInList x (cubeCorners solvedCube) && not (cornerInList x seen) && validateCornerQuantity xs (x:seen)
validateCornerQuantity [] _ = True

cornerInList :: Corner -> [Corner] -> Bool
cornerInList corner (x:xs) = corner `cornerEquivalent` x || cornerInList corner xs
cornerInList _ [] = False

cornerEquivalent :: Corner -> Corner -> Bool
cornerEquivalent c1 c2 = c1 == c2 || c1 == twistCorner c2 || c1 == twistCorner (twistCorner c2)