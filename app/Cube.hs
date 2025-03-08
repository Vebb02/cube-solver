module Cube where

import Control.Monad.State

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

data Cube = Cube
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

instance Show Cube where 
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

              ++ "\n   " ++ showFirstC (dbl cube) ++ showFirstE (db cube) ++ showFirstC (drb cube)
              ++ "\n   " ++ showFirstE (dl cube)  ++ show       (d cube)  ++ showFirstE (dr cube)
              ++ "\n   " ++ showFirstC (dlf cube) ++ showFirstE (df cube) ++ showFirstC (dfr cube)


solvedCube :: Cube
solvedCube = Cube
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
