module Cube where

import CubeState

import Control.Monad.State

type Cube a = State CubeState a

data MoveDirection = Normal | Prime | Two

data Move = F
          | R
          | U
          | B
          | L
          | D

move :: Move -> MoveDirection -> Cube ()
move F Normal = modify (\x -> x 
                         { uf = flipEdge $ fl x
                         , fr = flipEdge $ uf x
                         , df = flipEdge $ fr x
                         , fl = flipEdge $ df x
                         , urf = twistCorner $ ufl x
                         , dfr = twistCorner $ twistCorner $ urf x
                         , dlf = twistCorner $ dfr x
                         , ufl = twistCorner $ twistCorner $ dlf x
                        })
move R Normal = modify (\x -> x
                         { ur = fr x
                         , br = ur x
                         , dr = br x
                         , fr = dr x
                         , ubr = twistCorner $ urf x
                         , drb = twistCorner $ twistCorner $ ubr x
                         , dfr = twistCorner $ drb x
                         , urf = twistCorner $ twistCorner $ dfr x
                        })
move U Normal = modify (\x -> x
                         { uf = ur x
                         , ur = ub x
                         , ub = ul x
                         , ul = uf x
                         , urf = ubr x
                         , ubr = ulb x
                         , ulb = ufl x
                         , ufl = urf x
                        })
move B Normal = modify (\x -> x
                         { ub = flipEdge $ br x
                         , br = flipEdge $ db x
                         , db = flipEdge $ bl x
                         , bl = flipEdge $ ub x
                         , ulb = twistCorner $ ubr x
                         , dbl = twistCorner $ twistCorner $ ulb x
                         , drb = twistCorner $ dbl x
                         , ubr = twistCorner $ twistCorner $ drb x
                        })
move L Normal = modify (\x -> x 
                         { ul = bl x
                         , fl = ul x
                         , dl = fl x
                         , bl = dl x
                         , ufl = twistCorner $ ulb x
                         , dlf = twistCorner $ twistCorner $ ufl x
                         , dbl = twistCorner $ dlf x
                         , ulb = twistCorner $ twistCorner $ dbl x
                        })
move D Normal = modify (\x -> x 
                         { df = dl x
                         , dl = db x
                         , db = dr x
                         , dr = df x
                         , dlf = dbl x
                         , dbl = drb x
                         , drb = dfr x
                         , dfr = dlf x
                        })
move x Prime = do
    move x Normal
    move x Normal
    move x Normal
move x Two = do
    move x Normal
    move x Normal

sune :: Cube ()
sune = do
    move R Normal
    move U Normal
    move R Prime
    move U Normal
    move R Normal
    move U Two
    move R Prime

tPerm :: Cube ()
tPerm = do
    move R Normal
    move U Normal
    move R Prime
    move U Prime
    move R Prime
    move F Normal
    move R Two
    move U Prime
    move R Prime
    move U Prime
    move R Normal
    move U Normal
    move R Prime
    move F Prime

showCube :: Cube () -> CubeState -> String
showCube c initialC = show $ execState c initialC   