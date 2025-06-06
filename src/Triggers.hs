{-# LANGUAGE QuasiQuotes #-}
module Triggers where

import Cube
import AlgExpr

-- This is a totally legit speedcubing term, I promise
sexy :: Algorithm
sexy = [algExpr|R U R' U'|]

reverseSexy :: Algorithm
reverseSexy = reverseMoveSeq sexy

sledgeHammer :: Algorithm
sledgeHammer = [algExpr|R' F R F'|]

hedgeSlammer :: Algorithm
hedgeSlammer = reverseMoveSeq sledgeHammer
