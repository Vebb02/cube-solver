module Triggers where

import Cube

-- This is a totally legit speedcubing term, I promise
sexy :: Algorithm
sexy = [R, U, R', U']

reverseSexy :: Algorithm
reverseSexy = reverseMoveSeq sexy

sledgeHammer :: Algorithm
sledgeHammer = [R', F, R, F']

hedgeSlammer :: Algorithm
hedgeSlammer = reverseMoveSeq sledgeHammer
