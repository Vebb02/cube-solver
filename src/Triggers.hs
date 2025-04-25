module Triggers where

import Cube

-- This is a totally legit speedcubing term, I promise
sexy :: Algorithm
sexy =
    map (uncurry Move)
    [ (R, Normal)
    , (U, Normal)
    , (R, Prime)
    , (U, Prime)
    ]

reverseSexy :: Algorithm
reverseSexy = reverseMoveSeq sexy

sledgeHammer :: Algorithm
sledgeHammer =
    map (uncurry Move)
    [ (R, Prime)
    , (F, Normal)
    , (R, Normal)
    , (F, Prime)
    ]

hedgeSlammer :: Algorithm
hedgeSlammer = reverseMoveSeq sledgeHammer
