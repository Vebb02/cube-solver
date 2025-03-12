module Triggers where

import Cube

-- This is a totally legit speed cubing term, I promise
sexy :: Algorithm
sexy =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Prime
    ]

reverseSexy :: Algorithm
reverseSexy = reverseMoveSeq sexy

sledgeHammer :: Algorithm
sledgeHammer =
    [ Move R Prime
    , Move F Normal
    , Move R Normal
    , Move F Prime
    ]

hedgeSlammer :: Algorithm
hedgeSlammer = reverseMoveSeq sledgeHammer