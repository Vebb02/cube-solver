module Triggers where

import Cube

-- This is a totally legit speed cubing term, I promise
sexy :: [Move]
sexy =
	[ Move R Normal
    , Move U Normal
    , Move R Prime
	, Move U Prime
	]

reverseSexy :: [Move]
reverseSexy = reverseMoveSeq sexy

sledgeHammer :: [Move]
sledgeHammer =
	[ Move R Prime
	, Move F Normal
	, Move R Normal
	, Move F Prime
	]

hedgeSlammer :: [Move]
hedgeSlammer = reverseMoveSeq sledgeHammer