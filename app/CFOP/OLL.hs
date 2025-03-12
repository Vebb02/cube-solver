module CFOP.OLL where

import Cube
import Triggers

oll :: Cube Algorithm
oll = do
    edgeFlipMoves <- flipEdges
    cornerSolveMoves <- solveCorners
    return $ edgeFlipMoves ++ cornerSolveMoves

flipEdges :: Cube Algorithm
flipEdges = undefined

solveCorners :: Cube Algorithm
solveCorners = undefined

dotFlip :: Algorithm
dotFlip = angleFlip ++ [Move U Normal] ++ lineFlip

lineFlip :: Algorithm
lineFlip = [Move F Normal] ++ sexy ++ [Move F Prime]

angleFlip :: Algorithm
angleFlip = reverseMoveSeq lineFlip

sune :: Algorithm
sune =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Two
    , Move R Prime
    ]

antisune :: Algorithm
antisune = reverseMoveSeq sune

hOll :: Algorithm
hOll =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Prime
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Two
    , Move R Prime
    ]

lOll :: Algorithm
lOll = 
    [ Move F Normal
    , Move R Prime
    , Move F Prime
    , Move L Normal
    , Move F Normal
    , Move R Normal
    , Move F Prime
    , Move L Prime
    ]

piOll :: Algorithm
piOll = 
    [ Move R Normal
    , Move U Two
    , Move R Two
    , Move U Prime
    , Move R Two
    , Move U Prime
    , Move R Two
    , Move U Two
    , Move R Normal
    ]

tOll :: Algorithm
tOll = reverseMoveSeq lOll

uOll :: Algorithm
uOll = 
    [ Move R Two
    , Move D Normal
    , Move R Prime
    , Move U Two
    , Move D Prime
    , Move R Prime
    , Move U Two
    , Move R Prime
    ]