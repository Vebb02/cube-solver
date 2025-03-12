module CFOP.OLL where

import Cube
import Triggers

oll :: Cube [Move]
oll = do
    edgeFlipMoves <- flipEdges
    cornerSolveMoves <- solveCorners
    return $ edgeFlipMoves ++ cornerSolveMoves

flipEdges :: Cube [Move]
flipEdges = undefined

solveCorners :: Cube [Move]
solveCorners = undefined

dotFlip :: [Move]
dotFlip = angleFlip ++ [Move U Normal] ++ lineFlip

lineFlip :: [Move]
lineFlip = [Move F Normal] ++ sexy ++ [Move F Prime]

angleFlip :: [Move]
angleFlip = reverseMoveSeq lineFlip

sune :: [Move]
sune =
    [ Move R Normal
    , Move U Normal
    , Move R Prime
    , Move U Normal
    , Move R Normal
    , Move U Two
    , Move R Prime
    ]

antisune :: [Move]
antisune = reverseMoveSeq sune

hOll :: [Move]
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

lOll :: [Move]
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

piOll :: [Move]
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

tOll :: [Move]
tOll = reverseMoveSeq lOll

uOll :: [Move]
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