module CubeParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import CubeState

type Parser = Parsec Void T.Text

parseColor :: Parser Char
parseColor = char 'W' <|> char 'Y' <|> char 'G' <|> char 'B' <|> char 'R' <|> char 'O'

parseCubeState :: Parser CubeState
parseCubeState = do
    space
    u1 <- parseColor
    u2 <- parseColor
    u3 <- parseColor
    
    _ <- newline
    space
    u4 <- parseColor
    u5 <- parseColor
    u6 <- parseColor
    
    _ <- newline
    space
    u7 <- parseColor
    u8 <- parseColor
    u9 <- parseColor
    
    _ <- newline
    l1 <- parseColor
    l2 <- parseColor
    l3 <- parseColor
    f1 <- parseColor
    f2 <- parseColor
    f3 <- parseColor
    r1 <- parseColor
    r2 <- parseColor
    r3 <- parseColor
    b1 <- parseColor
    b2 <- parseColor
    b3 <- parseColor

    _ <- newline
    l4 <- parseColor
    l5 <- parseColor
    l6 <- parseColor
    f4 <- parseColor
    f5 <- parseColor
    f6 <- parseColor
    r4 <- parseColor
    r5 <- parseColor
    r6 <- parseColor
    b4 <- parseColor
    b5 <- parseColor
    b6 <- parseColor
    
    _ <- newline
    l7 <- parseColor
    l8 <- parseColor
    l9 <- parseColor
    f7 <- parseColor
    f8 <- parseColor
    f9 <- parseColor
    r7 <- parseColor
    r8 <- parseColor
    r9 <- parseColor
    b7 <- parseColor
    b8 <- parseColor
    b9 <- parseColor
    
    _ <- newline
    space
    d1 <- parseColor
    d2 <- parseColor
    d3 <- parseColor
    _ <- newline
    space
    d4 <- parseColor
    d5 <- parseColor
    d6 <- parseColor
    _ <- newline
    space
    d7 <- parseColor
    d8 <- parseColor
    d9 <- parseColor
    _ <- newline
    eof

    return CubeState
        { f = Center $ charToColor f5
        , r = Center $ charToColor r5
        , u = Center $ charToColor u5
        , b = Center $ charToColor b5
        , l = Center $ charToColor l5
        , d = Center $ charToColor d5
        , uf = charsToEdge u8 f2
        , ur = charsToEdge u6 r2
        , ub = charsToEdge u2 b2
        , ul = charsToEdge u4 l2
        , df = charsToEdge d2 f8
        , dl = charsToEdge d4 l8
        , db = charsToEdge d8 b8
        , dr = charsToEdge d6 r8
        , fr = charsToEdge f6 r4
        , fl = charsToEdge f4 l6
        , br = charsToEdge b4 r6
        , bl = charsToEdge b6 l4
        , urf = charsToCorner u9 r1 f3
        , ubr = charsToCorner u3 b1 r3
        , ulb = charsToCorner u1 l1 b3
        , ufl = charsToCorner u7 f1 l3
        , dfr = charsToCorner d3 f9 r7
        , dlf = charsToCorner d1 l9 f7
        , dbl = charsToCorner d7 b9 l7
        , drb = charsToCorner d9 r9 b7
        }

charsToEdge :: Char -> Char -> Edge
charsToEdge c1 c2 = Edge (charToColor c1) (charToColor c2)

charsToCorner :: Char -> Char -> Char -> Corner
charsToCorner c1 c2 c3 = Corner (charToColor c1) (charToColor c2) (charToColor c3)

charToColor :: Char -> Color
charToColor 'W' = White
charToColor 'Y' = Yellow
charToColor 'G' = Green
charToColor 'B' = Blue
charToColor 'R' = Red
charToColor 'O' = Orange
charToColor _ = undefined


runTest :: T.Text -> IO ()
runTest = parseTest parseCubeState