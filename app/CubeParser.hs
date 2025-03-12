module CubeParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import CubeState
import Cube

type Parser = Parsec Void T.Text

parseColor :: Parser Color
parseColor =
        White <$ char 'W'
    <|> Yellow <$ char 'Y'
    <|> Green <$ char 'G'
    <|> Blue <$ char 'B'
    <|> Red <$ char 'R'
    <|> Orange <$ char 'O'

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
        { f = Center f5
        , r = Center r5
        , u = Center u5
        , b = Center b5
        , l = Center l5
        , d = Center d5
        , uf = Edge u8 f2
        , ur = Edge u6 r2
        , ub = Edge u2 b2
        , ul = Edge u4 l2
        , df = Edge d2 f8
        , dl = Edge d4 l8
        , db = Edge d8 b8
        , dr = Edge d6 r8
        , fr = Edge f6 r4
        , fl = Edge f4 l6
        , br = Edge b4 r6
        , bl = Edge b6 l4
        , urf = Corner u9 r1 f3
        , ubr = Corner u3 b1 r3
        , ulb = Corner u1 l1 b3
        , ufl = Corner u7 f1 l3
        , dfr = Corner d3 f9 r7
        , dlf = Corner d1 l9 f7
        , dbl = Corner d7 b9 l7
        , drb = Corner d9 r9 b7
        }

parseScramble :: Parser Algorithm
parseScramble = do
    firstMove <- parseMove
    restMoves <- many parseNextMove
    eof
    return (firstMove : restMoves)

parseMove :: Parser Move
parseMove = Move <$> parseFace <*> parseDirection

parseNextMove :: Parser Move
parseNextMove = space *> parseMove

parseFace :: Parser MoveFace
parseFace =
        F <$ char 'F'
    <|> R <$ char 'R'
    <|> U <$ char 'U'
    <|> B <$ char 'B'
    <|> L <$ char 'L'
    <|> D <$ char 'D'

parseDirection :: Parser MoveDirection
parseDirection =
        Prime <$ char '\''
    <|> Two   <$ char '2'
    <|> return Normal

runTest :: T.Text -> IO ()
runTest = parseTest parseCubeState