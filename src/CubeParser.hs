module CubeParser (parseScramble, parse, parseCubeState, parseMove) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import CubeState
import Cube
import CubeValidator

type Parser = Parsec Void T.Text

parseColor :: Parser CubeColor
parseColor =
        White  <$ char' 'W'
    <|> Yellow <$ char' 'Y'
    <|> Green  <$ char' 'G'
    <|> Blue   <$ char' 'B'
    <|> Red    <$ char' 'R'
    <|> Orange <$ char' 'O'

parseLine :: Int -> Parser [CubeColor]
parseLine lineLength = do
    space
    count lineLength parseColor

parseFace :: Int -> Parser [CubeColor]
parseFace lineLength = do
    res <- count 3 (parseLine lineLength)
    return $ concat res

parseCubeState :: Parser CubeState
parseCubeState = do
    [u1, u2, u3, u4, u5, u6, u7, u8, u9] <- parseFace 3
    [l1 ,l2 ,l3 ,f1 ,f2 ,f3 ,r1 ,r2 ,r3 ,b1 ,b2 ,b3,
     l4 ,l5 ,l6 ,f4 ,f5 ,f6 ,r4 ,r5 ,r6 ,b4 ,b5 ,b6,
     l7 ,l8 ,l9 ,f7 ,f8 ,f9 ,r7 ,r8 ,r9 ,b7 ,b8 ,b9] <- parseFace 12
    [d1, d2, d3, d4, d5, d6, d7, d8, d9] <- parseFace 3
    space
    eof

    let cubeState = CubeState {
          f = Center f5
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
    if validateCubeState cubeState
        then return cubeState
        else fail "Cube is not in a valid state"

parseScramble :: Parser Algorithm
parseScramble = do
    moves <-parseScrambleHelper [] <|> return []
    eof
    return moves

parseScrambleHelper :: [Rotation] -> Parser Algorithm
parseScrambleHelper rotations = do
    (maybeRotation, alg) <- parseNextMoveWithRotation
    space
    let rotatedAlg = map (`applyRotations` rotations) alg
    let prependedRotations = prependIfExist maybeRotation rotations
    rest <- parseScrambleHelper prependedRotations <|> return []
    return $ rotatedAlg ++ rest

prependIfExist :: Maybe a -> [a] -> [a]
prependIfExist (Just a) list = a : list
prependIfExist Nothing list = list

parseNextMoveWithRotation :: Parser (Maybe Rotation, Algorithm)
parseNextMoveWithRotation =
        parseMoveWithRotation
    <|> parseRotationWithAlg
    <|> parseWideMoveWithRotation
    <|> parseSliceMoveWithRotation

parseMoveWithRotation :: Parser (Maybe Rotation, Algorithm)
parseMoveWithRotation = do
    m <- parseMove
    return (Nothing, [m])

parseWideMoveWithRotation :: Parser (Maybe Rotation, Algorithm)
parseWideMoveWithRotation = do
    wideMove <- parseWideMove
    let (rotation, m) = rotationAndMoveFromWideMove wideMove
    return (Just rotation, [m])

parseWideMove :: Parser WideMove
parseWideMove = WideMove <$> parseWideTurningFace <*> parseDirection

parseWideTurningFace :: Parser MoveFace
parseWideTurningFace =
        FFace <$ char 'f'
    <|> RFace <$ char 'r'
    <|> UFace <$ char 'u'
    <|> BFace <$ char 'b'
    <|> LFace <$ char 'l'
    <|> DFace <$ char 'd'

parseSliceMoveWithRotation :: Parser (Maybe Rotation, Algorithm)
parseSliceMoveWithRotation = do
    sliceMove <- parseSliceMove
    let (rotation, alg) = rotationAndAlgFromSliceMove sliceMove
    return (Just rotation, alg)

parseSliceMove :: Parser SliceMove
parseSliceMove = SliceMove <$> parseSlice <*> parseDirection

parseSlice :: Parser Slice
parseSlice =
        M <$ char 'M'
    <|> E <$ char 'E'
    <|> S <$ char 'S'

parseRotationWithAlg :: Parser (Maybe Rotation, Algorithm)
parseRotationWithAlg = do
    rotation <- parseRotation
    return (Just rotation, [])

parseRotation :: Parser Rotation
parseRotation = Rotation <$> parseRotationDirection <*> parseDirection

parseRotationDirection :: Parser RotationDirection
parseRotationDirection =
        X <$ char 'x'
    <|> Y <$ char 'y'
    <|> Z <$ char 'z'

parseMove :: Parser Move
parseMove = Move <$> parseTurningFace <*> parseDirection

parseTurningFace :: Parser MoveFace
parseTurningFace =
        FFace <$ char 'F'
    <|> RFace <$ char 'R'
    <|> UFace <$ char 'U'
    <|> BFace <$ char 'B'
    <|> LFace <$ char 'L'
    <|> DFace <$ char 'D'

parseDirection :: Parser MoveDirection
parseDirection =
        Prime <$ char '\''
    <|> Two   <$ char '2'
    <|> return Normal
