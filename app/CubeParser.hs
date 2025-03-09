module CubeParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import CubeState

type Parser = Parsec Void T.Text

parseColor :: Parser Char
parseColor = char 'W' <|> char 'Y' <|> char 'G' <|> char 'B' <|> char 'R' <|> char 'O'

charToColor :: Char -> Maybe Color
charToColor 'W' = Just White
charToColor 'Y' = Just Yellow
charToColor 'G' = Just Green
charToColor 'B' = Just Blue
charToColor 'R' = Just Red
charToColor 'O' = Just Orange
charToColor _ = Nothing


runTest :: T.Text -> IO ()
runTest = parseTest parseColor