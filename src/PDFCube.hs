{-# LANGUAGE OverloadedStrings #-}
module PDFCube (generatePDF, generatePDFSolution) where

import Graphics.PDF.Colors hiding (blue)
import Graphics.PDF hiding (blue)
import CubeState
import Cube
import Control.Monad.State
import qualified Data.Text as T
import Text.Megaparsec (runParser)
import CubeParser (parseCubeState)
import CFOP.CFOP (cfop)

generatePDF :: IO ()
generatePDF = do
    inputText <- readFile "scramble.in"
    let parsedResult = runParser parseCubeState "" (T.pack inputText)
    case parsedResult of
        Left errorMessage -> print errorMessage
        Right cubeState -> do
            generatePDFSolution (evalState cfop cubeState) cubeState
            putStrLn "PDF solution successfully generated!"

generatePDFSolution :: Algorithm -> CubeState -> IO ()
generatePDFSolution alg cubeState = do
    eitherPdf <- generateDoc alg cubeState
    case eitherPdf of
        Left errorMessage -> error errorMessage
        Right pdf -> runPdf "solution_manual.pdf" standardDocInfo rect pdf


generateDoc :: Algorithm -> CubeState -> IO (Either String (PDF ()))
generateDoc alg cubeState = do
    eitherFont <- showLeft <$> mkStdFont Helvetica_Bold
    eitherFrontPageImage <- readJpegFile "images/front-page.jpeg"
    eitherWarningImage <- readJpegFile "images/warning-page.jpeg"
    return $ do
        anyFont <- eitherFont
        frontPageImage <- eitherFrontPageImage
        warningImage <- eitherWarningImage
        let introductionPages = imagePage frontPageImage >> imagePage warningImage
        let font = PDFFont anyFont 50
        let size = 50
        let heightChange = size / 5 * 2
        let instructions = evalState (createPdfCubeSolution alg 1 font size heightChange) cubeState
        let pdf = introductionPages >> instructions
        return pdf

showLeft :: (Show a ) => Either a AnyFont -> Either String AnyFont
showLeft (Right a) = Right a
showLeft (Left a) = Left $ show a 

imagePage :: JpegFile -> PDF ()
imagePage frontPageImage = do
    page <- addPage Nothing
    image <- createPDFJpeg frontPageImage
    drawWithPage page (drawXObject image)

cubeStart :: PDFFloat
cubeStart = 300

transformComplex :: PDFFloat -> PDFFloat -> Point -> Point
transformComplex dx dy (x :+ y) = (x + dx) :+ (y + dy)

drawCube :: CubeState -> PDFFloat -> PDFFloat -> Draw ()
drawCube cubeState size heightChange = do
    drawLeftSide cubeState (-size) heightChange
    drawRightSide cubeState size heightChange
    drawTop cubeState size heightChange

drawTop :: CubeState -> PDFFloat -> PDFFloat -> Draw ()
drawTop cubeState size heightChange = do
    addTopTile (firstC $ urf cubeState) 0 0 size heightChange

    addTopTile (firstE $ uf cubeState) (-1) 1 size heightChange
    addTopTile (firstE $ ur cubeState) 1 1 size heightChange

    addTopTile (firstC $ ufl cubeState) (-2) 2 size heightChange
    addTopTile (centerColor $ u cubeState) 0 2 size heightChange
    addTopTile (firstC $ ubr cubeState) 2 2 size heightChange

    addTopTile (firstE $ ul cubeState) (-1) 3 size heightChange
    addTopTile (firstE $ ub cubeState) 1 3 size heightChange

    addTopTile (firstC $ ulb cubeState) 0 4 size heightChange
    
    addTopStroke [(0,0), (-1,1), (1,1), (-2,2), (0,2), (2,2), (-1,3), (1,3), (0,4)] size heightChange

drawRightSide :: CubeState -> PDFFloat -> PDFFloat -> Draw ()
drawRightSide cubeState size heightChange = do
    addSideTile (thirdC $ dfr cubeState) 0 0 size heightChange
    addSideTile (secondE $ dr cubeState) 1 0 size heightChange
    addSideTile (secondC $ drb cubeState) 2 0 size heightChange

    addSideTile (secondE $ fr cubeState) 0 1 size heightChange
    addSideTile (centerColor $ r cubeState) 1 1 size heightChange
    addSideTile (secondE $ br cubeState) 2 1 size heightChange

    addSideTile (secondC $ urf cubeState) 0 2 size heightChange
    addSideTile (secondE $ ur cubeState) 1 2 size heightChange
    addSideTile (thirdC $ ubr cubeState) 2 2 size heightChange
    
    addSideStroke [(x,y) | x <- [0..2], y <- [0..2]] size heightChange


drawLeftSide :: CubeState -> PDFFloat -> PDFFloat -> Draw ()
drawLeftSide cubeState size heightChange = do
    addSideTile (secondC $ dfr cubeState) 0 0 size heightChange
    addSideTile (secondE $ df cubeState) 1 0 size heightChange
    addSideTile (thirdC $ dlf cubeState) 2 0 size heightChange

    addSideTile (firstE $ fr cubeState) 0 1 size heightChange
    addSideTile (centerColor $ f cubeState) 1 1 size heightChange
    addSideTile (firstE $ fl cubeState) 2 1 size heightChange

    addSideTile (thirdC $ urf cubeState) 0 2 size heightChange
    addSideTile (secondE $ uf cubeState) 1 2 size heightChange
    addSideTile (secondC $ ufl cubeState) 2 2 size heightChange
    
    addSideStroke [(x,y) | x <- [0..2], y <- [0..2]] size heightChange

addTile :: CubeColor -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> (PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> [Point]) -> Draw ()
addTile color x y size heightChange tileCalc = do
    fillColor $ cubeColorToPdfColor color
    addPolygonToPath (tileCalc x y size heightChange)
    fillPath

addStroke :: [(PDFFloat, PDFFloat)] -> PDFFloat -> PDFFloat -> (PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> [Point]) -> Draw ()
addStroke ((x, y):xys) size heightChange tileCalc = do
    fillColor black
    addPolygonToPath (tileCalc x y size heightChange)
    strokePath
    addStroke xys size heightChange tileCalc
addStroke [] _ _ _ = return ()

calculatePolygonTileSide :: PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> [Point]
calculatePolygonTileSide x y size heightChange = let sizeY = abs size in
    transformComplex cubeStart cubeStart <$> 
    [ (size * x)     :+ (sizeY * y + heightChange * x)
    , (size * (x+1)) :+ (sizeY * y + heightChange * (x+1))
    , (size * (x+1)) :+ (sizeY * (y+1) + heightChange * (x+1))
    , (size * x)     :+ (sizeY * (y+1) + heightChange * x)
    , (size * x)     :+ (sizeY * y + heightChange * x)
    ]

addSideTile :: CubeColor -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
addSideTile color x y size heightChange = addTile color x y size heightChange calculatePolygonTileSide

addSideStroke :: [(PDFFloat, PDFFloat)] -> PDFFloat -> PDFFloat -> Draw ()
addSideStroke xys size heightChange = addStroke xys size heightChange calculatePolygonTileSide

calculatePolygonTileTop :: PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> [Point]
calculatePolygonTileTop x y size heightChange =
    transformComplex cubeStart (cubeStart + 3 * size) <$> 
    [ (size * x)     :+ (heightChange * y)
    , (size * (x+1)) :+ (heightChange * (y+1))
    , (size * x)     :+ (heightChange * (y+2))
    , (size * (x-1)) :+ (heightChange * (y+1))
    , (size * x)     :+ (heightChange * y)
    ]

addTopTile :: CubeColor -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
addTopTile color x y size heightChange = addTile color x y size heightChange calculatePolygonTileTop

addTopStroke :: [(PDFFloat, PDFFloat)] -> PDFFloat -> PDFFloat -> Draw ()
addTopStroke xys size heightChange = addStroke xys size heightChange calculatePolygonTileTop

rect :: PDFRect
rect = PDFRect 0 0 600 800

orange :: Color
orange = Rgb 1 0.5 0

yellow :: Color
yellow = Rgb 1 1 0

blue :: Color
blue = Rgb 0 0.4 1

cubeColorToPdfColor :: CubeColor -> Color
cubeColorToPdfColor White = white
cubeColorToPdfColor Yellow = yellow
cubeColorToPdfColor Green = green
cubeColorToPdfColor Blue = blue
cubeColorToPdfColor Red = red
cubeColorToPdfColor Orange = orange

drawMove :: Move -> PDFFont -> PDFFloat -> PDFFloat -> Draw ()
drawMove m font size heightChange = do
    setStrokeAlpha 0.75
    fillColor black
    setWidth 5
    setLineCap RoundCap
    curve m size heightChange
    arrow m size heightChange
    strokePath
    timesTwoMark m font

timesTwoMark :: Move -> PDFFont -> Draw ()
timesTwoMark (Move _ Two) font = drawText $ do
    setFont font
    textStart 450 250
    displayText "x2"
timesTwoMark _ _ = return ()

curve :: Move -> PDFFloat -> PDFFloat -> Draw ()
curve (Move F _) size heightChange =
    addPolygonToPath [ (cubeStart + size/2) :+ (cubeStart + size/2 + heightChange/2)
                     , (cubeStart + size/2) :+ (cubeStart + size*3 + heightChange/2)
                     , (cubeStart - size*2) :+ (cubeStart + size*3 + heightChange*3)
                     ]
curve (Move R _) size heightChange =
    addPolygonToPath [ (cubeStart - size/2) :+ (cubeStart + size/2 + heightChange/2)
                     , (cubeStart - size/2) :+ (cubeStart + size*3 + heightChange/2)
                     , (cubeStart + size*2) :+ (cubeStart + size*3 + heightChange*3)
                     ]
curve (Move U _) size heightChange =
    addPolygonToPath [ (cubeStart - size*2.5) :+ (cubeStart + size*2.5 + heightChange*2.5)
                     ,  cubeStart             :+ (cubeStart + size*2.5)
                     , (cubeStart + size*2.5) :+ (cubeStart + size*2.5 + heightChange*2.5)
                     ]
curve (Move B _) size heightChange =
    addPolygonToPath [ (cubeStart + size*2.5) :+ (cubeStart + size/2 + heightChange*2.5)
                     , (cubeStart + size*2.5) :+ (cubeStart + size*3 + heightChange*2.5)
                     ,  cubeStart             :+ (cubeStart + size*3 + heightChange*5)
                     ]
curve (Move L _) size heightChange =
    addPolygonToPath [ (cubeStart - size*2.5) :+ (cubeStart + size/2 + heightChange*2.5)
                     , (cubeStart - size*2.5) :+ (cubeStart + size*3 + heightChange*2.5)
                     ,  cubeStart             :+ (cubeStart + size*3 + heightChange*5)
                     ]
curve (Move D _) size heightChange =
    addPolygonToPath [ (cubeStart - size*2.5) :+ (cubeStart + size*0.5 + heightChange*2.5)
                     ,  cubeStart             :+ (cubeStart + size*0.5)
                     , (cubeStart + size*2.5) :+ (cubeStart + size*0.5 + heightChange*2.5)
                     ]

arrow :: Move -> PDFFloat -> PDFFloat -> Draw ()
arrow (Move m Two) size heightChange = arrow (Move m Normal) size heightChange
arrow (Move F Normal) size heightChange =
    addPolygonToPath [ (cubeStart + size/2 - size/5) :+ (cubeStart + size/2 + size/5 + heightChange/2)
                     , (cubeStart + size/2)          :+ (cubeStart + size/2 + heightChange/2)
                     , (cubeStart + size/2 + size/5) :+ (cubeStart + size/2 + size/5 + heightChange/2)
                     ]
arrow (Move F Prime)  size heightChange =
    addPolygonToPath [ (cubeStart - size*2 + size*0.35) :+ (cubeStart + size*3.04 + heightChange*3)
                     , (cubeStart - size*2)             :+ (cubeStart + size*3 + heightChange*3)
                     , (cubeStart - size*2 + size*0.1)  :+ (cubeStart + size*3 + heightChange*3 - size*0.22)
                     ]
arrow (Move R Normal) size heightChange =
    addPolygonToPath [ (cubeStart + size*2 - size*0.35) :+ (cubeStart + size*3.04 + heightChange*3)
                     , (cubeStart + size*2)             :+ (cubeStart + size*3 + heightChange*3)
                     , (cubeStart + size*2 - size*0.1)  :+ (cubeStart + size*3 + heightChange*3 - size*0.22)
                     ]
arrow (Move R Prime)  size heightChange =
    addPolygonToPath [ (cubeStart - size/2 - size/5) :+ (cubeStart + size/2 + size/5 + heightChange/2)
                     , (cubeStart - size/2)          :+ (cubeStart + size/2 + heightChange/2)
                     , (cubeStart - size/2 + size/5) :+ (cubeStart + size/2 + size/5 + heightChange/2)
                     ]
arrow (Move U Normal) size heightChange =
    addPolygonToPath [ (cubeStart - size*2.5 + size/4) :+ (cubeStart + size*2.5 + heightChange*2.5 + size/6)
                     , (cubeStart - size*2.5)          :+ (cubeStart + size*2.5 + heightChange*2.5)
                     , (cubeStart - size*2.5 + size/5) :+ (cubeStart + size*2.5 + heightChange*2.5 - size/3)
                     ]
arrow (Move U Prime)  size heightChange =
    addPolygonToPath [ (cubeStart + size*2.5 - size/4) :+ (cubeStart + size*2.5 + heightChange*2.5 + size/6)
                     , (cubeStart + size*2.5)          :+ (cubeStart + size*2.5 + heightChange*2.5)
                     , (cubeStart + size*2.5 - size/5) :+ (cubeStart + size*2.5 + heightChange*2.5 - size/3)
                     ]
arrow (Move B Normal) size heightChange =
    addPolygonToPath [ (cubeStart + size/7) :+ (cubeStart + size*3 + heightChange*5 - size/4)
                     ,  cubeStart           :+ (cubeStart + size*3 + heightChange*5)
                     , (cubeStart + size/3) :+ (cubeStart + size*3 + heightChange*5 + size/15)
                     ]
arrow (Move B Prime)  size heightChange =
    addPolygonToPath [ (cubeStart + size*2.5 - size/5) :+ (cubeStart + size/2 + size/5 + heightChange*2.5)
                     , (cubeStart + size*2.5)          :+ (cubeStart + size/2 + heightChange*2.5)
                     , (cubeStart + size*2.5 + size/5) :+ (cubeStart + size/2 + size/5 + heightChange*2.5)
                     ]
arrow (Move L Normal) size heightChange =
    addPolygonToPath [ (cubeStart - size*2.5 - size/5) :+ (cubeStart + size/2 + size/5 + heightChange*2.5)
                     , (cubeStart - size*2.5)          :+ (cubeStart + size/2 + heightChange*2.5)
                     , (cubeStart - size*2.5 + size/5) :+ (cubeStart + size/2 + size/5 + heightChange*2.5)
                     ]
arrow (Move L Prime)  size heightChange =
    addPolygonToPath [ (cubeStart - size/7) :+ (cubeStart + size*3 + heightChange*5 - size/4)
                     ,  cubeStart           :+ (cubeStart + size*3 + heightChange*5)
                     , (cubeStart - size/3) :+ (cubeStart + size*3 + heightChange*5 + size/15)
                     ]
arrow (Move D Normal) size heightChange =
    addPolygonToPath [ (cubeStart + size*2.5 - size/4) :+ (cubeStart + size/2 + heightChange*2.5 + size/6)
                     , (cubeStart + size*2.5)          :+ (cubeStart + size/2 + heightChange*2.5)
                     , (cubeStart + size*2.5 - size/5) :+ (cubeStart + size/2 + heightChange*2.5 - size/3)
                     ]
arrow (Move D Prime)  size heightChange =
    addPolygonToPath [ (cubeStart - size*2.5 + size/4) :+ (cubeStart + size/2 + heightChange*2.5 + size/6)
                     , (cubeStart - size*2.5)          :+ (cubeStart + size/2 + heightChange*2.5)
                     , (cubeStart - size*2.5 + size/5) :+ (cubeStart + size/2 + heightChange*2.5 - size/3)
                     ]

drawPageNumber :: Int -> PDFFont -> Draw ()
drawPageNumber pageNumber font = do
    drawText $ do
        setFont font
        textStart 50 700
        displayText (T.pack $ show pageNumber)

drawCubePage :: CubeState -> Int -> PDFFont -> PDFFloat -> PDFFloat -> Draw ()
drawCubePage cubeState pageNumber font size heightChange = do
    drawCube cubeState size heightChange
    drawPageNumber pageNumber font

drawCubePageWithMove :: CubeState -> Move -> Int -> PDFFont -> PDFFloat -> PDFFloat -> Draw ()
drawCubePageWithMove cubeState m pageNumber font size heightChange = do
    drawCubePage cubeState pageNumber font size heightChange
    drawMove m font size heightChange

createPdfCubeSolution :: Algorithm -> Int -> PDFFont -> PDFFloat -> PDFFloat ->  Cube (PDF ())
createPdfCubeSolution (m:ms) pageNumber font size heightChange = do
    cubeState <- get
    move m
    rest <- createPdfCubeSolution ms (pageNumber + 1) font size heightChange
    return $ do
        page <- addPage Nothing
        drawWithPage page (drawCubePageWithMove cubeState m pageNumber font size heightChange)
        rest
createPdfCubeSolution [] pageNumber font size heightChange  = do
    cubeState <- get
    return $ do
        page <- addPage Nothing
        drawWithPage page (drawCubePage cubeState pageNumber font size heightChange)
