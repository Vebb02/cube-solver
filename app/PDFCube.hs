{-# LANGUAGE OverloadedStrings #-}
module PDFCube where

import Graphics.PDF.Colors
import Graphics.PDF
import CubeState
import Cube
import Control.Monad.State
import qualified Data.Text as T


runPDFTest :: Algorithm -> CubeState -> IO ()
runPDFTest alg cubeState = do
    eitherFont <- mkStdFont Helvetica_Bold
    eitherFrontPageImage <- readJpegFile "images/front-page.jpeg"
    eitherWarningImage <- readJpegFile "images/warning-page.jpeg"
    case eitherFont of
        Left errorMessage -> print errorMessage
        Right anyFont -> case eitherFrontPageImage of
            Left errorMessage -> print errorMessage
            Right frontPageImage -> case eitherWarningImage of
                Left errorMessage -> print errorMessage
                Right warningImage -> do
                    let introductionPages = imagePage frontPageImage >> imagePage warningImage
                    let font = PDFFont anyFont 50
                    let size = 50
                    let heightChange = 20
                    let instructions = evalState (createPdfCubeSolution alg 1 font size heightChange) cubeState
                    let pdf = introductionPages >> instructions
                    runPdf "test.pdf" standardDocInfo rect pdf


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

cubeColorToPdfColor :: CubeColor -> Color
cubeColorToPdfColor White = white
cubeColorToPdfColor Yellow = yellow
cubeColorToPdfColor Green = green
cubeColorToPdfColor Blue = blue
cubeColorToPdfColor Red = red
cubeColorToPdfColor Orange = orange

drawMove :: Move -> PDFFont -> PDFFloat -> PDFFloat -> Draw()
drawMove m font size heightChange = do
    setStrokeAlpha 0.75
    fillColor black
    setWidth 5
    setLineCap RoundCap
    curve m size heightChange
    arrow m
    strokePath
    timesTwoMark m font

timesTwoMark :: Move -> PDFFont -> Draw ()
timesTwoMark (Move _ Two) font = drawText $ do
    setFont font
    textStart 450 250
    displayText "x2"
timesTwoMark _ _ = return ()

curve :: Move -> PDFFloat -> PDFFloat -> Draw ()
curve (Move F _) size heightChange = addPolygonToPath [ (cubeStart + size/2) :+ (cubeStart + size/2 + heightChange/2)
                                                      , (cubeStart + size/2) :+ (cubeStart + size*3 + heightChange/2)
                                                      , (cubeStart - size*2) :+ (cubeStart + size*3 + heightChange*3)
                                                      ]
curve (Move R _) size heightChange = addPolygonToPath [ (cubeStart - size/2) :+ (cubeStart + size/2 + heightChange/2)
                                                      , (cubeStart - size/2) :+ (cubeStart + size*3 + heightChange/2)
                                                      , (cubeStart + size*2) :+ (cubeStart + size*3 + heightChange*3)
                                                      ]
curve (Move U _) size heightChange = addPolygonToPath [ (cubeStart - size*2.5) :+ (cubeStart + size*2.5 + heightChange*2.5)
                                                      ,  cubeStart             :+ (cubeStart + size*2.5)
                                                      , (cubeStart + size*2.5) :+ (cubeStart + size*2.5 + heightChange*2.5)
                                                      ]
curve (Move B _) size heightChange = addPolygonToPath [ (cubeStart + size*2.5) :+ (cubeStart + size/2 + heightChange*2.5)
                                                      , (cubeStart + size*2.5) :+ (cubeStart + size*3 + heightChange*2.5)
                                                      ,  cubeStart             :+ (cubeStart + size*3 + heightChange*5)
                                                      ]
curve (Move L _) size heightChange = addPolygonToPath [ (cubeStart - size*2.5) :+ (cubeStart + size/2 + heightChange*2.5)
                                                      , (cubeStart - size*2.5) :+ (cubeStart + size*3 + heightChange*2.5)
                                                      ,  cubeStart             :+ (cubeStart + size*3 + heightChange*5)
                                                      ]
curve (Move D _) size heightChange = addPolygonToPath [ (cubeStart - size*2.5) :+ (cubeStart + size*0.5 + heightChange*2.5)
                                                      ,  cubeStart             :+ (cubeStart + size*0.5)
                                                      , (cubeStart + size*2.5) :+ (cubeStart + size*0.5 + heightChange*2.5)
                                                      ]

arrow :: Move -> Draw ()
arrow (Move m Two) = arrow (Move m Normal)
arrow (Move F Normal) = addPolygonToPath [315 :+ 345, 325 :+ 335, 335 :+ 345]
arrow (Move F Prime) = addPolygonToPath [210 :+ 500, 200 :+ 515, 215 :+ 520]
arrow (Move R Normal) = addPolygonToPath [390 :+ 500, 400 :+ 515, 385 :+ 520]
arrow (Move R Prime) = addPolygonToPath [285 :+ 345, 275 :+ 335, 265 :+ 345]
arrow (Move U Normal) = addPolygonToPath [185 :+ 460, 175 :+ 475, 190 :+ 480]
arrow (Move U Prime) = addPolygonToPath [415 :+ 460, 425 :+ 475, 410 :+ 480]
arrow (Move B Normal) = addPolygonToPath [310 :+ 538, 300 :+ 550, 315 :+ 552]
arrow (Move B Prime) = addPolygonToPath [415 :+ 385, 425 :+ 375, 435 :+ 385]
arrow (Move L Normal) = addPolygonToPath [185 :+ 385, 175 :+ 375, 165 :+ 385]
arrow (Move L Prime) = addPolygonToPath [290 :+ 538, 300 :+ 550, 285 :+ 552]
arrow (Move D Normal) = addPolygonToPath [415 :+ 360, 425 :+ 375, 410 :+ 380]
arrow (Move D Prime) = addPolygonToPath [185 :+ 360, 175 :+ 375, 190 :+ 380]


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

drawCubePageWithMove :: CubeState -> Move -> Int -> PDFFont -> PDFFloat -> PDFFloat -> Draw()
drawCubePageWithMove cubeState m pageNumber font size heightChange = do
    drawCubePage cubeState pageNumber font size heightChange
    drawMove m font size heightChange

createPdfCubeSolution :: Algorithm -> Int -> PDFFont -> PDFFloat -> PDFFloat ->  Cube (PDF ())
createPdfCubeSolution (m:ms) pageNumber font size heightChange = do
    cubeState <- get
    let pdf = do
            page <- addPage Nothing
            drawWithPage page (drawCubePageWithMove cubeState m pageNumber font size heightChange)
    move m
    rest <- createPdfCubeSolution ms (pageNumber + 1) font size heightChange
    return (pdf >> rest) 
createPdfCubeSolution [] pageNumber font size heightChange  = do
    cubeState <- get
    let pdf = do
            page <- addPage Nothing
            drawWithPage page (drawCubePage cubeState pageNumber font size heightChange)
    return pdf
