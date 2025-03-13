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
    case eitherFont of
        Left errorMessage -> print errorMessage
        Right anyFont -> do
            let font = PDFFont anyFont 50
            let pdf = evalState (createPdfCubeSolution alg 1 font) cubeState
            runPdf "test.pdf" standardDocInfo rect pdf


leftTile :: [Point]
leftTile = [300 :+ 300, 250 :+ 320, 250 :+ 370, 300 :+ 350, 300 :+ 300]

rightTile :: [Point]
rightTile = [300 :+ 300, 350 :+ 320, 350 :+ 370, 300 :+ 350, 300 :+ 300]

topTile :: [Point]
topTile = [300 :+ 450, 350 :+ 470, 300 :+ 490, 250 :+ 470, 300 :+ 450]

transformComplex :: PDFFloat -> PDFFloat -> Point -> Point
transformComplex dx dy (x :+ y) = (x + dx) :+ (y + dy)

drawCube :: CubeState -> Draw ()
drawCube cubeState = do
    drawLeftSide cubeState
    drawRightSide cubeState
    drawTop cubeState

drawTop :: CubeState -> Draw ()
drawTop cubeState = do
    addTile (firstC $ urf cubeState) topTile 0 0

    addTile (firstE $ uf cubeState) topTile (-50) 20
    addTile (firstE $ ur cubeState) topTile 50 20

    addTile (firstC $ ufl cubeState) topTile (-100) 40
    addTile (centerColor $ u cubeState) topTile 0 40
    addTile (firstC $ ubr cubeState) topTile 100 40

    addTile (firstE $ ul cubeState) topTile (-50) 60
    addTile (firstE $ ub cubeState) topTile 50 60

    addTile (firstC $ ulb cubeState) topTile 0 80
    

    addStroke topTile 0 0 

    addStroke topTile (-50) 20
    addStroke topTile 50 20

    addStroke topTile (-100) 40
    addStroke topTile 0 40
    addStroke topTile 100 40

    addStroke topTile (-50) 60
    addStroke topTile 50 60

    addStroke topTile 0 80

drawRightSide :: CubeState -> Draw ()
drawRightSide cubeState = do
    addTile (thirdC $ dfr cubeState) rightTile 0 0 
    addTile (secondE $ dr cubeState) rightTile 50 20
    addTile (secondC $ drb cubeState) rightTile (50 * 2) (20*2)

    addTile (secondE $ fr cubeState) rightTile 0 50
    addTile (centerColor $ r cubeState) rightTile 50 (50+20)
    addTile (secondE $ br cubeState) rightTile (50 * 2) (50+20*2)

    addTile (secondC $ urf cubeState) rightTile 0 (50*2)
    addTile (secondE $ ur cubeState) rightTile 50 (50*2+20)
    addTile (thirdC $ ubr cubeState) rightTile (50 * 2) (50*2+20*2)
    
    addStroke rightTile 0 0 
    addStroke rightTile 50 20
    addStroke rightTile (50 * 2) (20*2)

    addStroke rightTile 0 50
    addStroke rightTile 50 (50+20)
    addStroke rightTile (50 * 2) (50+20*2)
    
    addStroke rightTile 0 (50*2)
    addStroke rightTile 50 (50*2+20)
    addStroke rightTile (50 * 2) (50*2+20*2)

drawLeftSide :: CubeState -> Draw ()
drawLeftSide cubeState = do
    addTile (secondC $ dfr cubeState) leftTile 0 0 
    addTile (secondE $ df cubeState) leftTile (-50) 20
    addTile (thirdC $ dlf cubeState) leftTile (-(50 * 2)) (20*2)

    addTile (firstE $ fr cubeState) leftTile 0 50
    addTile (centerColor $ f cubeState) leftTile (-50) (50+20)
    addTile (firstE $ fl cubeState) leftTile (-(50 * 2)) (50+20*2)

    addTile (thirdC $ urf cubeState) leftTile 0 (50*2)
    addTile (secondE $ uf cubeState) leftTile (-50) (50*2+20)
    addTile (secondC $ ufl cubeState) leftTile (-(50 * 2)) (50*2+20*2)
    
    addStroke leftTile 0 0 
    addStroke leftTile (-50) 20
    addStroke leftTile (-(50 * 2)) (20*2)

    addStroke leftTile 0 50
    addStroke leftTile (-50) (50+20)
    addStroke leftTile (-(50 * 2)) (50+20*2)
    
    addStroke leftTile 0 (50*2)
    addStroke leftTile (-50) (50*2+20)
    addStroke leftTile (-(50 * 2)) (50*2+20*2)
addTile :: CubeColor -> [Point] -> PDFFloat -> PDFFloat -> Draw ()
addTile color tile dx dy = do
    fillColor $ cubeColorToPdfColor color
    addPolygonToPath (fmap (transformComplex dx dy) tile)
    fillPath

addStroke :: [Point] -> PDFFloat -> PDFFloat -> Draw ()
addStroke tile dx dy = do
    fillColor black
    addPolygonToPath (fmap (transformComplex dx dy) tile)
    strokePath

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

drawMove :: Move -> PDFFont -> Draw()
drawMove m font = do
    setStrokeAlpha 0.75
    fillColor black
    setWidth 5
    setLineCap RoundCap
    curve m
    arrow m
    strokePath
    timesTwoMark m font

timesTwoMark :: Move -> PDFFont -> Draw ()
timesTwoMark (Move _ Two) font = drawText $ do
    setFont font
    textStart 450 250
    displayText "x2"
timesTwoMark _ _ = return ()

curve :: Move -> Draw ()
curve (Move F _) = addPolygonToPath [325 :+ 335, 325 :+ 460, 200 :+ 515]
curve (Move R _) = addPolygonToPath [275 :+ 335, 275 :+ 460, 400 :+ 515]
curve (Move U _) = addPolygonToPath [175 :+ 475, 300 :+ 425, 425 :+ 475]
curve (Move B _) = addPolygonToPath [425 :+ 375, 425 :+ 500, 300 :+ 550]
curve (Move L _) = addPolygonToPath [175 :+ 375, 175 :+ 500, 300 :+ 550]
curve (Move D _) = addPolygonToPath [175 :+ 375, 300 :+ 325, 425 :+ 375]

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

drawCubePage :: CubeState -> Int -> PDFFont -> Draw ()
drawCubePage cubeState pageNumber font = do
    drawCube cubeState
    drawPageNumber pageNumber font

drawCubePageWithMove :: CubeState -> Move -> Int -> PDFFont -> Draw()
drawCubePageWithMove cubeState m pageNumber font = do
    drawCubePage cubeState pageNumber font
    drawMove m font

createPdfCubeSolution :: Algorithm -> Int -> PDFFont -> Cube (PDF ())
createPdfCubeSolution (m:ms) pageNumber font = do
    cubeState <- get
    let pdf = do
            page <- addPage Nothing
            drawWithPage page (drawCubePageWithMove cubeState m pageNumber font)
    move m
    rest <- createPdfCubeSolution ms (pageNumber + 1) font
    return (pdf >> rest) 
createPdfCubeSolution [] pageNumber font = do
    cubeState <- get
    let pdf = do
            page <- addPage Nothing
            drawWithPage page (drawCubePage cubeState pageNumber font)
    return pdf
