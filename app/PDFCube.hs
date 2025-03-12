{-# LANGUAGE OverloadedStrings #-}
module PDFCube where

import Graphics.PDF.Colors
import Graphics.PDF

runPDFTest :: IO ()
runPDFTest = do
    let pdf = createPdf
    runPdf "test.pdf" standardDocInfo rect pdf

createPdf :: PDF ()
createPdf = do
    page <- addPage Nothing
    drawWithPage page draw


leftTile :: [Point]
leftTile = [300 :+ 300, 250 :+ 320, 250 :+ 370, 300 :+ 350, 300 :+ 300]

rightTile :: [Point]
rightTile = [300 :+ 300, 350 :+ 320, 350 :+ 370, 300 :+ 350, 300 :+ 300]

topTile :: [Point]
topTile = [300 :+ 450, 350 :+ 470, 300 :+ 490, 250 :+ 470, 300 :+ 450]

transformComplex :: PDFFloat -> PDFFloat -> Point -> Point
transformComplex dx dy (x :+ y) = (x + dx) :+ (y + dy)

draw :: Draw ()
draw = do
    drawLeftSide
    drawRightSide
    drawTop

drawTop :: Draw ()
drawTop = do
    addTile green topTile 0 0

    addTile white topTile (-50) 20
    addTile red topTile 50 20

    addTile orange topTile (-100) 40
    addTile white topTile 0 40
    addTile white topTile 100 40

    addTile green topTile (-50) 60
    addTile red topTile 50 60

    addTile blue topTile 0 80
    

    addStroke topTile 0 0 

    addStroke topTile (-50) 20
    addStroke topTile 50 20

    addStroke topTile (-100) 40
    addStroke topTile 0 40
    addStroke topTile 100 40

    addStroke topTile (-50) 60
    addStroke topTile 50 60

    addStroke topTile 0 80

drawRightSide :: Draw ()
drawRightSide = do
    addTile yellow rightTile 0 0 
    addTile red rightTile 50 20
    addTile green rightTile (50 * 2) (20*2)

    addTile white rightTile 0 50
    addTile red rightTile 50 (50+20)
    addTile red rightTile (50 * 2) (50+20*2)

    addTile red rightTile 0 (50*2)
    addTile green rightTile 50 (50*2+20)
    addTile blue rightTile (50 * 2) (50*2+20*2)
    
    addStroke rightTile 0 0 
    addStroke rightTile 50 20
    addStroke rightTile (50 * 2) (20*2)

    addStroke rightTile 0 50
    addStroke rightTile 50 (50+20)
    addStroke rightTile (50 * 2) (50+20*2)
    
    addStroke rightTile 0 (50*2)
    addStroke rightTile 50 (50*2+20)
    addStroke rightTile (50 * 2) (50*2+20*2)

drawLeftSide :: Draw ()
drawLeftSide = do
    addTile red leftTile 0 0 
    addTile yellow leftTile (-50) 20
    addTile blue leftTile (-(50 * 2)) (20*2)

    addTile blue leftTile 0 50
    addTile green leftTile (-50) (50+20)
    addTile orange leftTile (-(50 * 2)) (50+20*2)

    addTile yellow leftTile 0 (50*2)
    addTile green leftTile (-50) (50*2+20)
    addTile blue leftTile (-(50 * 2)) (50*2+20*2)
    
    addStroke leftTile 0 0 
    addStroke leftTile (-50) 20
    addStroke leftTile (-(50 * 2)) (20*2)

    addStroke leftTile 0 50
    addStroke leftTile (-50) (50+20)
    addStroke leftTile (-(50 * 2)) (50+20*2)
    
    addStroke leftTile 0 (50*2)
    addStroke leftTile (-50) (50*2+20)
    addStroke leftTile (-(50 * 2)) (50*2+20*2)
addTile :: Color -> [Point] -> PDFFloat -> PDFFloat -> Draw ()
addTile color tile dx dy = do
    fillColor color
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
