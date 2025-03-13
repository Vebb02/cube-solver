{-# LANGUAGE OverloadedStrings #-}
module PDFCube where

import Graphics.PDF.Colors
import Graphics.PDF
import CubeState
import Cube
import Control.Monad.State

runPDFTest :: Algorithm -> CubeState -> IO ()
runPDFTest alg cubeState = do
    let pdf = evalState (createPdfCubeSolution alg) cubeState
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


drawCubePage :: CubeState -> Draw ()
drawCubePage cubeState = do
    drawCube cubeState

drawMove :: Move -> Draw()
drawMove m = do
    undefined

drawCubePageWithMove :: CubeState -> Move -> Draw()
drawCubePageWithMove cubeState m = do
    drawCubePage cubeState
    -- drawMove m

createPdfCubeSolution :: Algorithm -> Cube (PDF ())
createPdfCubeSolution (m:ms) = do
    cubeState <- get
    let pdf = do
            page <- addPage Nothing
            drawWithPage page (drawCubePageWithMove cubeState m)
    move m
    rest <- createPdfCubeSolution ms
    return (pdf >> rest) 
createPdfCubeSolution [] = do
    cubeState <- get
    let pdf = do
            page <- addPage Nothing
            drawWithPage page (drawCubePage cubeState)
    return pdf
