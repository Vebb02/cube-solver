module Main where

import PDFCube (generatePDF)
import BluetoothCube (bluetooth)

main :: IO ()
main = do
    bluetooth
    generatePDF
