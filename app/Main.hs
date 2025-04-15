module Main where

import PDFCube (generatePDF)
import BluetoothCube (bluetooth)

main :: IO ()
main = if True then bluetooth else generatePDF
