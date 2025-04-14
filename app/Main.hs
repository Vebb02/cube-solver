module Main where

import PDFCube (generatePDF)
import BluetoothCube (bluetooth, decryptionTest)

main :: IO ()
main = do
    decryptionTest
    -- bluetooth
    generatePDF
