module Main where

import BluetoothCube (bluetooth)
import Options
import PDFCube (generatePDF)

newtype CubeSolverOptions = CubeSolverOptions
    { bluetoothMode :: Bool }

instance Options CubeSolverOptions where
    defineOptions = CubeSolverOptions <$> simpleOption "bluetooth" False
        "Whether to use bluetooth cube."

main :: IO ()
main = runCommand $ \opts _ ->
    if bluetoothMode opts
        then bluetooth
        else generatePDF