module Main where

import BluetoothCube (bluetooth)
import Options
import PDFCube (generatePDF)
import CubeSolverServer (runServer)

data CubeSolverOptions = CubeSolverOptions
    { bluetoothMode :: Bool
    , serverMode :: Bool
    }

instance Options CubeSolverOptions where
    defineOptions = CubeSolverOptions
        <$> simpleOption "bluetooth" False
        "Whether to use bluetooth cube."
        <*> simpleOption "server" False
        "Whether to host a server."

main :: IO ()
main = runCommand $ \opts _ ->
    if serverMode opts
        then runServer
        else if bluetoothMode opts
            then bluetooth
            else generatePDF
