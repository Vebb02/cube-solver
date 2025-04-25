module Main where

import BluetoothCube (bluetooth)
import Options
import PDFCube (generatePDF)
import CubeSolverServer (runServer)

data CubeSolverOptions = CubeSolverOptions
    { bluetoothMode :: Bool
    , serverMode :: Bool
    , nonLinux :: Bool
    }

instance Options CubeSolverOptions where
    defineOptions = CubeSolverOptions
        <$> simpleOption "bluetooth" False
        "Whether to use bluetooth cube."
        <*> simpleOption "server" False
        "Whether to host a server."
        <*> simpleOption "non-linux" False
        "Whether the system is not Linux."

main :: IO ()
main = runCommand $ \opts _ ->
    if serverMode opts
        then runServer
        else if bluetoothMode opts
            then bluetooth $ not (nonLinux opts)
            else generatePDF
