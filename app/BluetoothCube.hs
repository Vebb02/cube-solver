module BluetoothCube (bluetooth) where

import PDFCube (generatePDFFromCubeState)
import System.IO
import System.Process


bluetooth :: IO ()
bluetooth = do
    callCommand "rfkill unblock bluetooth"
    let process = shell "cd app/bluetooth && npm start"
    (_, maybeHout, _, ph) <- createProcess process { std_out = CreatePipe }
    case maybeHout of
        Just hout -> readLines 0 hout
        Nothing -> error "Could not get input handle"
    terminateProcess ph
    callCommand "rfkill block bluetooth"
    callCommand "rfkill unblock bluetooth"
    return ()


readLines :: Int -> Handle -> IO ()
readLines 25 _ = return ()
readLines count hout = do
    input <- hGetLine hout
    putStrLn input
    readLines (count + 1) hout
