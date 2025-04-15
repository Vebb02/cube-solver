{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CubeSolverServer (runServer) where
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant
import Text.Megaparsec (runParser)
import CubeParser (parseCubeState)
import qualified Data.Text as T
import CFOP.CFOP (cfop)
import Control.Monad.State (evalState, MonadIO (liftIO))
import qualified Data.ByteString as BS
import PDFCube (generatePDFFromSolution)

type CubeSolverApi = "api" :> "cubesolver" :> QueryParam "cube" String :>  Get '[OctetStream] BS.ByteString

solveCube :: Maybe String -> Handler BS.ByteString
solveCube Nothing = return "No paramter given\n"
solveCube (Just unparsedCube) = do
    let parsedResult = runParser parseCubeState "" (T.pack unparsedCube)
    case parsedResult of
        Left _ -> return "Parsing failed\n"
        Right cubeState -> do
            liftIO $ generatePDFFromSolution cubeState (evalState cfop cubeState)
            liftIO $ BS.readFile "solution_manual.pdf"

server :: Server CubeSolverApi
server = solveCube

api :: Proxy CubeSolverApi
api = Proxy

app :: Application
app = serve api server

runServer :: IO ()
runServer = run 8082 app
