{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module CubeSolverServer (runServer) where
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant
import Text.Megaparsec (runParser)
import CubeParser (parseCubeState)
import qualified Data.Text as T
import CFOP.CFOP (cfop)
import Control.Monad.State (evalState)

type CubeSolverApi = "api" :> "cubesolver" :> QueryParam "cube" String :>  Get '[PlainText] String

solveCube :: Maybe String -> Handler String
solveCube Nothing = return "No paramter given\n"
solveCube (Just unparsedCube) = do
    let parsedResult = runParser parseCubeState "" (T.pack unparsedCube)
    case parsedResult of
        Left _ -> return "Parsing failed"
        Right cubeState -> return $ show (evalState cfop cubeState) ++ "\n"
            -- do
            -- generatePDFFromSolution (evalState cfop cubeState) cubeState

server :: Server CubeSolverApi
server = solveCube

api :: Proxy CubeSolverApi
api = Proxy

app :: Application
app = serve api server

runServer :: IO ()
runServer = run 8082 app
