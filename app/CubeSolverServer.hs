{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module CubeSolverServer (runServer) where
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant

type CubeSolverApi = "api" :> "cubesolver" :> Get '[PlainText] String

server :: Server CubeSolverApi
server = return "Hello world\n"

api :: Proxy CubeSolverApi
api = Proxy

app :: Application
app = serve api server

runServer :: IO ()
runServer = run 8082 app
