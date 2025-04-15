import Distribution.Simple
import System.Process

main :: IO ()
main = defaultMainWithHooks simpleUserHooks 
    { preBuild = \args flags -> do
        callCommand "cd app/bluetooth && npm i"
        preBuild simpleUserHooks args flags
    }
