import Distribution.Simple
import System.Process

main :: IO ()
main = defaultMainWithHooks simpleUserHooks 
    { preBuild = \args flags -> do
        callCommand "cd src/bluetooth && npm i"
        preBuild simpleUserHooks args flags
    }
