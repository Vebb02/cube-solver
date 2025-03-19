module CFOP.Cross where

import Cube
import CubeState
import Control.Monad.State
import CubeValidator
import Data.Maybe
import Data.List ( permutations )

cross :: Cube Algorithm
cross = do
    undefined

crossEdges :: [CubeState -> Edge]
crossEdges = [df, dr, db, dl]

crossSolved :: CubeState -> Bool
crossSolved cubeState = all (\edge -> edge cubeState == edge solvedCube) crossEdges
