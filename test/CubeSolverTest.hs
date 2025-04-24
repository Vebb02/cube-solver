module Main where

import Cube
import CubeState
import CubeValidator

import Test.QuickCheck
import Control.Monad.State

main :: IO ()
main = do
    quickCheck moveChangesState
    quickCheck alwaysValidCubeState

moveChangesState :: Move -> Bool
moveChangesState m = 
    let newState = execState (move m) solvedCube
    in not $ cubeSolved newState

alwaysValidCubeState :: Algorithm -> Bool
alwaysValidCubeState alg = 
    let newState = execState (applyAlgorithm alg) solvedCube
    in validateCubeState newState

instance Arbitrary Move where
    arbitrary = applyArbitrary2 Move 

instance Arbitrary MoveFace where
    arbitrary = 
        oneof [ return F
              , return B
              , return R
              , return L
              , return U
              , return D
              ]

instance Arbitrary MoveDirection where
    arbitrary = 
        oneof [ return Normal
              , return Prime
              , return Two
              ]
