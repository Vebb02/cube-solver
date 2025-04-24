module Main where

import Cube
import CubeState
import CubeValidator

import CFOP.Cross
import CFOP.F2L
import CFOP.OLL
import CFOP.PLL 
import CFOP.CFOP

import Control.Monad.State
import Text.Megaparsec
import qualified Data.Text as T

import Test.QuickCheck
import System.Exit (exitFailure)
import CubeParser (parseCubeState, parseScramble)

main :: IO ()
main = do
    processQuickCheck $ quickCheckResult moveChangesState
    processQuickCheck $ quickCheckResult alwaysValidCubeState
    processQuickCheck $ quickCheckResult undoMoveCancelsOut
    processQuickCheck $ quickCheckResult reverseSequenceCancelsOut
    processQuickCheck $ quickCheckResult combineMovesSameAsSequential
    processQuickCheck $ quickCheckResult cancellingMovesLessOrEqual
    processQuickCheck $ quickCheckResult parseCubeStateTest
    processQuickCheck $ quickCheckResult parseAlgorithmTest
    processQuickCheck $ quickCheckResult (withMaxSuccess 1000 crossSolvesCross)
    processQuickCheck $ quickCheckResult (withMaxSuccess 1000 f2lSolvesF2l)
    processQuickCheck $ quickCheckResult (withMaxSuccess 1000 ollSolvesOll)
    processQuickCheck $ quickCheckResult (withMaxSuccess 1000 pllSolvesPll)
    processQuickCheck $ quickCheckResult (withMaxSuccess 10000 cfopSolvesTheCube)

processQuickCheck :: IO Result -> IO ()
processQuickCheck ioResult = do
    result <- ioResult
    case result of
        Success {} -> return ()
        _ -> exitFailure

getState :: Cube a -> CubeState
getState transformation = execState transformation solvedCube

moveChangesState :: Move -> Bool
moveChangesState m = 
    let newState = getState (move m)
    in not $ cubeSolved newState

alwaysValidCubeState :: Algorithm -> Bool
alwaysValidCubeState alg = 
    let newState = getState (applyAlgorithm alg)
    in validateCubeState newState

undoMoveCancelsOut :: Move -> Bool
undoMoveCancelsOut m =
    let newState = getState (do move m; undoMove m)
    in cubeSolved newState

reverseSequenceCancelsOut :: Algorithm -> Bool
reverseSequenceCancelsOut alg =
    let newState = getState (do applyAlgorithm alg; applyAlgorithm (reverseMoveSeq alg))
    in cubeSolved newState

combineMovesSameAsSequential :: Move -> Move -> Bool
combineMovesSameAsSequential m1 m2 =
    let combinedState = getState (applyAlgorithm $ combineMoves m1 m2)
        sequentialState = getState (do move m1; move m2)
    in combinedState == sequentialState

cancellingMovesLessOrEqual :: Algorithm -> Bool
cancellingMovesLessOrEqual alg =
    let shortenedAlg = removeCancellingMoves alg []
    in length shortenedAlg <= length alg

parseCubeStateTest :: Algorithm -> Bool
parseCubeStateTest alg =
    let newState = getState (applyAlgorithm alg)
        eitherParsedState = runParser parseCubeState "" (T.pack $ show newState)
    in case eitherParsedState of
        Left errorMessage -> error $ show errorMessage
        Right parsedState -> newState == parsedState

parseAlgorithmTest :: Algorithm -> Bool
parseAlgorithmTest alg =
    let eitherParsedAlg = runParser parseScramble "" (T.pack $ showAlg alg)
    in case eitherParsedAlg of
        Left errorMessage -> error $ show errorMessage
        Right parsedAlg -> alg == parsedAlg

crossSolvesCross :: Algorithm -> Bool
crossSolvesCross scramble = solvesStep scramble cross crossSolved

f2lSolvesF2l :: Algorithm -> Bool
f2lSolvesF2l scramble = solvesStep scramble (do cross; f2l) f2lSolved

ollSolvesOll :: Algorithm -> Bool
ollSolvesOll scramble = solvesStep scramble (do cross; f2l; oll) ollSolved

pllSolvesPll :: Algorithm -> Bool
pllSolvesPll scramble = solvesStep scramble (do cross; f2l; oll; pll) pllSolved

cfopSolvesTheCube :: Algorithm -> Bool
cfopSolvesTheCube scramble = solvesStep scramble cfop (== solvedCube)

solvesStep :: Algorithm -> Cube Algorithm -> (CubeState -> Bool) -> Bool
solvesStep scramble solver verifier =
    let scrambled = getState (applyAlgorithm scramble)
        solvedState = execState solver scrambled
    in verifier solvedState

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
