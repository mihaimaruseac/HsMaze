module Maze.Plan
where

-- TODO: limit the exported symbols

import Control.Arrow
import Control.Monad.State
import Data.Array.ST
import System.Random

import qualified Array as A
import qualified Data.Vector as V

import Maze.Maze
import Maze.Types

{- The environment when testing a chromosome. -}
type Env = (Maze, Plan)

{- The results of testing a chromosome, before getting the fitness. -}
type RPoint = (Point, Time)

{-
Does one step of the plan, updating the position of the robot (in maze and
plan).
-}
doStep :: Env -> RPoint -> RPoint
doStep (m, p) (pos, t) = (takeAct (m A.! pos) (p V.! t) pos, t + 1)

{-
Take one action.
-}
takeAct :: Cell -> Cardinal -> Point -> Point
takeAct (C l) d p = if d `elem` l then move d p else p

{-
Move in one direction.
-}
move :: Cardinal -> Point -> Point
move N = first (subtract 1)
move E = second (+ 1)
move S = first (+ 1)
move W = second (subtract 1)

{-
Generates the random initial population.
-}
getRandomInitialPlans :: Int -> Int -> State StdGen (V.Vector Plan)
getRandomInitialPlans len size = do
  plans <- replicateM len (genOnePlan size)
  return $ V.fromList plans

{-
Generates one initial plan (one chromosome from the initial population).
-}
genOnePlan :: Int -> State StdGen Plan
genOnePlan len = do
  l <- replicateM len getRandomDir
  return $ V.fromList l

{-
Generates one random direction.
-}
getRandomDir :: State StdGen Cardinal
getRandomDir = do
  r <- state $ randomR (0, 3)
  return $ toEnum r

{-
Computes the fitness of a plan.
-}
fitness :: Point -> Time -> Point -> Time -> Fitness
fitness p t ep et = 100 * (et - t) - 3 * manhattan p ep

{-
Gets the manhattan distance between two points.
-}
manhattan :: Point -> Point -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

