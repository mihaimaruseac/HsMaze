module Maze.Plan
where

import Control.Arrow

import qualified Array as A
import qualified Data.Vector as V

import Maze.Maze
import Maze.Types

{- Type synonim for the fitness. -}
type Fitness = Integer

{- Time step. Limited to the size of maze (by code). -}
type Time = Int

{- A plan is a vector of directions to go, at each time step. -}
type Plan = V.Vector Cardinal

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
m = fst $ (runState $ genMaze (5, 5)) (mkStdGen 42)
p = V.fromList [E, E, S, E, S, S, S, E, W, S, E, W, S, N]
pos = (1, 1) :: Point
t = 0 :: Time
-}

