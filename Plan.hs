module Plan
where

import Control.Arrow
import Control.Monad.State
import System.Random

import qualified Array as A
import qualified Data.Vector as V

import Maze
import Types

import Debug.Trace

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
{-
A plan to leave the maze. If the plane ends and the time is not spent, the
plan is restarted. This allows for simple plans while having complicated
mazes.
-}
data Plan
  = Go Cardinal -- go in one direction
  | Seq Plan Plan -- do one plan, then another
  | If Cond Plan Plan -- do one plan or another
  deriving (Show, Read)

{-
A condition to use in a plan. The only input is the neighborhood.
-}
data Cond
  = Free Cardinal -- cell in that direction is reachable
  | Blocked Cardinal -- way blocked
  | Or Cond Cond -- one condition or another
  | And Cond Cond -- both conditions
  deriving (Show, Read)

{-
The state used while patrolling the maze:
  * the original plan (used for plan looping)
  * the actual plan
  * the maze used in evolution
  * start point
  * end point
-}
type MazeState = (Plan, Plan, Maze, Point, Point)
-}

m = fst $ (runState $ genMaze (5, 5)) (mkStdGen 42)
p = V.fromList [E, E, S, E, S, S, S, E, W, S, E, W, S, N]
pos = (1, 1) :: Point
t = 0 :: Time

{-
getFitness :: Plan -> (Plan, Fitness)
getFitness p = undefined

--executePlan :: Plan -> Maze
executePlan = undefined

f = do
  state $ \(m, p, e, cp) -> ((cp, 0), (m, p, e, cp))
-}

