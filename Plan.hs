module Plan
where

import Control.Monad.State
import System.Random

import Maze
import Types

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

m = fst $ (runState $ genMaze (5, 5)) (mkStdGen 42)

--executePlan :: Plan -> Maze
executePlan = undefined

f = do
  state $ \(m, p, e, cp) -> ((cp, 0), (m, p, e, cp))

