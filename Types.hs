module Types
where

{- Size of maze -}
type Length = Int
type Coord = Int
type Size = (Length, Length)
type Point = Size
type Dir = Size

{- The cardinal directions. -}
data Cardinal = N | E | S | W deriving (Eq, Show, Read, Ord, Enum)

{- A cell. The list contains the openings. -}
newtype Cell = C [Cardinal] deriving (Show, Read)

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

