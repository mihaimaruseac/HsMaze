module Types
where

{- Size of maze -}
type Length = Int
type Coord = Int
type Size = (Length, Length)
type Point = Size
type Dir = Size

{- The cardinal directions. -}
data Cardinal = N | E | S | W
  deriving (Eq, Show, Read, Ord, Enum)

{- A cell. The list contains the openings. -}
newtype Cell = C [Cardinal]
  deriving (Show, Read)

{- Block a cell from one direction. -}
block :: Cell -> Cardinal -> Cell
block (C l) x = C $ filter (/= x) l

{- Open a cell to one direction. -}
open :: Cell -> Cardinal -> Cell
open (C l) x = C $ if x `elem` l then l else x : l

