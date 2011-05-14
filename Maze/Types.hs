module Maze.Types
where

{-
Common types are presented in this file.
-}

{- Small typedefs -}
type Length = Int
type Coord = Int
type Size = (Length, Length)
type Point = Size
type Dir = Size

{- The cardinal directions. -}
data Cardinal = N | E | S | W deriving (Eq, Show, Read, Ord, Enum)

