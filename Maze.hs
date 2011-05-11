module Maze
where

import Control.Arrow
import Control.Monad.State
import Data.Array.ST
import System.Random

import Types

{-
Generates the entire maze.
-}
--genMaze :: Size -> State StdGen (GHC.Arr.Array Size Cell)
genMaze s@(sx, sy) = do
  (ews, ups) <- gMP s
  return $ build sx sy ews ups

{-
Builds the maze using Sidewinder's algorithm.
-}
--build :: Length -> Length -> [Point] -> [Point] -> GHC.Arr.Array Size Cell
build sx sy ews ups = runSTArray $ do
  m <- newListArray ((1, 1), (sy, sx)) $ repeat $ C [E, W]
  -- 1. Block eastern walls (including first row's end)
  mapM (blockCell m E) $ (sx, 1) : ews
  -- 2. Block western walls of corridors
  mapM (blockCell m W) $ map (first (+1)) $ filter (fst . first (/= sx)) $ ews
  -- 3. Block starts of rows.
  mapM (blockCell m W) $ map (\y -> (1, y)) [1 .. sy]
  -- 4. Open northwards.
  mapM (openCell m N) $ ups
  -- 5. Open southwards
  mapM (openCell m S) $ map (second (\x -> x - 1)) ups
  return m

{-
Block one cell from the maze, represented as an array.
-}
-- blockCell :: (MArray a Cell m) => a Size Cell -> Cardinal -> Size -> m ()
blockCell m d (x, y) = do
  e <- readArray m (y, x)
  writeArray m (y, x) $ block e d

{-
Open one cell from the maze, represented as an array.
-}
-- openCell :: (MArray a Cell m) => a Size Cell -> Cardinal -> Size -> m ()
openCell m d (x, y) = do
  e <- readArray m (y, x)
  writeArray m (y, x) $ open e d

{-
Generates all the important points in the maze. Receives size of maze and
returns a tuple with cells where the eastern corridor ends and where the
northwards openings are placed.
-}
gMP :: Size -> State StdGen ([Point], [Point])
gMP (sx, sy) = do
  points <- mapM (gRP 0 sx) [2..sy]
  return $ foldl (\(x, y) (a, b) -> (x ++ a, y ++ b)) ([], []) points

{-
Generates the important point for a row. Receives current position, length of
row and row order and returns a tuple containing a list of cells where the
eastern corridor should end and a list of cells where northwards openings
should be placed.
-}
gRP :: Coord -> Length -> Coord -> State StdGen ([Point], [Point])
gRP c sx y
  | sx <= 0 = return ([], [])
  | otherwise = do
    len <- state $ randomR (1, sx)
    up <- state $ randomR (1, len)
    (rx, ry) <- gRP (c + len) (sx - len) y
    return ((len + c, y):rx, (up + c, y):ry)

{- Block a cell from one direction. -}
block :: Cell -> Cardinal -> Cell
block (C l) x = C $ filter (/= x) l

{- Open a cell to one direction. -}
open :: Cell -> Cardinal -> Cell
open (C l) x = C $ if x `elem` l then l else x : l

