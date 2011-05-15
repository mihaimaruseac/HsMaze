module Maze.Plan
where

-- TODO: limit the exported symbols

import Control.Arrow
import Control.Monad.State
import Data.Array.ST
import Data.List (sortBy)
import System.Random

import qualified Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Maze.Maze
import Maze.Types

import Debug.Trace

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
--takeAct (C l) d p = if d `elem` l then move d p else p
takeAct c@(C l) d p
  = if d `elem` l then move d p else if E `elem` l then move E p
  else if S `elem` l then move S p else p

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
Fitness weights
-}
fTIME = 100
fDIST = -3
fBDIST = -5
fAREA = 1

{-
Computes the fitness of a plan.
-}
fitness :: Point -> Time -> Point -> Time -> Int -> Fitness
fitness p t ep et bd
  = fTIME * (et - t)
  + fDIST * manhattan p ep
  + fBDIST * bd
  + fAREA * fst p * snd p
  where
    s = snd ep

{-
Gets the manhattan distance between two points.
-}
manhattan :: Point -> Point -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

{-
Returns a new population from an older one, via crossover and mutation.
-}
newPopulation :: V.Vector (Plan, Fitness) -> Double -> State StdGen (V.Vector Plan)
newPopulation p mRate = do
  let sp = sortBy (\(x, y) (x', y') -> y' `compare` y) $ V.toList p
  let len = length sp
  let slots = V.fromList $ getSlots 1 1 $ reverse sp
  let numSlots = snd . V.last $ slots
  ps <- replicateM len (selectFromPopulation numSlots slots)
  nps <- mapM cross $ group2 ps
  newPlans <- mapM (mutate mRate) $ ungroup2 nps
  return $ V.fromList $ newPlans

{-
Does the crossover between two chromosomes.
-}
cross :: (Plan, Plan) -> State StdGen (Plan, Plan)
cross (p1, p2) = do
  let l = V.length p1
  r <- state $ randomR (1, l - 1)
  let v1 = V.generate l (\x -> if x < r then p1 V.! x else p2 V.! x)
  let v2 = V.generate l (\x -> if x < r then p2 V.! x else p1 V.! x)
  return (v1, v2)

{-
Does the mutation of a chromosome.
-}
mutate :: Double -> Plan -> State StdGen Plan
mutate mRate p = do
  pmutate <- state random
  if pmutate > mRate then return p else do
    let l = V.length p
    r <- state $ randomR (0, l - 1)
    rv <- getRandomDir
    return $ mutatePlan p r rv

{-
Does the actual mutation of a plan.
-}
mutatePlan :: Plan -> Int -> Cardinal -> Plan
mutatePlan p ix nv = V.modify (\v -> VM.write v ix nv) p

{-
Selects one guy to participate in next population.
-}
selectFromPopulation :: Int -> V.Vector (Plan, Int) -> State StdGen Plan
selectFromPopulation m ps = do
  r <- state $ randomR (0, m)
  return $ findRoulette r ps

{-
Finds the actual chromosome via roulette.
-}
findRoulette :: Int -> V.Vector (Plan, Int) -> Plan
findRoulette ix ps = fst . V.head $ V.dropWhile (\(x, y) -> ix > y) ps

{-
Gets the slots for each vector.
-}
getSlots :: Int -> Int -> [(Plan, Fitness)] -> [(Plan, Int)]
getSlots _ _ [] = []
getSlots ix s ((p, f) : ps) = (p, s) : getSlots (ix + 1) (s + ix + 1) ps

{-
Transforms a list of an even number of elements into a list of pairs from
adjacent elements. It the list has an odd number of elements, the last one is
ignored.
-}
group2 :: [a] -> [(a, a)]
group2 (x:y:rs) = (x, y) : group2 rs
group2 _ = []

{-
Inverse of the above operation. Always yields a list with an even number of
elements (thus, it is not really an inverse in the other case).
-}
ungroup2 :: [(a, a)] -> [a]
ungroup2 ((x, y) : xys) = x : y : ungroup2 xys
ungroup2 _ = []

