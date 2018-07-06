module Grid (
  Grid(..), randomGrid, flood1, flood2, flood3, isSolved, area
  ) where

import System.Random                       ( RandomGen, Random(random) )
import Data.Array.IArray                   ( Array, bounds, inRange, listArray, elems, (!), (//) )
import Data.Array.ST                       ( readArray, writeArray, runSTArray, thaw )
import Control.Monad                       ( when, unless, replicateM )
import Control.Monad.Trans.State           ( State, evalState, execState, modify, get, state )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
-- import Data.List.Split                     ( chunksOf )

newtype Grid a = Grid { ungrid :: Array (Int, Int) a }
  deriving Show

randomGrid :: (Random a, RandomGen g) => Int -> Int -> State g (Grid a)
randomGrid m n = (Grid . listArray ((1,1), (m,n))) <$> replicateM (m*n) (state random)

flood1 :: Eq a => a -> Grid a -> Grid a
flood1 newColour (Grid a) = Grid $ runSTArray $ do
  mArr <- thaw a
  targetColour <- readArray mArr (1, 1)
  unless (targetColour == newColour) $ do
    let go node = when (inRange bound node) $ do
          currentColour <- readArray mArr node
          when (currentColour == targetColour) $ do
            writeArray mArr node newColour
            mapM_ go $ neighbours node
    go (1, 1)
  return mArr
  where bound = bounds a

type Pos = (Int,Int)

-- improve using Set, a single update (and maybe a sneaky intset)
flood2 :: Eq a => a -> Grid a -> Grid a
flood2 newColour (Grid a) = Grid go
  where targetColour = a ! (1,1)
        go = if targetColour == newColour
                then a
                else run (1,1) a
        run node b = if inRange (bounds b) node && b ! node == targetColour
                        then let b0 = b // [(node, newColour)]
                                 b1 = run (neighbours node !! 0) b0
                                 b2 = run (neighbours node !! 1) b1
                                 b3 = run (neighbours node !! 2) b2
                                 b4 = run (neighbours node !! 3) b3
                              in b4
                         else b

flood3 :: Eq a => a -> Grid a -> Grid a
flood3 newColour (Grid a) = Grid go
  where go = if targetColour == newColour
                then a
                else a // zip modifications (repeat newColour)
        targetColour = a ! (1,1)
        modifications = Set.toList $ execState (run (1,1)) (Set.empty)
        run :: Pos -> State (Set Pos) ()
        run node = do
          used <- get
          when (node `Set.notMember` used && inRange (bounds a) node && a ! node == targetColour) $ do
            modify (Set.insert node)
            traverse_ run (neighbours node)

neighbours :: Pos -> [Pos]
neighbours (x,y) = [(x,y+1), (x, y-1), (x-1,y), (x+1,y)]

isSolved :: Eq a => Grid a -> Bool
isSolved (Grid arr) = constant (elems arr)
  where constant xs = all (== head xs) (tail xs)

area :: Eq a => Grid a -> Int
area (Grid b) = evalState (go (1,1)) Set.empty
  where
    target = b ! (1,1)
    go :: Pos -> State (Set Pos) Int
    go now = do
      seen <- get
      if inRange (bounds b) now && now `Set.notMember` seen && b ! now == target
         then do
           modify (Set.insert now)
           rest <- traverse go (neighbours now)
           return (sum rest + 1)
         else return 0
