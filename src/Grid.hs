module Grid (
  Grid(..), randomGrid, flood, isSolved
  ) where

import System.Random                       ( RandomGen, Random(random) )
import Data.Array.IArray                   ( Array, bounds, inRange, listArray, elems )
import Data.Array.ST                       ( readArray, writeArray, runSTArray, thaw )
import Control.Monad                       ( when, unless, replicateM )
import Control.Monad.State                 ( State, state )
-- import Data.List.Split                     ( chunksOf )

newtype Grid a = Grid { ungrid :: Array (Int, Int) a }
  deriving Show

-- gridToLists :: Grid a -> [[a]]
-- gridToLists (Grid b) = chunksOf y (elems b)
--   where y = snd (snd (bounds b))

listsToGrid :: [[a]] -> Grid a
listsToGrid l = Grid $ listArray ((1,1), (x,y)) (concat l)
  where x = length l
        y = length (head l)

randomGrid :: (Random a, RandomGen g) => Int -> Int -> State g (Grid a)
randomGrid m n = listsToGrid <$> replicateM m (replicateM n (state random))

flood :: Eq a => a -> Grid a -> Grid a
flood newColour (Grid a) = Grid $ runSTArray $ do
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
        neighbours (x,y) = [(x,y+1), (x, y-1), (x-1,y), (x+1,y)]

-- floods :: Eq a => [a] -> Grid a -> Grid a
-- floods = foldr1 (.) . map flood

isSolved :: Eq a => Grid a -> Bool
isSolved (Grid arr) = constant (elems arr)
  where constant xs = all (== head xs) (tail xs)
