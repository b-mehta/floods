module Grid (
  Grid(..), randomGrid, flood, flood', isSolved, listsToGrid, neighbours
  ) where

import System.Random                       ( RandomGen, Random(random) )
import Data.Array.IArray                   ( Array, bounds, inRange, listArray, elems, (!), (//) )
import Data.Array.ST                       ( readArray, writeArray, runSTArray, thaw )
import Control.Monad                       ( when, unless, replicateM )
import Control.Monad.State
-- import Data.Set (Set)
-- import qualified Data.Set as S
-- import Data.List.Split                     ( chunksOf )

newtype Grid a = Grid { ungrid :: Array (Int, Int) a }
  deriving Show

-- gridToLists :: Grid a -> [[a]]
-- gridToLists (Grid b) = chunksOf y (elems b)
--   where y = snd (snd (bounds b))

listsToGrid :: [[a]] -> Grid a
listsToGrid l = Grid $ listArray ((1,1), (x,y)) (concat l)
  where x = length l
        y = minimum . map length $ l

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

type Pos = (Int,Int)

-- improve using Set, a single update (and maybe a sneaky intset)
-- bounds check!!
flood' :: Eq a => a -> Grid a -> Grid a
flood' newColour (Grid a) = Grid $ run (1,1) a
  where targetColour = a ! (1,1)
        run node b = if b ! node == targetColour
                        then let b0 = b // [(node, newColour)]
                                 b1 = run (neighbours node !! 0) b0
                                 b2 = run (neighbours node !! 1) b1
                                 b3 = run (neighbours node !! 2) b2
                                 b4 = run (neighbours node !! 3) b3
                              in b4
                         else b

neighbours :: Pos -> [Pos]
neighbours (x,y) = [(x,y+1), (x, y-1), (x-1,y), (x+1,y)]

isSolved :: Eq a => Grid a -> Bool
isSolved (Grid arr) = constant (elems arr)
  where constant xs = all (== head xs) (tail xs)
