{-# LANGUAGE TupleSections #-}
module Grid2 
  where

import DisjointSet

import System.Random
import Control.Monad.State

type Grid2 = LabelledSet (Int,Int)

cell :: (RandomGen g, Random a) => b -> State g (b,a)
cell x = fmap (x,) (state random)

randomGrid2 :: (Random a, Eq a, RandomGen g) => Int -> Int -> State g (Grid2 a)
randomGrid2 m n = fromLabelledElements (gridGraph m n) <$> traverse cell indices
  where indices = (,) <$> [1..m] <*> [1..n]

flood0 :: Eq a => a -> Grid2 a -> Grid2 a
flood0 = execState . flood0'

flood0' :: Eq a => a -> State (Grid2 a) Int 
flood0' = flood (1::Int,1::Int)

isSolved2 :: Grid2 a -> Bool
isSolved2 = evalState isCoarse
