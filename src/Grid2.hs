module Grid2 (
  -- Grid2(..), randomGrid, flood4, isSolved, listsToGrid, neighbours
  ) where

import Control.Monad                       ( when, unless, replicateM )
import Control.Monad.State

import DisjointSet

type Pos = (Int,Int)


-- listsToGrid :: [[a]] -> DisjointSet a
-- listsToGrid l = Grid $ listArray ((1,1), (x,y)) (concat l)
--   where x = length l
--         y = minimum . map length $ l
