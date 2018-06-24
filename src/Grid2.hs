module Grid2 (
  Grid2 (..), randomGrid, flood, flood2, flood3, isSolved, listsToGrid, neighbours
  ) where

import Control.Monad                       ( when, unless, replicateM )
import Control.Monad.State

import DisjointSet
