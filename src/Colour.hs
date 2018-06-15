module Colour where
  
import Grid                                ( Grid, randomGrid )
import System.Random                       ( Random(..), newStdGen )
import Control.Arrow                       ( first )
import Control.Monad.State                 ( evalState )

type Board = Grid Colour

data Colour = Red | Yellow | Green | Cyan | Blue | Violet
  deriving (Bounded, Enum, Eq, Read, Show)

instance Random Colour where
  random = randomR (minBound,maxBound)
  randomR (a,b) g = first toEnum (randomR (fromEnum a, fromEnum b) g)

randomBoardIO :: Int -> Int -> IO Board
randomBoardIO m n = evalState (randomGrid m n) <$> newStdGen
