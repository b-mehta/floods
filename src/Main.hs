module Main where

-- TODO
-- add more user help
-- modularise
-- add targets/fail
-- benchmark STArray against Array for implementing flood fill
-- consider different layout options
-- create a solver

import qualified System.Console.ANSI as A
import System.Random                       ( Random(..), RandomGen, newStdGen )
import Control.Monad                       ( when, unless, replicateM, forever )
import Control.Monad.State                 ( State, evalState, state, liftIO )
import Control.Monad.Trans.State.Strict    ( StateT, get, evalStateT, put )
import Data.Array.IArray                   ( Array, bounds, inRange, listArray, elems, assocs )
import Data.Array.ST                       ( readArray, writeArray, runSTArray, thaw )
import Data.List.Split                     ( chunksOf )
import Control.Arrow                       ( first )
import Text.Read                           ( readMaybe ) 

data Colour = Red | Green | Yellow | Blue | Magenta | White
  deriving (Bounded, Enum, Eq, Read, Show)

instance Random Colour where
  random = randomR (minBound,maxBound)
  randomR (a,b) g = first toEnum (randomR (fromEnum a, fromEnum b) g)

newtype Grid a = Grid { ungrid :: Array (Int, Int) a }
  deriving Show
type Board = Grid Colour

colourMap :: Colour -> A.Color
colourMap c = case c of
              Red -> A.Red; Green -> A.Green; Yellow -> A.Yellow; Blue -> A.Blue; Magenta -> A.Magenta; White -> A.White
 
gridToLists :: Grid a -> [[a]]
gridToLists (Grid b) = chunksOf y (elems b)
  where y = snd (snd (bounds b))

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

floods :: Eq a => [a] -> Grid a -> Grid a
floods = foldr1 (.) . map flood
 
reset :: IO ()
reset = A.clearScreen >> A.setCursorPosition 0 0

main :: IO ()
main = do
  putStrLn "Size?"
  n <- readLn
  start <- evalState (randomGrid n n) <$> newStdGen :: IO Board
  ans <- iterateUntilM (solved . fst) run' (start, 0)
  print ans -- improve this

run' :: (Board, Int) -> IO (Board, Int)
run' (b, c) = do
  showBoard b >> print c
  nextCol <- patientRead
  return (flood nextCol b, c+1)

patientRead :: Read a => IO a
patientRead = getLine >>= maybe patientRead return . readMaybe

displayCell :: ((Int, Int), Colour) -> IO ()
displayCell ((x,y),c) = do
  A.setCursorPosition (x-1) (2*y-2)
  A.setSGR [A.SetColor A.Foreground A.Dull (colourMap c)]
  putStr "██"

showBoard :: Board -> IO ()
showBoard (Grid b) = do
  reset
  mapM_ displayCell (assocs b)
  A.setSGR [A.SetColor A.Foreground A.Dull A.White]
  A.cursorDownLine 2

solved :: Eq a => Grid a -> Bool
solved (Grid arr) = constant (elems arr)
  where constant xs = all (==head xs) (tail xs)
