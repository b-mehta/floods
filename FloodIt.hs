{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

import qualified System.Console.ANSI as A
import System.Random                       ( Random(..), RandomGen )
import Control.Monad                       ( when, unless, replicateM, forever )
import Control.Monad.State                 ( State, evalState, state, liftIO )
import Control.Monad.Trans.State.Strict    ( StateT, modify', get, evalStateT )
import Data.Array.IArray                   ( Array, bounds, inRange, listArray, elems )
import Data.Array.ST                       ( readArray, writeArray, runSTArray, thaw )
import Data.List.Split                     ( chunksOf )
import Control.Arrow                       ( (***) )
import System.Random                       ( newStdGen )

data Colour = Red | Green | Yellow | Blue | Magenta | White
  deriving (Bounded, Enum, Eq, Read)

instance Random Colour where
  random g = randomR (minBound,maxBound) g
  randomR (a,b) g =
    case (randomR (fromEnum a, fromEnum b) g) of
      (x, g') -> (toEnum x, g')

newtype Grid a = Grid { ungrid :: Array (Int, Int) a }
type Board = Grid Colour

instance Show Board where
  show g = showsGrid (gridToLists g) ""
 
colourMap :: Colour -> A.Color
colourMap Red = A.Red
colourMap Green = A.Green
colourMap Yellow = A.Yellow
colourMap Blue = A.Blue
colourMap Magenta = A.Magenta
colourMap White = A.White
 
showsGrid :: [[Colour]] -> ShowS
showsGrid g = tail . foldr1 (.) (map showsLine g)
  where prepend t = ('\n':) . t
        showsLine = prepend . foldr1 (.) . map showsColour
        showsColour c = (A.setSGRCode [A.SetColor A.Foreground A.Dull (colourMap c)] ++) . ("██"++)

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
    let go node = do
        when (inRange bound node) $ do
          currentColour <- readArray mArr node
          when (currentColour == targetColour) $ do
            writeArray mArr node newColour
            mapM_ go $ neighbours node
    go (1, 1)
  return mArr
  where bound = bounds a

neighbours :: Num i => (i, i) -> [(i, i)]
neighbours (x,y) = [(x,y+1), (x, y-1), (x-1,y), (x+1,y)]

floods :: Eq a => [a] -> Grid a -> Grid a
floods = foldr1 (.) . map flood
 
reset :: IO ()
reset = A.clearScreen >> A.setCursorPosition 0 0

ask :: Read a => String -> IO a
ask s = putStrLn s >> readLn

main :: IO ()
main = do
  n <- ask "Size?"
  reset
  start <- evalState (randomGrid n n) <$> newStdGen :: IO Board
  evalStateT (forever test) (start, 0)

test :: StateT (Board, Int) IO ()
test = do
  current <- get
  inp <- liftIO (reset >> print (fst current) >> print (snd current) >> readLn)
  modify' (flood inp *** (+1))
