module Main where

-- TODO
-- add more user help
-- add targets/fail
-- benchmark STArray against Array for implementing flood fill
-- consider different layout options
-- create a solver

import Grid                                ( Grid(..), flood, isSolved )
import Colour                              ( Board, Colour(..), randomBoardIO )

import qualified System.Console.ANSI as A
import Data.Array.IArray                   ( assocs, (!) )
import Text.Read                           ( readMaybe )
import Control.Monad.Loops                 ( iterateUntilM, iterateUntil, untilJust )

type GameState = (Board, Int)

colourMap :: Colour -> A.Color
colourMap c = case c of
              Red -> A.Red; Green -> A.Green; Yellow -> A.Yellow; Blue -> A.Blue; Magenta -> A.Magenta; White -> A.White

reset :: IO ()
reset = A.clearScreen >> A.setCursorPosition 0 0

main :: IO ()
main = do
  reset
  putStrLn "Size?"
  n <- readLn
  start <- randomBoardIO n n
  (final, steps) <- iterateUntilM (isSolved . fst) (step (n+1)) (start, 0)
  showBoard (n+1) (final, steps)
  statusLine (n+1) ("Solved after " ++ show steps ++ " steps!")

step :: Int -> GameState -> IO GameState
step t (b,c) = do
  showBoard t (b,c)
  let currentCol = ungrid b ! (1,1)
  nextCol <- iterateUntil (/= currentCol) (patientRead t)
  return (flood nextCol b, c+1)

statusLine :: Int -> String -> IO ()
statusLine n m = A.setCursorPosition n 0 >> putStrLn m

patientRead :: Int -> IO Colour
patientRead t = untilJustMsg (readMaybe <$> getLine) (statusLine t "Couldn't understand that colour, try again?")

untilJustMsg :: Monad m => m (Maybe a) -> m () -> m a
untilJustMsg m t = go
  where go = m >>= maybe (t >> go) return

displayCell :: ((Int, Int), Colour) -> IO ()
displayCell ((x,y),c) = do
  A.setCursorPosition (x-1) (2*y-2)
  A.setSGR [A.SetColor A.Foreground A.Dull (colourMap c)]
  putStr "██"

showBoard :: Int -> GameState -> IO ()
showBoard t ((Grid b),n) = do
  reset
  mapM_ displayCell (assocs b)
  A.setSGR [A.Reset]
  statusLine t (show n)
