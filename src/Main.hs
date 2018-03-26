module Main where

-- TODO
-- add more user help
-- add targets/fail
-- benchmark STArray against Array for implementing flood fill
-- consider different layout options
-- create a solver

import Grid
import Colour

import qualified System.Console.ANSI as A
import Data.Array.IArray                   ( assocs, (!) )
import Text.Read                           ( readMaybe )
import Control.Monad.Loops                 ( iterateUntilM, iterateUntil, untilJust )

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
  (final, steps) <- iterateUntilM (solved . fst) run' (start, 0)
  showBoard final
  putStrLn ("Solved after " ++ show steps ++ " steps!")

run' :: (Board, Int) -> IO (Board, Int)
run' (b, c) = do
  showBoard b >> print c
  let currentCol = ungrid b ! (1,1)
  nextCol <- iterateUntil (/= currentCol) patientRead
  return (flood nextCol b, c+1)

patientRead :: Read a => IO a
patientRead = untilJust (readMaybe <$> getLine)

displayCell :: ((Int, Int), Colour) -> IO ()
displayCell ((x,y),c) = do
  A.setCursorPosition (x-1) (2*y-2)
  A.setSGR [A.SetColor A.Foreground A.Dull (colourMap c)]
  putStr "██"

showBoard :: Board -> IO ()
showBoard (Grid b) = do
  reset
  mapM_ displayCell (assocs b)
  A.setSGR [A.Reset]
  A.cursorDownLine 2
