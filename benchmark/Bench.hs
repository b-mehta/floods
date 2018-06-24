module Main
  where

import Gauge.Main
import Control.Monad

import Solve
import Grid
import Colour

tester :: (Colour -> Grid Colour -> Grid Colour) -> Grid Colour -> Int
tester flooder = length . takeWhile (not . isSolved) . flip (scanl (flip flooder)) (cycle [minBound..maxBound])

singleTest :: (Colour -> Grid Colour -> Grid Colour) -> Int -> Benchmarkable
singleTest flooder n = nfIO $ do
  b <- randomBoardIO n n
  return (tester flooder b)

main :: IO ()
main = defaultMain
                   [ bench "STArray 20"  $ singleTest flood 20
                   , bench "STArray 50"  $ singleTest flood 50
                   , bench "STArray 100" $ singleTest flood 100
                   -- , bench "STArray 200" $ singleTest flood 200
                   -- , bench "STArray 500" $ singleTest flood 500
                   , bench "recursive 20"  $ singleTest flood2 20
                   , bench "recursive 50"  $ singleTest flood2 50
                   -- , bench "recursive 100" $ singleTest flood2 100
                   , bench "set 20"  $ singleTest flood3 20
                   , bench "set 50"  $ singleTest flood3 50
                   -- , bench "recursive 100" $ singleTest flood3 100
                   ]
