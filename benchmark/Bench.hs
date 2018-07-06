{-# LANGUAGE StandaloneDeriving, DeriveGeneric, FlexibleInstances #-}
module Main
  where

import Gauge.Main
import Control.Monad
import Control.Monad.State
import Control.DeepSeq
import System.Random
import GHC.Generics (Generic, Generic1)

import Solve
import Grid
import Grid2
import Colour
import DisjointSet

deriving instance Generic Colour
deriving instance Generic (Grid a)
deriving instance Generic1 Grid
instance NFData Colour
instance NFData a => NFData (Grid a)

deriving instance Generic (Grid2 a)
deriving instance Generic1 Grid2
deriving instance Generic (Element a)
deriving instance Generic1 Element
instance NFData (Element (Int,Int))
instance NFData a => NFData (Grid2 a)

tester :: (Colour -> Grid Colour -> Grid Colour) -> Grid Colour -> Int
tester flooder = length . takeWhile (not . isSolved) . flip (scanl (flip flooder)) (cycle [minBound..maxBound])

makeSamples :: Int -> Int -> IO [Grid Colour]
makeSamples m n = return $ evalState (replicateM m $ randomGrid n n) (mkStdGen 0)

test :: (Colour -> Grid Colour -> Grid Colour) -> [Grid Colour] -> Benchmarkable
test = nf . map . tester

tester2 :: Grid2 Colour -> Int
tester2 = length . takeWhile (not . isSolved2) . flip (scanl (flip flood0)) (cycle [minBound..maxBound])

makeSamples2 :: Int -> Int -> IO [Grid2 Colour]
makeSamples2 m n = return $ evalState (replicateM m $ randomGrid2 n n) (mkStdGen 0)

test2 :: [Grid2 Colour] -> Benchmarkable
test2 = nf (map tester2)

main :: IO ()
main = defaultMain
          [ bgroup "STArray" [ env (makeSamples 20   5) (bench   "5" . test flood1)
                             , env (makeSamples 20  20) (bench  "20" . test flood1)
                             , env (makeSamples 20  50) (bench  "50" . test flood1)
                             , env (makeSamples 20 100) (bench "100" . test flood1)
                             ]
          -- , bgroup "recursive" [ env (makeSamples  20) (bench  "20" . test flood2)
          --                      , env (makeSamples  50) (bench  "50" . test flood2)
          --                      , env (makeSamples 100) (bench "100" . test flood2)
          --                      ]
          -- , bgroup "set" [ env (makeSamples  20) (bench  "20" . test flood3)
          --                , env (makeSamples  50) (bench  "50" . test flood3)
          --                , env (makeSamples 100) (bench "100" . test flood3)
          --                ]
            bgroup "partition" [ env (makeSamples2 20   5) (bench   "5" . test2)
                               , env (makeSamples2 20  20) (bench  "20" . test2)
                               , env (makeSamples2 20  50) (bench  "50" . test2)
                               , env (makeSamples2 20 100) (bench "100" . test2)
                               ]
          ]
-- main = defaultMain
--                    [ bench "STArray 20"  $ singleTest flood1 20
--                    , bench "STArray 50"  $ singleTest flood1 50
--                    , bench "STArray 100" $ singleTest flood1 100
--                    -- , bench "STArray 200" $ singleTest flood 200
--                    -- , bench "STArray 500" $ singleTest flood 500
--                    , bench "recursive 20"  $ singleTest flood2 20
--                    , bench "recursive 50"  $ singleTest flood2 50
--                    -- , bench "recursive 100" $ singleTest flood2 100
--                    , bench "set 20"  $ singleTest flood3 20
--                    , bench "set 50"  $ singleTest flood3 50
--                    -- , bench "recursive 100" $ singleTest flood3 100
--                    ]
