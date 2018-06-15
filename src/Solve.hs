import Grid                                     (flood, Grid(..), neighbours, isSolved)
import Colour                                   (Colour, randomBoardIO)
import Data.Array.IArray                        ((!), bounds, inRange)
import Data.Ord                                 (comparing)
import Data.List                                (maximumBy, takeWhile, iterate)
import qualified Data.Set as Set                (insert, notMember, empty)
import Control.Monad.Trans.State                (modify', get, evalState)
import Control.Monad

class (Enum a, Bounded a, Eq a) => Value a
instance Value Colour

data Strategy a = Strategy {play :: Grid a -> Grid a
                           ,name :: String
                           }

solution :: Value a => (Grid a -> Grid a) -> Grid a -> Int
solution = (length .) . solve

solve :: Value a => (Grid a -> Grid a) -> Grid a -> [Grid a]
solve strat = takeWhile (not . isSolved) . iterate strat

greedyArea :: Value a => Strategy a
greedyArea = Strategy { play = snd . maximumBy (comparing fst) . areas
                      , name = "Greedy area"
                      }

cycleColour :: Value a => Strategy a
cycleColour = Strategy { play = \b -> let current = ungrid b ! (1,1) in flood (loop current) b 
                       , name = "Cycle colours"
                       }

areas :: Value a => Grid a -> [(Int, Grid a)]
areas = map (\(_, t) -> (area t, t)) . step

step :: Value a => Grid a -> [(a, Grid a)]
step b = [(col, flood col b) | col <- [minBound..maxBound], col /= ungrid b ! (1,1)]

loop :: Value a => a -> a
loop c
  | c == maxBound = minBound
  | otherwise     = succ c

area :: Eq a => Grid a -> Int
area (Grid b) = evalState (go (1,1)) Set.empty
  where
    target = b ! (1,1)
    go now = do
      seen <- get
      if inRange (bounds b) now && now `Set.notMember` seen && b ! now == target
         then do
           modify' (Set.insert now)
           rest <- traverse go (neighbours now)
           return (sum rest + 1)
         else return 0

main :: IO ()
main = replicateM_ 10 $
  do
    b <- randomBoardIO 100 100
    print $ solution (play cycleColour) b
