module DisjointSet
  where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad

data Element a = Element { par :: a
                         , siz :: Int
                         }
                         deriving (Eq, Show)

type DisjointSet a = Map a (Element a)

-- look :: Ord a => a -> Lens' (DisjointSet a) (Element a)
-- look x = at x . non defaultEntry

discrete :: Ord a => [a] -> DisjointSet a
discrete xs = M.fromList [(x, Element x 1) | x <- xs]

findRoot :: Ord a => a -> State (DisjointSet a) a
findRoot x = do
  p <- gets (getParent x)
  if (p == x)
     then return p
     else do
       p' <- findRoot p
       setParent x p'
       return p'

getRoot :: Ord a => a -> DisjointSet a -> a
getRoot x s
  | x == p    = x
  | otherwise = getRoot p s
  where p = getParent x s

setParent :: Ord a => a -> a -> State (DisjointSet a) ()
setParent x y = modify' (M.adjust (\c -> c {par = y}) x)

setSize :: Ord a => a -> Int -> State (DisjointSet a) ()
setSize x n = modify' (M.adjust (\c -> c {siz = n}) x)

getParent :: Ord a => a -> DisjointSet a -> a
getParent x = par . (M.! x)

getSize :: Ord a => a -> DisjointSet a -> Int
getSize x = siz . (M.! x)

union :: Ord a => a -> a -> State (DisjointSet a) ()
union x y = do
  xRoot <- findRoot x
  yRoot <- findRoot y

  let go (Element a m) (Element b n) = do
        setParent b a
        setSize a (m + n)

  unless (xRoot == yRoot) $ do
    xr <- gets (M.! xRoot)
    yr <- gets (M.! yRoot)
    if siz xr < siz yr
       then go yr xr
       else go xr yr

toSets :: Ord a => DisjointSet a -> [[a]]
toSets s = map reverse . M.elems $ M.fromListWith (++) [(v,[k]) | (k,v) <- things s]

things :: Ord a => DisjointSet a -> [(a, a)]
things s = zip vals $ evalState (traverse findRoot vals) s
  where vals = M.keys s
