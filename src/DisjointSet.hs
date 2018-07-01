module DisjointSet (discrete, findRoot, getRoot, getSize, union, toSets, DisjointSet, addSingleton, getLabel, findLabel, mergeUnsafe) 
  where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad

data Element a = Element { par :: a
                         , siz :: Int
                         }
                         deriving (Eq, Show)

type DisjointSet a = Map a (Element a)

discrete :: Ord a => [a] -> DisjointSet a
discrete = foldr addSingleton M.empty

addSingleton :: Ord a => a -> DisjointSet a -> DisjointSet a
addSingleton x = M.insert x (Element x 1)

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
getParent x = par . (! x)

getSize :: Ord a => a -> DisjointSet a -> Int
getSize x = siz . (! x)

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

type LabelledSet a b = (DisjointSet a, Map a b)

getLabel :: Ord a => a -> LabelledSet a b -> b
getLabel x (s,m) = m ! getRoot x s

findLabel :: Ord a => a -> State (LabelledSet a b) b
findLabel x = do
  (s,m) <- get
  let (p,s') = runState (findRoot x) s
  put (s',m)
  return $ m ! p

-- merges both sets, labelling with one of the two originals
mergeUnsafe :: Ord a => a -> a -> State (LabelledSet a b) ()
mergeUnsafe x y = do
  (s,m) <- get
  let ((x',y'),s') = runState ((,) <$> findRoot x <*> findRoot y) s
  let (z,s'') = runState (union x y >> findRoot x) s'
  let m' = if z == x'
             then M.delete y' m
             else M.delete x' m
  put (s'',m')

mergeResult :: Ord a => a -> a -> b -> State (LabelledSet a b) ()
mergeResult x y c = do
  (s,m) <- get
  let ((x',y'),s') = runState ((,) <$> findRoot x <*> findRoot y) s
  let (z,s'') = runState (union x y >> findRoot x) s'
  let m' = if z == x'
             then M.insert x' c . M.delete y' $ m
             else M.insert y' c . M.delete x' m
  put (s'',m')
