{-# LANGUAGE ApplicativeDo, FlexibleContexts #-}
module DisjointSet
  where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Foldable
-- import Control.Applicative

data Element a = Element { par :: a
                         , siz :: Int
                         }
                         deriving (Eq, Show)

type DS a = Map a (Element a)
type DisjointSet a = State (DS a)

discrete :: Ord a => [a] -> DS a
discrete = foldr addSingleton M.empty

addSingleton :: Ord a => a -> DS a -> DS a
addSingleton x = M.insert x (Element x 1)

findRoot :: Ord a => a -> DisjointSet a a
findRoot x = do
  p <- getParent x
  if (p == x)
     then return p
     else do
       p' <- findRoot p
       setParent x p'
       return p'

setParent :: Ord a => a -> a -> DisjointSet a ()
setParent x y = modify' (M.adjust (\c -> c {par = y}) x)

setSize :: Ord a => a -> Int -> DisjointSet a ()
setSize x n = modify' (M.adjust (\c -> c {siz = n}) x)

getParent :: Ord a => a -> DisjointSet a a
getParent x = gets $ par . (! x)

getSize :: Ord a => a -> DisjointSet a Int
getSize x = gets $ siz . (! x)

union :: Ord a => a -> a -> DisjointSet a ()
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

toSets :: Ord a => DisjointSet a [[a]]
toSets = map reverse . M.elems <$> helper

helper :: Ord a => DisjointSet a (Map a [a])
helper = do
  vals <- gets M.keys
  roots <- traverse findRoot vals
  return $ M.fromListWith (++) [(v,[k]) | (k,v) <- zip vals roots]

type LS a b = StateT (Map a b) (DisjointSet a)
type LabelledSet a b = State (DS a, Map a b)

findLabel :: Ord a => a -> LS a b b
findLabel = fmap snd . findRootAndLabel

mergeHelper :: Ord a => a -> a -> MaybeT (LS a b) a
mergeHelper x y = do
  (xRoot,yRoot) <- lift . lift $ (,) <$> findRoot x <*> findRoot y
  guard (xRoot /= yRoot)
  z <- lift . lift $ union x y >> findRoot x
  modify $ M.delete $
    if z == xRoot
       then yRoot
       else xRoot
  return z

-- merges both sets, labelling with one of the two originals
mergeUnsafe :: Ord a => a -> a -> LS a b ()
mergeUnsafe x y = void $ runMaybeT $ mergeHelper x y

mergeResult :: Ord a => a -> a -> b -> LS a b ()
mergeResult x y c = void $ runMaybeT $ mergeHelper x y >>= modify . (`M.insert` c)

mergeSame :: (Ord a, Eq b) => a -> a -> LS a b ()
mergeSame x y = do
  equal <- (==) <$> findLabel x <*> findLabel y
  when equal $ mergeUnsafe x y

-- toLSs :: Ord a => LS a b [([a],b)]
findRootAndLabel :: Ord a => a -> LS a b (a,b)
findRootAndLabel x = do
  p <- lift $ findRoot x
  l <- gets (!p)
  return (p,l)

wrap :: LS a b c -> LabelledSet a b c
wrap f = state $ \(s,m) ->
  let ((c,m'),s') = runState (runStateT f m) s
   in (c,(s',m'))

toLabelledSets :: Ord a => LS a b [([a], b)]
toLabelledSets = do
  rootedSets <- lift helper
  m <- get
  return [(reverse vals, label) | (root,vals) <- M.toAscList rootedSets, let label = m ! root]

type Graph a = [(a,[a])]

discreteLabels' :: Ord a => [(a,b)] -> (DS a, Map a b)
discreteLabels' assign = (discrete $ fst <$> assign, M.fromAscList assign)

fromLabelledElements' :: (Ord a, Eq b) => Graph a -> [(a,b)] -> (DS a, Map a b)
fromLabelledElements' gr = execState (wrap combine) . discreteLabels'
  where combine = traverse_ (uncurry mergeSame) [(x,y) | (x,ys) <- gr, y <- ys]

lineGraph :: Int -> Graph Int
lineGraph n = [(i, adj i) | i <- [1..n]]
  where adj i
          | i == 1 = [2]
          | i == n = [n-1]
          | otherwise = [i-1, i+1]

gridGraph :: Int -> Int -> Graph (Int,Int)
gridGraph m n = [(k, adj k) | i <- [1..m], j <- [1..n], let k = (i,j)]
  where adj (i,j) = filter inRange [(i-1, j), (i, j-1), (i, j+1), (i+1, j)]
        inRange (i,j) = i >= 1 && i <= m && j >= 1 && j <= n
