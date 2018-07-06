{-# LANGUAGE ApplicativeDo, FlexibleContexts, BangPatterns #-}
module DisjointSet -- (flood, fromLabelledElements, gridGraph, findLabel, toLabelledSets, LabelledSet(..), isCoarse, Element(..))
  where

import Data.Map.Strict                      ( Map, (!) )
import Data.Set                             ( Set )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State.Strict               ( StateT, State, execState, get, put, runStateT, runState, gets, modify, MonadState )
import Control.Monad.Trans.Maybe                ( MaybeT, runMaybeT )
import Control.Monad.Trans                      ( lift )
import Control.Monad                            ( when, void, guard )
import Control.Arrow                            ( first, (***) )
import Data.Semigroup                           ( (<>) )
import Control.Monad.Loops

-- type Map = HashMap
-- type Set = HashSet

data Element a = Element { par :: !a
                         , siz :: !Int
                         }
                         deriving (Eq, Show)

type DS a = Map a (Element a)
type DisjointSet a = State (DS a)

discrete :: (Ord a) => [a] -> DS a
discrete = foldr addSingleton M.empty

addSingleton :: (Ord a) => a -> DS a -> DS a
addSingleton x = M.insert x (Element x 1)

findRoot2 :: (Ord a) => a -> DisjointSet a a
findRoot2 x = do
  p <- getParent x
  if (p == x)
     then return p
     else do
       p' <- findRoot p
       setParent x p'
       return p'

findRoot :: (Ord a) => a -> DisjointSet a a
findRoot = findRoot3

findRoot3 :: (Ord a) => a -> DisjointSet a a
findRoot3 x = do
  p <- getParent x
  let isRoot ~(a,b) = a == b
  let stepUp ~(x,p) = do gp <- getParent p
                         setParent x gp
                         return (p,gp)
  fst <$> iterateUntilM isRoot stepUp (x,p)

-- function Find(x)
--   while x.parent != x
--     x.parent := x.parent.parent
--     x := x.parent
--   return x

setParent :: (Ord a) => a -> a -> DisjointSet a ()
setParent x y = modify (M.adjust (\c -> c {par = y}) x)

setSize :: (Ord a) => a -> Int -> DisjointSet a ()
setSize x n = modify (M.adjust (\c -> c {siz = n}) x)

getParent :: (Ord a) => a -> DisjointSet a a
getParent x = gets $ par . (! x)

getSize :: (Ord a) => a -> DisjointSet a Int
getSize x = gets $ siz . (! x)

union :: (Ord a) => a -> a -> DisjointSet a ()
union x y = do
  xRoot <- findRoot x
  yRoot <- findRoot y

  let go (Element a m) (Element b n) = do
        setParent b a
        setSize a (m + n)

  when (xRoot /= yRoot) $ do
    xr <- gets (M.! xRoot)
    yr <- gets (M.! yRoot)
    if siz xr < siz yr
       then go yr xr
       else go xr yr

-- toSets :: (Ord a) => DisjointSet a [[a]]
-- toSets = map reverse . M.elems <$> helper

helper :: (Ord a) => DisjointSet a (Map a [a])
helper = do
  vals <- gets M.keys
  roots <- traverse findRoot vals
  return $ M.fromListWith (++) [(v,[k]) | (k,v) <- zip vals roots]

type AdjGraph a = Map a (Set a)
type LS a b = StateT (Map a b, AdjGraph a) (DisjointSet a)

findLabelLS :: (Ord a) => a -> LS a b b
findLabelLS = fmap snd . findRootAndLabel

mergeHelper :: (Ord a) => a -> a -> MaybeT (LS a b) a
mergeHelper x y = do
  (xRoot,yRoot) <- lift . lift $ (,) <$> findRoot x <*> findRoot y
  guard (xRoot /= yRoot)
  z <- lift . lift $ union x y >> findRoot x
  let old = if z == xRoot
              then yRoot
              else xRoot
  modify (M.delete old *** fuse z old)
  return z

fuse :: (Ord a) => a -> a -> AdjGraph a -> AdjGraph a
fuse u v g =
  let vNei = S.delete u (g ! v)
      uNei = S.delete v (g ! u)
      func = S.delete v . S.insert u
   in S.foldr (M.adjust func) (M.insert u (uNei <> vNei) . M.delete v $ g) vNei

-- merges both sets, labelling with one of the two originals
mergeUnsafe :: (Ord a) => a -> a -> LS a b ()
mergeUnsafe x y = void $ runMaybeT $ mergeHelper x y

mergeResult :: (Ord a) => a -> a -> b -> LS a b ()
mergeResult x y c = void $ runMaybeT $ mergeHelper x y >>= modify . first . (`M.insert` c)

mergeSame :: (Ord a, Eq b) => a -> a -> LS a b ()
mergeSame x y = do
  equal <- (==) <$> findLabelLS x <*> findLabelLS y
  when equal $ mergeUnsafe x y

findRootAndLabel :: (Ord a) => a -> LS a b (a,b)
findRootAndLabel x = do
  p <- lift $ findRoot x
  l <- gets $ (!p) . fst
  return (p,l)

toLabelledSetsLS :: (Ord a) => LS a b [([a], b)]
toLabelledSetsLS = do
  rootedSets <- lift helper
  (m,_) <- get
  return [(vals, label) | (root,vals) <- M.toList rootedSets, let label = m ! root]

type Graph a = [(a,Set a)]

-- lineGraph :: Int -> Graph Int
-- lineGraph n = [(i, S.fromList $ adj i) | i <- [1..n]]
--   where adj i
--           | i == 1 = [2]
--           | i == n = [n-1]
--           | otherwise = [i-1, i+1]
--
gridGraph :: Int -> Int -> Graph (Int,Int)
gridGraph m n = [(k, S.fromList $ adj k) | i <- [1..m], j <- [1..n], let k = (i,j)]
  where adj (i,j) = filter inRange [(i-1, j), (i, j-1), (i, j+1), (i+1, j)]
        inRange (i,j) = i >= 1 && i <= m && j >= 1 && j <= n

floodLS :: (Ord a, Eq b) => a -> b -> LS a b Int
floodLS x c = do
  root <- lift $ findRoot x
  (col, neigh) <- gets ((!root) *** (!root))
  when (c /= col) $ do
    (m,_) <- get
    sequence_ [mergeResult x y c | y <- S.toList neigh, m ! y == c]
  lift $ findRoot x >>= getSize

data LabelledSet a b = LabelledSet !(DS a) !(Map a b) !(AdjGraph a)

wrap :: MonadState (LabelledSet a b) m => LS a b c -> m c
wrap f = do
  LabelledSet s m g <- get
  let ((!c,(!m',!g')),!s') = runState (runStateT f (m,g)) s
  put $ LabelledSet s' m' g'
  return c

discreteLabels :: (Ord a) => Graph a -> [(a,b)] -> LabelledSet a b
discreteLabels gr assign = LabelledSet (discrete $ fst <$> assign) (M.fromList assign) (M.fromList gr)

toLabelledSets :: (Ord a, MonadState (LabelledSet a b) m) => m [([a], b)]
toLabelledSets = wrap toLabelledSetsLS

fromLabelledElements :: (Ord a, Eq b) => Graph a -> [(a,b)] -> LabelledSet a b
fromLabelledElements gr = execState (wrap combine) . discreteLabels gr
  where combine = sequence_ [mergeSame x y | (x,ys) <- gr, y <- S.toList ys]

flood :: (Ord a, Eq b, MonadState (LabelledSet a b) m) => a -> b -> m Int
flood x y = wrap $ floodLS x y

findLabel :: (Ord a, MonadState (LabelledSet a b) m) => a -> m b
findLabel x = wrap $ findLabelLS x

isCoarse :: (MonadState (LabelledSet a b) m) => m Bool
isCoarse = do
  LabelledSet _ _ g <- get
  return (M.size g == 1)
