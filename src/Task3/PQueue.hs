{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.PQueue where

import Common.PriorityQueue
import Common.MonoidalTree

import Task1 (Measured(..), MinMax(..), Min(..), Max(..))
import Task3.Tree

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)


-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v) }
  deriving (Show, Eq)

instance Ord k => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = MinMax (Min k, Max k)

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldr (\(k, v) q -> insert k v q) empty 
  entries (PQueue tree) = foldr (\e res -> getEntry e : res) [] tree
  insert k v (PQueue tree) =  PQueue (tree |> Entry (k, v))
 
  extractMin :: Ord k => PQueue k v -> Maybe (v, PQueue k v)
  extractMin = extractBy f (<=)
    where
      f (MinMax (Min k, _))  = k
      f (MinMax (PosInf, _)) = error ".."

  extractMax :: Ord k => PQueue k v -> Maybe (v, PQueue k v)
  extractMax = extractBy f (>=)
    where
      f (MinMax (_, Max k))  = k
      f (MinMax (_, NegInf)) = error ".."

extractBy :: forall k v. Ord k => (MinMax k -> k) -> (k -> k -> Bool) -> PQueue k v -> Maybe (v, PQueue k v)
extractBy _ _ (PQueue Empty)  = Nothing
extractBy f cmp (PQueue tree) = Just (go tree)
  where
    go Empty    = error ".."
    go (Leaf e) = (snd (getEntry e), PQueue Empty)
    go (Node2 _ l r)
      | cmp (getP l) (getP r) = let (v, PQueue t) = go l
                                in (v, PQueue (case t of
                                                Empty -> r
                                                _     -> node2 t r))
      | otherwise             = let (v, PQueue t) = go r
                                in (v, PQueue (case t of
                                                Empty -> l
                                                _     -> node2 l t))
    go (Node3 _ l m r)
      | cmp (getP l) (getP m) && cmp (getP l) (getP r) = let (v, PQueue t) = go l
                                                         in (v, PQueue (case t of
                                                                          Empty -> node2 m r
                                                                          _     -> node3 t m r))
      | cmp (getP m) (getP r)                          = let (v, PQueue t) = go m
                                                         in (v, PQueue (case t of
                                                                          Empty -> node2 l r
                                                                          _     -> node3 l t r))
      | otherwise                                      = let (v, PQueue t) = go r
                                                         in (v, PQueue (case t of
                                                                          Empty -> node2 l m
                                                                          _     -> node3 l m t))
    getP t = f (measure t :: MinMax k)
