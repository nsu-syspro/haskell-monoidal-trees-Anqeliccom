{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.Seq where

import Common.Sequence
import Common.MonoidalTree

import Task1 (Measured(..), Size(..))
import Task2.Tree

-- * Sequence definition

-- | Random-access sequence based on binary tree
newtype Seq a = Seq { getTree :: Tree (Size a) (Elem a) }
  deriving (Show, Eq)

-- | Sequence element wrapper
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Eq)

-- | Measures given element as 'Size 1'
instance Measured (Size a) (Elem a) where
  measure _ = Size 1 

instance Foldable Seq where
  foldMap f = foldMap (f . getElem) . getTree

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length (Seq t) = getSize (measure t :: Size a)

-- * Sequence instance

instance Sequence Seq where
  empty = Seq Empty
  toSequence = foldr (+|) empty 
  x +| Seq t =  Seq (Elem x <| t)
  Seq t |+ x = Seq (t |> Elem x)

  insertAt :: forall a. Int -> a -> Seq a -> Seq a
  insertAt i v (Seq tree) = Seq (go (max 0 i) tree)
    where
      go _ Empty    = leaf (Elem v)
      go 0 t        = branch (leaf (Elem v)) t
      go _ (Leaf x) = branch (leaf x) (leaf (Elem v))
      go j (Branch _ l r)
        | j < leftSize = branch (go j l) r
        | otherwise    = branch l (go (j - leftSize) r)
        where
          leftSize = getLen l

  removeAt :: forall a. Int -> Seq a -> Seq a
  removeAt i seqT@(Seq tree)
    | i < 0 || i >= length seqT = Seq tree
    | otherwise                 = Seq (go i tree)
    where
      go _ Empty    = Empty
      go _ (Leaf _) = Empty
      go j (Branch _ l r)
        | j < leftSize = branch (go j l) r
        | otherwise    = branch l (go (j - leftSize) r)
        where
          leftSize = getLen l

  elemAt :: forall a. Int -> Seq a -> Maybe a
  elemAt i (Seq tree) = go i tree
    where
      go _ Empty    = Nothing
      go 0 (Leaf x) = Just (getElem x)
      go _ (Leaf _) = Nothing
      go j (Branch _ l r)
        | j < leftSize = go j l
        | otherwise    = go (j - leftSize) r
        where
          leftSize = getLen l

getLen :: forall a. Tree (Size a) (Elem a) -> Int
getLen t = getSize (measure t :: Size a)
