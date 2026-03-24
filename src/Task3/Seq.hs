{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Seq where

import Common.Sequence
import Common.MonoidalTree

import Task1 (Measured(..), Size(..))
import Task3.Tree (Tree(..), leaf, node2, node3, InsertResult(..))

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
  insertAt i v (Seq tree) = case go (max 0 i) tree of
    Same t     -> Seq t
    Kicked l r -> Seq (node2 l r)
    where
      go _ Empty    = Same (leaf (Elem v))
      go 0 t        = Kicked (leaf (Elem v)) t
      go _ (Leaf x) = Kicked (leaf x) (leaf (Elem v))
      go j (Node2 _ l r)
        | j < leftSize = case go j l of
            Same l'      -> Same (node2 l' r)
            Kicked l1 l2 -> Same (node3 l1 l2 r)
        | otherwise     = case go (j - leftSize) r of
            Same r'      -> Same (node2 l r')
            Kicked r1 r2 -> Same (node3 l r1 r2)
        where
          leftSize = getLen l
      go j (Node3 _ l m r)
        | j < leftSize = case go j l of
            Same l'      -> Same (node3 l' m r)
            Kicked l1 l2 -> Kicked (node2 l1 l2) (node2 m r)
        | j < leftSize + midSize = case go (j - leftSize) m of
            Same m'      -> Same (node3 l m' r)
            Kicked m1 m2 -> Kicked (node2 l m1) (node2 m2 r)
        | otherwise    = case go (j - leftSize - midSize) r of
            Same r'      -> Same (node3 l m r')
            Kicked r1 r2 -> Kicked (node2 l m) (node2 r1 r2)
        where
          leftSize = getLen l
          midSize  = getLen m

  removeAt :: forall a. Int -> Seq a -> Seq a
  removeAt i seqT@(Seq tree)
    | i < 0 || i >= length seqT = Seq tree
    | otherwise                 = case go i tree of
        Deleted t   -> Seq t
        Collapsed t -> Seq t
    where
      go _ Empty    = Deleted Empty
      go _ (Leaf _) = Collapsed Empty
      go j (Node2 _ l r)
        | j < leftSize = case go j l of
            Deleted l'   -> Deleted (node2 l' r)
            Collapsed l' -> case r of
              Empty            -> Collapsed l'
              Leaf x           -> Collapsed (leaf x)
              Node2 _ rl rr    -> Collapsed (node3 l' rl rr)
              Node3 _ rl rm rr -> Deleted (node2 (node2 l' rl) (node2 rm rr))
        | otherwise    = case go (j - leftSize) r of
            Deleted r'   -> Deleted (node2 l r')
            Collapsed r' -> case l of
              Empty            -> Collapsed r'
              Leaf x           -> Collapsed (leaf x)
              Node2 _ ll lr    -> Collapsed (node3 ll lr r')
              Node3 _ ll lm lr -> Deleted (node2 (node2 ll lm) (node2 lr r'))
        where leftSize = getLen l

      go j (Node3 _ l m r)
        | j < leftSize = case go j l of
            Deleted l'   -> Deleted (node3 l' m r)
            Collapsed l' -> case m of
              Empty            -> Deleted r
              Leaf x           -> Deleted (node2 (leaf x) r)
              Node2 _ ml mr    -> Deleted (node2 (node3 l' ml mr) r)
              Node3 _ ml mm mr -> Deleted (node3 (node2 l' ml) (node2 mm mr) r)
        | j < leftSize + midSize = case go (j - leftSize) m of
            Deleted m'   -> Deleted (node3 l m' r)
            Collapsed m' -> case r of
              Empty            -> Deleted l
              Leaf x           -> Deleted (node2 l (leaf x))
              Node2 _ rl rr    -> Deleted (node2 l (node3 m' rl rr))
              Node3 _ rl rm rr -> Deleted (node3 l (node2 m' rl) (node2 rm rr)) 
        | otherwise    = case go (j - leftSize - midSize) r of
            Deleted r'   -> Deleted (node3 l m r')
            Collapsed r' -> case m of
              Empty            -> Deleted l
              Leaf x           -> Deleted (node2 l (leaf x))
              Node2 _ ml mr    -> Deleted (node2 l (node3 ml mr r'))
              Node3 _ ml mm mr -> Deleted (node3 l (node2 ml mm) (node2 mr r'))
        where
          leftSize = getLen l
          midSize  = getLen m

  elemAt :: forall a. Int -> Seq a -> Maybe a
  elemAt i (Seq tree) = go i tree
    where
      go _ Empty    = Nothing
      go 0 (Leaf x) = Just (getElem x)
      go _ (Leaf _) = Nothing
      go j (Node2 _ l r)
        | j < leftSize = go j l
        | otherwise    = go (j - leftSize) r
        where leftSize = getSize (measure l :: Size a)
      go j (Node3 _ l m r)
        | j < leftSize           = go j l
        | j < leftSize + midSize = go (j - leftSize) m
        | otherwise              = go (j - leftSize - midSize) r
        where
          leftSize = getLen l
          midSize  = getLen m

getLen :: forall a. Tree (Size a) (Elem a) -> Int
getLen t = getSize (measure t :: Size a)

data RemoveResult m a
  = Deleted (Tree m a)
  | Collapsed (Tree m a)
