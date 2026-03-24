{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Tree where

import Common.MonoidalTree

import Task1 (Measured(..))

-- * 2-3 tree definition

-- | 2-3 tree with values 'a' in leaves
-- Intermediate nodes contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Node2 m (Tree m a) (Tree m a)
  | Node3 m (Tree m a) (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty           = mempty
  measure (Leaf x)        = measure x
  measure (Node2 m _ _)   = m
  measure (Node3 m _ _ _) = m

instance Foldable (Tree m) where
  foldMap _ Empty            = mempty
  foldMap f (Leaf x)         = f x
  foldMap f (Node2 _ l r)    = foldMap f l <> foldMap f r
  foldMap f (Node3 _ l m r)  = foldMap f l <> foldMap f m <> foldMap f r

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

node2 :: Measured m a => Tree m a -> Tree m a -> Tree m a
node2 l r     = Node2 (measure l <> measure r) l r

node3 :: Measured m a => Tree m a -> Tree m a -> Tree m a -> Tree m a
node3 l m r     = Node3 (measure l <> measure m <> measure r) l m r

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = foldr (<|) Empty

  x <| tree = case insertLeft (Leaf x) tree of
    Same t      -> t
    Kicked l r  -> node2 l r

  tree |> x = case insertRight (Leaf x) tree of
    Same t      -> t
    Kicked l r  -> node2 l r 

data InsertResult m a
  = Same (Tree m a)
  | Kicked (Tree m a) (Tree m a)

insertLeft :: Measured m a => Tree m a -> Tree m a -> InsertResult m a
insertLeft new Empty    = Same new
insertLeft new (Leaf x) = Kicked new (Leaf x)
insertLeft new (Node2 _ l r) = case insertLeft new l of
  Same l'       -> Same (node2 l' r)
  Kicked l1 l2  -> Same (node3 l1 l2 r)
insertLeft new (Node3 _ l m r) = case insertLeft new l of
  Same l'       -> Same (node3 l' m r)
  Kicked l1 l2  -> Kicked (node2 l1 l2) (node2 m r) 

insertRight :: Measured m a => Tree m a -> Tree m a -> InsertResult m a
insertRight new Empty    = Same new
insertRight new (Leaf x) = Kicked (Leaf x) new
insertRight new (Node2 _ l r) = case insertRight new r of
  Same r'       -> Same (node2 l r')
  Kicked r1 r2  -> Same (node3 l r1 r2)
insertRight new (Node3 _ l m r) = case insertRight new r of
  Same r'       -> Same (node3 l m r')
  Kicked r1 r2  -> Kicked (node2 l m) (node2 r1 r2)
