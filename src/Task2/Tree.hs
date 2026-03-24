{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.Tree where

import Common.MonoidalTree

import Task1 (Measured(..))

-- * Binary tree definition

-- | Binary tree with values 'a' in leaves
-- Intermediate branches contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Branch m (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty          = mempty
  measure (Leaf x)       = measure x
  measure (Branch m _ _) = m

instance Foldable (Tree m) where
  foldMap _ Empty          = mempty
  foldMap f (Leaf x)       = f x
  foldMap f (Branch _ l r) = foldMap f l <> foldMap f r

-- * Smart constructors

leaf :: a -> Tree m a
leaf x = Leaf x

branch :: Measured m a => Tree m a -> Tree m a -> Tree m a
branch Empty r = r
branch l Empty = l
branch l r     = Branch (measure l <> measure r) l r

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree x = foldr (<|) Empty x

  x <| Empty = leaf x
  x <| tree  = branch (leaf x) tree

  Empty |> x = leaf x
  tree  |> x = branch tree (leaf x)
