
module Computations.Tree
       (
         Tree(..),
         compute,
       )
       where

import Data.Char (ord)

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
            deriving (Eq, Ord, Show, Read)
                     
compute :: String -> String
compute = show . treeCharToInt . read

treeCharToInt :: Tree Char -> Tree Int
treeCharToInt (Node l r) =
  Node (treeCharToInt l) (treeCharToInt r)
treeCharToInt (Leaf x)   = Leaf (ord x)
