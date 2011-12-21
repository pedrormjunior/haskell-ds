
module Main
       (
         main,
       )
       where

import Client
import Data.Array
import Computations.Tree

treeExample :: Tree Char
treeExample =
  Node
  (Node (Node (Node
               (Leaf '!')
               (Leaf '!'))
         (Node
          (Leaf '!')
          (Leaf '!')))
   (Node (Node
          (Leaf '!')
          (Leaf 'd'))
    (Node
     (Leaf 'l')
     (Leaf 'r'))))
  (Node (Node (Node
               (Leaf 'o')
               (Leaf 'W'))
         (Node
          (Leaf ' ')
          (Leaf 'o')))
   (Node (Node
          (Leaf 'l')
          (Leaf 'l'))
    (Node
     (Leaf 'e')
     (Leaf 'H'))))

main :: IO ()
main = do
  t1 <- "fibonacci" $/ 30
  t2 <- "treeCharToInt" $/ treeExample
  t3 <- "sudoku" $/
        ".......21" ++
        "43......." ++
        "6........" ++
        "2.15....." ++
        ".....637." ++
        "........." ++
        ".68...4.." ++
        "...23...." ++
        "....7...."

  result1 <- t1 $\ (read :: String -> Int)
  result2 <- t2 $\ (read :: String -> Tree Int)
  result3 <- t3 $\ (read :: String
                            -> Maybe (Array (Char, Char) [Char]))

  putStr "Result1: "
  print result1
  putStr "Result2: "
  print result2
  putStr "Result3: "
  print result3
