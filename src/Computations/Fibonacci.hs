
module Computations.Fibonacci
       (
         compute,
       )
       where

compute :: String -> String
compute = show . fibonacci . read

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
