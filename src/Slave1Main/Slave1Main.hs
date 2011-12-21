
module Main
       (
         main,
       )
       where

import Slave
import qualified Computations.Fibonacci as Fib
import qualified Computations.Tree as Tree
import qualified Computations.Sudoku as Sudoku

main :: IO ()
main = slaveMain [("fibonacci", Fib.compute),
                  ("treeCharToInt", Tree.compute),
                  ("sudoku", Sudoku.compute)]
