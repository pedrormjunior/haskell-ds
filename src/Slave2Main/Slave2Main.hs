
module Main
       (
         main,
       )
       where

import Slave
import qualified Computations.Fibonacci as Fib

main :: IO ()
main = slaveMain [("fibonacci", Fib.compute)]
