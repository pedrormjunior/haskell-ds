
module Test
       -- (
       -- )
       where

import System.Timeout
import Control.Exception

import Prelude hiding (catch)

ioBool :: IO Bool
ioBool = do
  putStrLn "ioBool"
  return True

main :: IO ()
main = do
  b <- timeout 60 ioBool
  print b

f :: IO Int
f = undefined

test = catch f (\e -> do
                   let err = show (e :: SomeException)
                   putStrLn $ "Teste " ++ err
                   return 2)
