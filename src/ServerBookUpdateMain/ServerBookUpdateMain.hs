
module Main
       (
         main,
       )
       where

import Server
import BookLocations

import System.IO
  (hGetLine, hClose, hPutStrLn, hSetBuffering, BufferMode(..),
   Handle, stdout)
import qualified Data.Map as Map
import qualified Data.List as List (union)

computeRequisition :: Handle -> HostName -> PortNumber -> IO ()
computeRequisition h host _ = do
  hSetBuffering h LineBuffering

  bl <- readBookLocations

  putStr "Receiving Slave address... " -- verbose
  port <- hGetLine h
  let adrs = host ++ ":" ++ (read port :: String)
  putStrLn adrs                         -- verbose
  putStr "Receiving function names... " -- verbose
  fns_str <- hGetLine h
  putStrLn fns_str              -- verbose
  let fns = read fns_str :: [String]
      new_bl = foldl (updateLocations adrs) bl fns
  writeBookLocations new_bl

  hClose h

main :: IO ()
main = serverMain computeRequisition
