
module Main
       (
         main,
       )
       where

import Server
import BookLocations

import System.IO
  (hGetLine, hClose, hPutStrLn, hSetBuffering, BufferMode(..),
   Handle)

computeRequisition :: Handle -> HostName -> PortNumber -> IO ()
computeRequisition h _ _ = do
  hSetBuffering h LineBuffering

  bl <- readBookLocations

  putStr "Receiving function name... " -- verbose
  fn <- hGetLine h
  putStrLn fn                             -- verbose
  putStr "Sending location addresses... " -- verbose
  let la = getLocations bl fn
  print la                      -- verbose
  hPutStrLn h $ show la

  hClose h

main :: IO ()
main = serverMain computeRequisition
