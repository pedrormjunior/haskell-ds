
module ConfigFile
       (
         readConfigFile,
         configLookup,
         -- * Read Functions
         readIP,
         readPort,
         readTimeout,
       )
       where

import System.IO
  (openFile, IOMode(..), Handle, hIsEOF, hGetLine, hClose)
import Data.Map as Map
  (Map, empty, insert, lookup)
import Network
  (PortNumber)

type ConfigFile = Map String String

config_file :: FilePath
config_file = "ConfigFile.dat"

readConfigFile :: IO ConfigFile
readConfigFile = do
  h <- openFile config_file ReadMode
  readConfigFile' h empty
  
readConfigFile' :: Handle -> ConfigFile -> IO ConfigFile
readConfigFile' h cf = hIsEOF h >>= \eof ->
  if eof then hClose h >> return cf else do
    line <- hGetLine h
    let (keyword, value) = break' ':' line
    readConfigFile' h (insert keyword value cf)
    
break' :: (Eq a) => a -> [a] -> ([a], [a])
break' c as = case break (== c) as of
  (l, r) -> if null r then (l, r) else (l, tail r)

configLookup :: ConfigFile -> String -> (String -> a) -> a
configLookup cf key f = maybe undefined f $ Map.lookup key cf

readIP :: String -> String
readIP = id

readPort :: String -> PortNumber
readPort = fromIntegral . read

readTimeout :: String -> Int
readTimeout = read
