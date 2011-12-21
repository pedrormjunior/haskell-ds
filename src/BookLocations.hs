
module BookLocations
       (
         readBookLocations,
         writeBookLocations,
         getLocations,
         updateLocations,
       )
       where

import System.IO
  (hGetLine, hClose, hPutStrLn, hSetBuffering, BufferMode(..),
   Handle, stdout, IOMode(..), openFile, hIsEOF)
import Data.Map as Map
import Data.List as List (intercalate, union)

type BookLocations = Map String [String]

book_locations :: FilePath
book_locations = "BookLocations.dat"

readBookLocations :: IO BookLocations
readBookLocations = do
  h <- openFile book_locations ReadMode
  l <- readLocationsFile h
  hClose h
  return $ Map.fromList l
  
readLocationsFile :: Handle -> IO [(String, [String])]
readLocationsFile h = hIsEOF h >>= \eof ->
  if eof then return [] else do
    fn <- hGetLine h
    line_las <- hGetLine h
    let las = words line_las
    remaining <- readLocationsFile h
    return $ (fn, las) : remaining

writeBookLocations :: BookLocations -> IO ()
writeBookLocations bl = let
  fns  = Map.keys bl
  llns = Map.elems bl
  in do
    h <- openFile book_locations WriteMode
    writeLocationsFile h fns llns
    hClose h

writeLocationsFile :: Handle -> [String] -> [[String]] -> IO ()
writeLocationsFile h (fn:fns) (lln:llns) = do
  hPutStrLn h fn
  hPutStrLn h $ intercalate " " lln
  writeLocationsFile h fns llns
writeLocationsFile _ _ _                 = return ()

getLocations :: BookLocations -> String -> Maybe [String]
getLocations bl fn = Map.lookup fn bl

updateLocations ::
  String -> BookLocations -> String -> BookLocations
updateLocations adrs bl fn =  case Map.lookup fn bl of
  Nothing    -> Map.insert fn [adrs] bl
  Just adrss -> Map.insert fn (List.union adrss [adrs]) bl
