
module MaybeExceptions
       (
         -- * Functions
         catchMaybe,
         catchMaybeJoin,
         catchIO,
         timeout,               -- From "System.Timeout"
       )
       where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad (join)
import System.Timeout (timeout)

catchMaybe :: IO a -> IO (Maybe a)
catchMaybe comp =
  (comp >>= (return . return)) `catch` f
  where f :: SomeException -> IO (Maybe a)
        f e = do putStrLn $ "catchMaybe: " ++ show e -- verbose
                 return Nothing

catchMaybeJoin :: IO (Maybe a) -> IO (Maybe a)
catchMaybeJoin comp = catchMaybe comp >>= return . join

catchIO :: IO () -> IO ()
catchIO comp = 
  comp `catch` f
  where f :: SomeException -> IO ()
        f e = putStrLn $ "catchIO: " ++ show e -- verbose
