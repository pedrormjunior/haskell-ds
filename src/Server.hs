
module Server
       (
         module Network,
         serverMain,
       )
       where

import System.IO
  (hGetLine, hClose, hPutStrLn, hSetBuffering,
   BufferMode(..), Handle, stdout, IOMode(..), openFile, hIsEOF)
import Network
import Control.Concurrent
  (forkIO)

import MaybeExceptions
import ConfigFile

serverMain ::
  (Handle -> HostName -> PortNumber -> IO ()) -> IO ()
serverMain fcr = withSocketsDo $ do
  cf <- readConfigFile
  let ($>$) :: String -> (String -> a) -> a
      ($>$) s f = configLookup cf s f

      server_port = "serverPort" $>$ readPort

  hSetBuffering stdout LineBuffering
  sock <- listenOn (PortNumber server_port)
  let serverMain' :: IO ()
      serverMain' = withSocketsDo $ do
        putStrLn "Awaiting connection..." -- verbose
        (h, host, port) <- accept sock
        putStrLn $
          "Received connection from " ++ (prepareHost host) ++
          ":" ++ show port -- verbose
  
        forkIO $
          catchIO $ fcr h (prepareHost host) port
        serverMain'
  serverMain'

prepareHost :: String -> String
prepareHost s = prepareHost' s s
  where prepareHost' (x:xs) res =
          prepareHost' xs (if x == ':' then xs else res)
        prepareHost' [] res     = res
