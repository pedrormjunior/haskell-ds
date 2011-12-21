
module Slave
       (
         module Network,
         slaveMain,
       )
       where

import Network
import System.IO
  (hGetLine, Handle, hSetBuffering, BufferMode(..), stdout,
   hPutStrLn, hClose)

import Server
import MaybeExceptions
import ConfigFile

computeRequisition :: [(String, String -> String)]
  -> Handle -> HostName -> PortNumber -> IO ()
computeRequisition l h _ _ = do
  hSetBuffering h LineBuffering

  fn <- hGetLine h
  param <- hGetLine h
  maybe (sendResult h "") (\f -> sendResult h $ f param)
    (lookup fn l)
  hClose h
  putStrLn "computeRequisition: Handle closed."

sendResult :: Handle -> String -> IO ()
sendResult h result = do
  hPutStrLn h result
  putStrLn "sendResult: result sent." -- verbose

-- | Unused for now.
compute :: (Read a, Show b) => (a -> b) -> String -> String
compute f = show . f . read

slaveMain :: [(String, String -> String)] -> IO ()
slaveMain arg = do
  cf <- readConfigFile
  let ($>$) :: String -> (String -> a) -> a
      ($>$) s f = configLookup cf s f

      server_port  = "serverPort"  $>$ readPort
      updater_ip   = "updaterIP"   $>$ readIP
      updater_port = "updaterPort" $>$ readPort
      computations = map fst arg

      notifyServer :: IO ()
      notifyServer = do
        hSetBuffering stdout LineBuffering
  
        h <- connectTo updater_ip (PortNumber updater_port)
        hSetBuffering h LineBuffering
  
        putStrLn "Sending port information..." -- vervose
        hPutStrLn h $ show $ show server_port
        putStrLn "Sending list of functions..." -- verbose
        hPutStrLn h $ show computations
  
        hClose h

  catchIO $ notifyServer
  serverMain (computeRequisition arg)
