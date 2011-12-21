
module Client
       (
         compute,
         ($/),
         getResult,
         ($\),
       )
       where

import Network
import System.IO (hGetLine, hClose, hPutStrLn, hSetBuffering,
                  BufferMode(..), Handle, stdout)

import MaybeExceptions
import ConfigFile

data Ticket = Ticket String String Handle
            deriving (Eq, Show)

askForHostName :: Handle -> String -> IO [(String, String)]
askForHostName h fn = do
  hPutStrLn h fn
  lhn_maybe <- hGetLine h
  case read lhn_maybe :: Maybe [String] of
    Just lhn -> return $ map (break' ':') lhn
    Nothing -> return []

askForComputation :: Handle -> String -> String -> IO ()
askForComputation h fn param = do
  hPutStrLn h fn
  hPutStrLn h param

infix 1 $/
($/) :: (Show a) => String -> a -> IO (Maybe Ticket)
($/) = compute

compute :: (Show a) => String -> a -> IO (Maybe Ticket)
compute fn param = withSocketsDo $ catchMaybeJoin $ do
  cf <- readConfigFile
  let ($>$) :: String -> (String -> a) -> a
      ($>$) s f = configLookup cf s f

      server_port = "serverPort" $>$ readPort
      server_ip = "serverIP" $>$ readIP

  hSetBuffering stdout LineBuffering
  h_server <- connectTo server_ip (PortNumber server_port)
  hSetBuffering h_server LineBuffering
  
  l <- askForHostName h_server fn
  print l                       -- verbose
  hClose h_server

  compute' l
  where
    compute' :: [(String, String)] -> IO (Maybe Ticket)
    compute' ((slave_ip, slave_port_str):xs) = let
      slave_port :: PortNumber
      slave_port = fromIntegral (read slave_port_str :: Integer)
      in
       catchMaybe
       (connectTo slave_ip (PortNumber slave_port)) >>= maybe
       (compute' xs)
       (\h -> do
           let show_param = show param
           hSetBuffering h LineBuffering
           (catchMaybe $ askForComputation h fn show_param) >>=
             maybe
             (compute' xs) 
             (const $ return $ return (Ticket fn show_param h)))
    compute' [] = return Nothing

infix 1 $\
($\) :: Maybe Ticket -> (String -> a) -> IO (Maybe a)
($\) = getResult

getResult :: Maybe Ticket -> (String -> a) -> IO (Maybe a)
getResult Nothing _                    = return Nothing
getResult (Just (Ticket fn param h)) f = do
  cf <- readConfigFile
  let ($>$) :: String -> (String -> a) -> a
      ($>$) s f = configLookup cf s f

      server_port = "serverPort" $>$ readPort
      server_ip   = "serverIP"   $>$ readIP
      tout        = "timeout"    $>$ readTimeout

      compute' = withSocketsDo $ do
        hSetBuffering stdout LineBuffering
        putStrLn "getResult: original Ticket fail" -- verbose
        -- 
        catchMaybe $ hClose h
        -- 
        h_server <- connectTo server_ip (PortNumber server_port)
        hSetBuffering h_server LineBuffering
        -- 
        l <- askForHostName h_server fn
        print l                 -- verbose
        hClose h_server
        -- 
        compute'' l

      compute'' ((slave_ip, slave_port_str):xs) = let
        slave_port :: PortNumber
        slave_port =
          fromIntegral (read slave_port_str :: Integer)
        in catchMaybe
           (connectTo slave_ip (PortNumber slave_port)) >>= maybe
           (compute'' xs)
           (\h -> do
               hSetBuffering h LineBuffering
               catchMaybe $ askForComputation h fn param
               getResult' tout h f >>= maybe
                 (do catchMaybe $ hClose h
                     if null xs
                       then putStrLn "getResult: last fail"
                       else putStrLn "getResult: attempt fail"
                     compute'' xs)
                 (return . return))
      compute'' [] = return Nothing

  catchMaybeJoin $
    getResult' tout h f >>= maybe compute' (return . return)

getResult' :: Int -> Handle -> (String -> a) -> IO (Maybe a)
getResult' tout h f = catchMaybeJoin $
                      timeout tout $
                      hGetLine h >>= return . f

break' :: (Eq a) => a -> [a] -> ([a], [a])
break' c as = case break (== c) as of
  (l, r) -> if null r then (l, r) else (l, tail r)
