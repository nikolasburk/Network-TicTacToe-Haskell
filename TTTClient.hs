import Network.Socket
import Network.BSD
import Control.Monad
import System.IO
import TTTServer
import TicTacToe
import Board

playGame :: HostName -> String -> IO ()
playGame hostname port = do
  p <- getPlayerFromConnection hostname port
  playersTurn' p 
  return ()

playersTurn' :: Player -> IO ()
playersTurn' p = do
  m <- hGetLine $ handle p
  putStrLn m
  maybeInp <- processMsg m
  case maybeInp of
    Nothing -> playersTurn' p
    Just inp  -> do
                   hPutStrLn (handle p) inp 
                   playersTurn' p

processMsg :: String -> IO (Maybe String)
processMsg s = let msg = stringToMsg s
                   in case msgType msg of
                        REQ_INPUT -> do
                          inp <- getLine
                          return $ Just inp
                        _ -> return Nothing

getPlayerFromConnection :: HostName -> String -> IO Player
getPlayerFromConnection hostname port = do

  -- | create the server's address using the input parameters
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  putStrLn $ "did create server address: " ++ show serveraddr

  -- | create a TCP socket for the incoming data
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  -- | configure socket
  setSocketOption sock KeepAlive 1

  -- | connect to server
  connect sock (addrAddress serveraddr)

  -- | turn socket into handle
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering

  --info <- hGetLine h
  --putStrLn $ "initial info: " ++ info

  return $ Player h Cross

