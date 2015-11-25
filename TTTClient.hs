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
  --m <- hGetLine $ handle p
  --putStrLn m
  msg <- receiveMsg p
  maybeInp <- processMsg msg
  case maybeInp of
    Nothing -> playersTurn' p
    Just inp  -> do
                   hPutStrLn (handle p) inp 
                   playersTurn' p

receiveMsg :: Player -> IO Message
receiveMsg p = do
  m <- hGetLine $ handle p
  let msg = stringToMsg m
  case msgType msg of
    --BOARD -> putStrLn ("BOARD:\n" ++ (show (decodeBoard (content msg))))
    BOARD -> putStrLn ("BOARD:\n" ++ show (decodeBoard (tail $ content msg)))
    _ -> print msg
  return msg

processMsg :: Message-> IO (Maybe String)
processMsg msg = case msgType msg of
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

  return $ Player h Cross

