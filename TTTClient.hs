import Network.Socket
import Network.BSD
import System.IO
import TTTServer
import TicTacToe


playGame :: HostName -> String -> IO ()
playGame hostname port = do
  p <- getPlayerFromConnection hostname port
  playersTurn p 
  return ()


playersTurn :: Player -> IO ()
playersTurn p = do
  m1 <- hGetLine $ handle p
  putStrLn $ "did reveive msg from server: " ++ m1
  col <- getLine
  hPutStrLn (handle p) col
  m2 <- hGetLine $ handle p
  putStrLn $ "did reveive msg from server: " ++ m2
  row <- getLine
  hPutStrLn (handle p) row
  playersTurn p
  return ()

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

  info <- hGetLine h
  putStrLn $ "initial info: " ++ info

  return $ Player h Cross

