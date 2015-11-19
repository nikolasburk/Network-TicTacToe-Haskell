import Network.Socket
import Network.BSD
import System.IO
import TTTServer
import TicTacToe

openConnection :: HostName -> String -> IO Player
openConnection hostname port = do
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  setSocketOption sock KeepAlive 1

  connect sock (addrAddress serveraddr)

  h <- socketToHandle sock ReadWriteMode

  hSetBuffering h LineBuffering

  messages <- hGetContents h
  putStrLn messages
  
  return $ Player h Cross

