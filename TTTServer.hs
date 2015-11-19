module TTTServer where 

import Network.Socket
import Network.BSD
import Data.List
import System.IO
import Control.Concurrent
import TicTacToe

data Player = Player { handle :: Handle,
                       marker :: Marker }

data Game = Game { player1 :: Player,
                   player2 :: Player,
                   boardOrMsg :: BoardOrMsg }

type HandlerFunc = SockAddr -> String -> IO ()

serveTTT :: String -> HandlerFunc -> IO ()
serveTTT port handlerfunc = do 
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  bind sock (addrAddress serveraddr)

  listen sock 4

  processRequest sock []

  where
        -- | process incoming requests
        processRequest :: Socket -> [Handle] -> IO () 
        processRequest mastersock handles =
          do (playersock, clientaddr) <- accept mastersock
             putStrLn $ "did receive connection from: " ++ show clientaddr
             newHandles <- handlePlayer playersock handles
             processRequest mastersock newHandles

        -- | handle an accepted connection
        handlePlayer :: Socket -> [Handle] -> IO [Handle]
        handlePlayer playersock handles = do
          h <- socketToHandle playersock ReadWriteMode
          let newHandles = handles ++ [h]
          if let l = length newHandles
               in even l && l > 0
             then do
                    initiateNewGame $ toTuple $ lastN 2 newHandles
                    return $ take ((length newHandles) - 2) newHandles
            else return newHandles

        -- | initiate a new game
        initiateNewGame :: (Handle, Handle) -> IO () 
        initiateNewGame (h1, h2) = do
          let player1 = Player h1 Cross
          let player2 = Player h2 Circle
          let game = Game player1 player2 (Right initialBoard)
          sendMessage "Let the game begin" player1 
          sendMessage "Let the game begin" player2 
          return ()


-- | Helpers

sendMessage :: String -> Player -> IO ()
sendMessage msg p = do
  hPutStrLn (handle p) msg
  hFlush (handle p)

toTuple :: [a] -> (a, a)
toTuple (x:y:xs) = (x, y)

zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs
