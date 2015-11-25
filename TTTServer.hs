module TTTServer where 

import Network.Socket
import Network.BSD
import Data.List
import System.IO
import Control.Concurrent
import Text.Read
import TicTacToe
import Board

data Player = Player { handle :: Handle,
                       marker :: Marker }

data Game = Game { player1 :: Player,
                   player2 :: Player,
                   board :: Board }

type PendingPlayers = [Handle]


serveTTT :: String -> IO ()
serveTTT port = do

  -- create the server's address
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
  let serveraddr = head addrinfos

  -- create a TCP socket
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  putStrLn $ "did create socket with server address: " ++ show serveraddr

  -- bind the socket to the address and start listening
  bind sock (addrAddress serveraddr)
  listen sock 4

  -- request processing loop (start with empty list of connections)
  acceptRequest sock []

  where
        -- | accept incoming requests
        acceptRequest :: Socket -> PendingPlayers -> IO () 
        acceptRequest mastersock handles =
          do (playersock, clientaddr) <- accept mastersock
             putStrLn $ "did receive connection from: " ++ show clientaddr
             newHandles <- handleIncomingPlayer playersock handles
             acceptRequest mastersock newHandles

        -- | handle an accepted connection
        handleIncomingPlayer :: Socket -> PendingPlayers -> IO PendingPlayers
        handleIncomingPlayer playersock handles = do
          h <- socketToHandle playersock ReadWriteMode
          hSetBuffering h LineBuffering
          let newHandles = handles ++ [h]
          if let l = length newHandles
               in even l && l > 0
             then do
                    _ <- forkIO $ initiateNewGame $  (toTuple . take 2) newHandles
                    return $ drop 2 newHandles
            else return newHandles

        -- | initiate a new game (should be called in separate thread)
        initiateNewGame :: (Handle, Handle) -> IO () 
        initiateNewGame (h1, h2) = do
          let p1 = Player h1 Cross
          let p2 = Player h2 Circle
          let game = Game p1 p2 initialBoard
          sendMessage (Message INFO "You're playing 'X'") p1
          sendMessage (Message INFO "You're playing 'O'") p2
          manageRound game
          return ()

        manageRound :: Game -> IO () 
        manageRound game = do

          -- | check if someone won the game already
          case checkWinnerPure $ board game of
            Nothing -> print $ board game
            Just m -> do
              putStrLn $ "Player " ++ show m ++ " won the game"
              handleGameOver (Just $ playerFromMarker m game) game
              return ()

--          case isFull $ board game of
--            True -> do
--                      handleGameOver Nothing game
--                      return ()
--            False -> return ()
                 
          -- | determine the current player
          let p = case currentPlayer $ board game of
                Cross -> player1 game
                Circle -> player2 game

          -- | send the current board to the player
          let bs = encodeBoard $ board game 
          putStrLn $ "encoded: " ++ bs
          sendMessage (Message BOARD bs) p

          -- | get the input from the current player
          (col, row) <- getPlayerChoice p
          case mkChoice (col, row, marker p) $ board game of
            Left msg -> do
              putStrLn msg
              sendMessage (Message INFO msg) p
              manageRound game
            Right b -> do
              let newGame = updateBoard b game
              manageRound newGame
          return ()

        -- | get input from from a player
        getPlayerChoice :: Player -> IO (Int, Int)
        getPlayerChoice p = do
          col <- getPlayerInput p "column" 
          putStrLn $ "did receive col: " ++ show col
          row <- getPlayerInput p "row"
          putStrLn $ "did receive row: " ++ show row
          sendMessage (Message INFO "Wait for other player...") p
          return (col, row)

        getPlayerInput :: Player -> String -> IO Int
        getPlayerInput p s = do
          sendMessage (Message REQ_INPUT $ "Please input " ++ s ++ ": ") p
          inp <- hGetLine (handle p)
          case readMaybe inp of
            Nothing -> getPlayerInput p s
            Just n -> return n

        handleGameOver :: Maybe Player -> Game -> IO ()
        handleGameOver p game = do
          let msgContent = case p of
                Nothing -> "The game ends with a tie"
                Just p -> "Congratulations to player '" ++ show (marker p) ++ "'"
          _ <- broadcastMessage (Message GAME_OVER msgContent) [player1 game, player2 game] 
          hClose (handle $ player1 game)
          hClose (handle $ player2 game)
          tId <- myThreadId
          killThread tId
          return ()


-- | HELPERS: Messaging

data MsgType = REQ_INPUT | INFO | GAME_OVER | BOARD | UNKNOWN deriving (Eq, Show)
data Message = Message { msgType :: MsgType,
                         content :: String }

instance Show Message where
  show m
   -- | msgType m == BOARD = "BOARD: \n" ++ show (decodeBoard (drop 1 $ content m))
   -- | msgType m == BOARD = "BOARD: \n" ++ show (decodeBoard $ content m)
    | otherwise = show (msgType m) ++ ": " ++ content m 

broadcastMessage :: Message -> [Player] -> IO [()]
broadcastMessage msg = mapM (sendMessage msg)

sendMessage :: Message -> Player -> IO ()
sendMessage m p = do
  hPrint (handle p) m
  putStrLn $ "did send message to client: " ++ show m

stringToMsg :: String -> Message
stringToMsg s 
    | isPrefix "INFO" s = Message INFO $ drop 5 s
    | isPrefix "REQ_INPUT" s = Message REQ_INPUT $ drop 10 s
    | isPrefix "BOARD" s = Message BOARD $ drop 6 s
    | otherwise = Message UNKNOWN s


-- HELPERS :: General

updateBoard :: Board -> Game -> Game
updateBoard b g = Game (player1 g) (player2 g) b

toTuple :: [a] -> (a, a)
toTuple (x:y:_) = (x, y)

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (p:ps) (x:xs)
  | p == x    = isPrefix ps xs
  | otherwise = False

playerFromMarker :: Marker -> Game -> Player
playerFromMarker m g = if marker (player1 g) == m then player1 g
                                                  else player2 g
