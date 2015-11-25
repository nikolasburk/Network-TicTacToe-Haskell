import TicTacToe
import System.Exit
import Text.Read
import Board

main = tictactoe $ Right initialBoard

tictactoe :: BoardOrMsg -> IO ()
tictactoe bmsg = case bmsg of
                      (Left msg) -> putStrLn msg
                      (Right b) -> nextRound b

nextRound :: Board -> IO ()
nextRound b = do

  -- check if one of the players won the game
  case checkWinnerPure b of
    Nothing -> putStrLn $ "Player's turn: " ++ (show $ currentPlayer b)
    Just p -> do
      putStrLn $ "Player " ++ show p ++ " won the game"
      exitSuccess

  -- determine current player and output info
  let p = currentPlayer b
  putStrLn $ show b

  -- get choice from input
  col <- getInput "Input column: "
  row <- getInput "Input row: "

  -- make choice on board
  case mkChoice (col,row,p) b of
       Left msg -> do
         putStrLn msg
         nextRound b
       Right b' -> nextRound b'


getInput :: String -> IO Int
getInput msg = do
  putStr msg
  inp <- getLine
  case readMaybe inp of
    Nothing -> getInput msg
    Just n -> return n
