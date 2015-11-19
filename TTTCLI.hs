import TicTacToe

main = tictactoe $ Right initialBoard

tictactoe :: BoardOrMsg -> IO ()
tictactoe bmsg = case bmsg of
                      (Left msg) -> putStrLn msg
                      (Right b) -> nextRound b


nextRound :: Board -> IO ()
nextRound b = do
  case checkWinnerPure b of
    Nothing -> putStrLn $ "Player's turn: " ++ (show $ currentPlayer b)
    Just p -> do
      putStrLn $ "Player " ++ show p ++ " won the game"
      tictactoe $ Right initialBoard 
  let p = currentPlayer b
  putStrLn $ show b
  putStr "Column: "
  col <- getLine
  let colNum = read col
  putStr "Row: "
  row <- getLine
  let rowNum = read row
  case mkChoice (colNum,rowNum,p) b of
       Left msg -> do
         putStrLn msg
         nextRound b
       Right b' -> nextRound b'
  
  
