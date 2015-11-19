module TicTacToe where


-- | represents the choice of a player
data Marker = Circle | Cross 
  deriving (Eq, Show)

-- | fields make up the board
data Field = Empty | FieldCons Marker 
  deriving (Eq)

-- | a row consists of three fields
data Row = RowCons Field Field Field

-- | a board consists of three rows
data Board = BoardCons Row Row Row

-- | convenience type definitions
type BoardOrMsg = Either String Board
type Choice = (Int, Int, Marker)

instance Show Field where
  show (FieldCons m)
    | m == Circle = "O"
    | otherwise   = "X"
  show Empty = " "

instance Show Row where
  show (RowCons f0 f1 f2) = show f0 ++ " " ++ show f1 ++ " " ++ show f2

instance Show Board where
  show (BoardCons r0 r1 r2) =  show r0 ++ "\n" ++ show r1 ++ "\n" ++ show r2 

-- | creates the initial empty board with only empty fields
initialBoard :: Board
initialBoard = let initialRows = replicate 3 (RowCons Empty Empty Empty)
                 in case boardFromRows initialRows of
                    (Right b) -> b

boardFromRows :: [Row] -> BoardOrMsg
boardFromRows rows
  | length rows /= 3 = Left ("Not the right number of rows: " ++ show (length rows))
  | otherwise = Right (BoardCons (rows !! 0) (rows !! 1) (rows !! 2))


-- | let the player make a choice
mkChoice :: Choice -> Board -> BoardOrMsg
mkChoice c@(col, row, m) b 
  | validChoice = Right $ replaceRow r row b
  | otherwise = Left $ "Choice " ++ show c ++ " is not allowed."
  where validChoice = isEmpty (col, row) b && isValidIndex (col, row) 
        r = newRow (col, m) (rowAtIndex row b)


-- checks the range of the indices
isValidIndex :: (Int, Int) -> Bool
isValidIndex (col, row) = col < 3 && row < 3 

-- checks if the field at the given index is free
isEmpty :: (Int, Int) -> Board -> Bool
isEmpty (col, row) b = 
    let fields = boardToList b
        index  = row*3 + col
        in fields !! index == Empty

-- returns the row at the specified index from the board
rowAtIndex :: Int -> Board -> Row
rowAtIndex i (BoardCons r0 r1 r2)
  | i == 0 = r0
  | i == 1 = r1
  | i == 2 = r2

-- | replace a given row in the board row
replaceRow :: Row -> Int -> Board -> Board
replaceRow newRow i (BoardCons r0 r1 r2) 
  | i == 0 = (BoardCons newRow r1 r2)
  | i == 1 = (BoardCons r0 newRow r2) 
  | i == 2 = (BoardCons r0 r1 newRow) 

currentPlayer :: Board -> Marker
currentPlayer b
  | numberOfMarkers b Cross <= numberOfMarkers b Circle = Cross
  | otherwise = Circle

numberOfMarkers :: Board -> Marker -> Int
numberOfMarkers b m = let fieldList = boardToList b
                        in length $ filter (hasMarker m) fieldList

hasMarker :: Marker -> Field -> Bool
hasMarker _ Empty = False
hasMarker m (FieldCons m') = m == m' 

-- | helper function to create a new row
newRow :: (Int, Marker) -> Row -> Row
newRow (col, m) (RowCons f0 f1 f2) 
  | col == 0 = RowCons (FieldCons m) f1 f2
  | col == 1 = RowCons f0 (FieldCons m) f2
  | col == 2 = RowCons f0 f1 (FieldCons m)

checkWinner :: Board -> BoardOrMsg
checkWinner b = 
  let w0 = checkRows b in
  case w0 of
    Just m -> Left $ "Player won: " ++ show m 
    Nothing -> let w1 = checkCols b in
      case w1 of
        Just m -> Left $ "Player won: " ++ show m 
        Nothing -> let w2 = checkDiagonals b in
          case w2 of
            Just m -> Left $ "Player won: " ++ show m 
            Nothing -> return b
            
checkWinner' :: BoardOrMsg -> Maybe Marker
checkWinner' (Right b)    = checkWinnerPure b
checkWinner' (Left msg) = Nothing

checkWinnerPure :: Board -> Maybe Marker
checkWinnerPure b = let res0 = checkRows b in
                case res0 of
                  Just _ -> res0
                  Nothing -> let res1 = checkCols b in
                    case res1 of
                      Just _ -> res1
                      Nothing -> let res2 = checkDiagonals b in
                        case res2 of
                          Just _ -> res2
                          Nothing -> Nothing


checkRows :: Board -> Maybe Marker
checkRows (BoardCons r0 r1 r2) = let res0 = checkRow r0 in
                case res0 of
                  Just _ -> res0
                  Nothing -> let res1 = checkRow r1 in
                    case res1 of
                      Just _ -> res1
                      Nothing -> let res2 = checkRow r2 in
                        case res2 of
                          Just _ -> res2
                          Nothing -> Nothing


checkRow :: Row -> Maybe Marker
checkRow (RowCons f0 f1 f2) 
  | f0 == f1 && f1 == f2  = case f0 of
    Empty              -> Nothing
    (FieldCons Cross)  -> Just Cross
    (FieldCons Circle) -> Just Circle
  | otherwise             = Nothing


checkCols :: Board -> Maybe Marker
checkCols b = let res0 = checkFirstCol b in
                case res0 of
                  Just _ -> res0
                  Nothing -> let res1 = checkSecondCol b in
                    case res1 of
                      Just _ -> res1
                      Nothing -> let res2 = checkThirdCol b in
                        case res2 of
                          Just _ -> res2
                          Nothing -> Nothing

checkFirstCol :: Board -> Maybe Marker
checkFirstCol (BoardCons (RowCons f0 _ _) (RowCons f1 _ _) (RowCons f2 _ _)) 
  | f0 == f1 && f1 == f2 = case f0 of
    Empty              -> Nothing
    (FieldCons Cross)  -> Just Cross
    (FieldCons Circle) -> Just Circle
  | otherwise = Nothing
  
checkSecondCol :: Board -> Maybe Marker
checkSecondCol (BoardCons (RowCons _ f0 _) (RowCons _ f1 _) (RowCons _ f2 _)) 
  | f0 == f1 && f1 == f2 = case f0 of
    Empty              -> Nothing
    (FieldCons Cross)  -> Just Cross
    (FieldCons Circle) -> Just Circle
  | otherwise = Nothing
  
checkThirdCol :: Board -> Maybe Marker
checkThirdCol (BoardCons (RowCons _ _ f0) (RowCons _ _ f1) (RowCons _ _ f2)) 
  | f0 == f1 && f1 == f2 = case f0 of
    Empty              -> Nothing
    (FieldCons Cross)  -> Just Cross
    (FieldCons Circle) -> Just Circle
  | otherwise = Nothing

checkDiagonals :: Board -> Maybe Marker
checkDiagonals (BoardCons (RowCons f00 _ f10) (RowCons _ f1 _) (RowCons f12 _ f02)) 
  | f00 == f1 && f1 == f02 = case f00 of
    Empty              -> Nothing
    (FieldCons Cross)  -> Just Cross
    (FieldCons Circle) -> Just Circle
  | f10 == f1 && f1 == f12 = case f10 of
    Empty              -> Nothing
    (FieldCons Cross)  -> Just Cross
    (FieldCons Circle) -> Just Circle
  | otherwise = Nothing
  

boardToList :: Board -> [Field]
boardToList (BoardCons r0 r1 r2) = rowToList r0 ++ rowToList r1 ++ rowToList r2

rowToList :: Row -> [Field]
rowToList (RowCons f0 f1 f2) = [f0, f1, f2]
