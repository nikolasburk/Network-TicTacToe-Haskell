module Board where


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

boardToList :: Board -> [Field]
boardToList (BoardCons r0 r1 r2) = rowToList r0 ++ rowToList r1 ++ rowToList r2

rowToList :: Row -> [Field]
rowToList (RowCons f0 f1 f2) = [f0, f1, f2]


-- | creates the initial empty board with only empty fields
initialBoard :: Board
initialBoard = let initialRows = replicate 3 (RowCons Empty Empty Empty)
                 in case boardFromRows initialRows of
                    (Right b) -> b


boardFromRows :: [Row] -> BoardOrMsg
boardFromRows rows
  | length rows /= 3 = Left ("Not the right number of rows: " ++ show (length rows))
  | otherwise = Right (BoardCons (rows !! 0) (rows !! 1) (rows !! 2))


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


-- HELPERS: Board encoding / decoding

decodeBoard :: String -> BoardOrMsg
decodeBoard s = boardFromRows $ snd $ foldl dec (0, [], []) s 
  where snd (_, x, _) = x

dec :: (Int, [Row], [Field])-> Char -> (Int, [Row], [Field])
dec (i, rows, fields) c
  | (i+1) `mod` 3 == 0 = let newRow = RowCons (fields !! 0) (fields !! 1) (charToField c)
                     in (i+1, rows ++ [newRow], [])
  | otherwise  = let newField = charToField c 
                     in (i+1, rows, fields ++ [newField])

charToField :: Char -> Field
charToField c 
  | c == 'O'  = FieldCons Circle
  | c == 'X'  = FieldCons Cross
  | otherwise = Empty

fieldToChar :: Field -> Char
fieldToChar Empty = 'E'
fieldToChar (FieldCons m)
  | m == Circle = 'O'
  | m == Cross = 'X'

encodeBoard :: Board -> String
encodeBoard b = foldl (\acc x -> acc ++ [fieldToChar x]) "" (boardToList b)

