module TicTacToe where

import Board

-- | let the player make a choice
mkChoice :: Choice -> Board -> BoardOrMsg
mkChoice c@(col, row, m) b 
  | validChoice = Right $ replaceRow r row b
  | otherwise = Left $ "Choice " ++ show c ++ " is not allowed."
  where validChoice = isValidIndex (col, row) && isEmpty (col, row) b
        r = newRow (col, m) (rowAtIndex row b)

currentPlayer :: Board -> Marker
currentPlayer b
  | numberOfMarkers b Cross <= numberOfMarkers b Circle = Cross
  | otherwise = Circle

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
  
