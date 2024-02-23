module GameEngine where
import Sudoku
import Boards

-- contant representing a blank value
blank :: Char
blank = '.'

-- returns 'True' if the character is a period, 'False' otherwise
isBlank :: Char -> Bool
isBlank = (== blank)

-- gets a value from a position on the board
getValue :: Board -> Position -> Value
-- getValue bd1 (0,0)
-- getValue bd1 (0,8)
-- getValue bd1 (8,8)
getValue bd (row, col) = bd !! row !! col

-- puts a value on the board
putValue :: Board -> Square -> Board
-- putValue bd1 ((0, 0), '9')
-- putValue bd1 ((3, 5), '1')
putValue bd ((row, col), val) = replaceListElement bd (replaceListElement (bd !! row) val col) row 

-- replaces an element in a list
replaceListElement :: [a] -> a -> Int -> [a]
-- replaceListElement [1..9] 5 0
-- replaceListElement [1..9] 1 8
-- replaceListElement ['a'..'z'] 'z' 3
replaceListElement lst val index = take index lst ++ (val : drop (index + 1) lst)






