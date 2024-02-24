module Game where
import Sudoku ( Position, Value, Board, Square )
import Boards ()

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
replaceListElement lst element index = take index lst ++ (element : drop (index + 1) lst)

-- constant for board display
cellBorder :: [Char]
cellBorder = "+-----------+-----------+-----------+\n"

-- constant for board display
cellGap :: [Char]
cellGap = "|           |           |           |\n"

-- formats a board for console display
formatBoard :: Board -> [Char]
formatBoard bd =
    cellBorder ++ formatRows bd ++ cellBorder

-- formats all board rows for console display
formatRows :: Board -> [Char]
formatRows bd = formatRows' bd 0
    where
    formatRows' [] _ = []
    formatRows' (h:t) acc
        | acc == 5 || acc == 11 = cellBorder ++ formatRows' (h:t) (acc + 1) 
        | odd acc =  cellGap ++ formatRows' (h:t) (acc + 1) 
        | otherwise = formatRow h ++ formatRows' t (acc + 1)

-- formats a single board row for console display
formatRow :: [Value] -> [Char]
formatRow [c0,c1,c2,c3,c4,c5,c6,c7,c8] =
    "| " ++ (c0 : "   ") ++ (c1 : "   ") ++ (c2 : " |") ++
    " " ++ (c3 : "   ") ++ (c4 : "   ") ++ (c5 :  " |") ++
    " " ++ (c6 : "   ") ++ (c7 : "   ") ++ (c8 : " |\n")

-- produces a single list from a list of lists
appendLists :: [[a]] -> [a]
appendLists = concat






