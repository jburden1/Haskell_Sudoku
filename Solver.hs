module Solver where
import Sudoku
import Boards
import GameEngine
import Data.Maybe

-- produces a solution for the board or 'Nothing' if the board is unsolvable
-- uses brute force recursive search with backtracking
solveBoard :: Board -> Maybe Board
-- Try:
-- solveBoard bdNovice == Just bdNoviceSoln
-- solveBoard bdEasy == Just bdEasySoln
-- solveBoard bdModerate == Just bdModerateSoln
-- solveBoard bdHard == Just bdHardSoln
-- solveBoard bd5 == Nothing
solveBoard bd 
    | solved bd = Just bd
    | otherwise = solveBoards (nextBoards bd)
        where
            solveBoards [] = Nothing
            solveBoards bds = 
                let try = solveBoard (head bds)
                in
                    if isJust try
                    then try  
                    else solveBoards (tail bds)

-- produces 'True' if board is solved, 'False' otherwise
solved :: Board -> Bool
-- Try:
-- solved bdNovice == False
-- solved bdNoviceSoln == True
solved bd = and [blank `notElem` lst | lst <- bd]

-- produces list of valid next boards from board
nextBoards :: Board -> [Board]
nextBoards bd =
    let blankPos = findBoardBlank bd
    in
        pruneInvalid (fillValues blankPos bd) blankPos

-- finds the position of the first blank value in a board
-- assume board contains at least one blank value
findBoardBlank :: Board -> Position
-- Try:
-- findBoardBlank bdBlank == (0,0)
-- findBoardBlank bd2 == (1,0)
-- findBoardBlank bd3 == (6,7)
findBoardBlank bd = findBoardBlank' bd 0
    where
        findBoardBlank' [] _ = error "Board did not contain a blank value."
        findBoardBlank' (h:t) row =
            let listBlank = findListBlank h
            in 
                if isJust listBlank  
                then (row, fromJust listBlank)  
                else findBoardBlank' t (row + 1)

-- finds the index of the first blank in a list
findListBlank :: [Value] -> Maybe Int
-- Try:
-- findListBlank ".234.6789" == Just 0
-- findListBlank "1234.6789" == Just 4
-- findListBlank "12345678." == Just 8
findListBlank lst = findListBlank' lst 0
    where
        findListBlank' [] _ = Nothing
        findListBlank' (h:t) col 
            | isBlank h = Just col
            | otherwise = findListBlank' t (col + 1)

-- generates boards from board by filling in the values ['1'..'9'] into position 
fillValues :: Position -> Board -> [Board]
-- Try:
-- fillValues (0,0) bdBlank
fillValues pos bd = [putValue bd (pos, val) | val <- ['1'..'9']]

-- removes invalid boards from generated list of boards
pruneInvalid :: [Board] -> Position -> [Board]
pruneInvalid bds pos = [bd | bd <- bds, validateRow bd pos && validateCol bd pos && validateBox bd pos]

-- produces 'True' if the row containing position is valid, 'False' otherwise
validateRow :: Board -> Position -> Bool
-- Try:
-- validateRow bdNovice (0,0) == True
-- validateRow bd4 (6,0) == False
validateRow bd (row, _) = validRow (bd !! row)

-- produces 'True' if the column containing position is valid, 'False' otherwise
validateCol :: Board -> Position -> Bool
-- Try:
-- validateCol bdNovice (0,0) == True
-- validateCol bd1 (0,0) == False
-- validateCol bd1 (5,5) == False
validateCol bd (_, col) = validRow [row !! col | row <- bd]

-- produces 'True' if the box containing position is valid, 'False' otherwise
validateBox :: Board -> Position -> Bool
-- Try:
-- validateBox bdNovice (0,0) == True
-- validateBox bd1 (1,1) == False
validateBox bd (row, col) = validRow (appendLists (trimBoxRows (getBoxRows bd row) col)) 

-- produces rows of a box based on row position
getBoxRows :: Board -> Int -> Board
getBoxRows bd row 
    | row <= 2 = take 3 bd
    | row <= 5 = take 3 (drop 3 bd)
    | otherwise = drop 6 bd 

-- trims the rows of a box based on column position
trimBoxRows :: Board -> Int -> Board
trimBoxRows bd col 
    | col <= 2 = [take 3 row | row <- bd]
    | col <= 5 = [take 3 (drop 3 row) | row <- bd]
    | otherwise = [drop 6 row | row <- bd]

-- produces a single list from a list of lists
appendLists :: [[a]] -> [a]
appendLists [] = []
appendLists (h:t) = h ++ appendLists t

-- produces 'True' if a row is valid, 'False' otherwise 
validRow :: [Value] -> Bool
-- Try:
-- validRow "123456789" == True
-- validRow "..345..89" == True
-- validRow "113456789" == False
-- validRow ".234..72." == False
validRow [] = True
validRow (h:t) 
    | not (isBlank h) && (h `elem` t) = False
    | otherwise = validRow t



