import Boards
import Data.Maybe ( fromJust, isJust )
import Data.Char (digitToInt)
import Game ( formatBoard )
import Solver ( solveBoard )
import Sudoku ( Board )


-- To run it, try:
-- ghci
-- :load Main
-- start

start =
  do
    putStrLn "Welcome to Haskell Sudoku!"
    gameBoard <- selectBoard
    putStrLn "Loading board..."
    let solnBoard = solveBoard gameBoard
    if isJust solnBoard
      then runGame (fromJust solnBoard) gameBoard
      else putStrLn "Invalid game board."
    putStrLn "Leaving Sudoku."
    return ()

-- selectBoard :: IO Board
selectBoard :: IO Board
selectBoard =
  do
    putStrLn "Select a game board:"
    putStrLn "1. Effortless"
    putStrLn "2. Novice"
    putStrLn "3. Easy"
    putStrLn "4. Moderate"
    putStrLn "5. Hard"
    line <- getLine
    case line of
      "1" -> do
        putStrLn "Selected effortless board..."
        return bd3
      "2" -> do
        putStrLn "Selected novice board..."
        return bdNovice
      "3" -> do
        putStrLn "Selected easy board..."
        return bdEasy
      "4" -> do
        putStrLn "Selected moderate board..."
        return bdModerate
      "5" -> do
        putStrLn "Selected hard board..."
        return bdHard
      _ -> do
        putStrLn "Invalid selection..."
        selectBoard

-- runGame :: Board -> Board -> IO ()
-- runGame solnBoard gameBoard =
--   do
--     putStrLn "Current board:"
--     putStrLn (formatBoard gameBoard)
--     putStrLn "Choose one of the following options:"
--     putStrLn "1. Update Square"
--     putStrLn "2. Get Hint"
--     putStrLn "3. Solve Board"
--     putStrLn "4. Quit"
--     line <- getLine
--     case line of
--       "1" -> do
--         putStrLn "Select square to update in the format (x, y) with x representing the horizontal cell number"
--         putStrLn "and y the vertical cell number with both x and y between 1 and 9 inclusive."
--         line <- getLine
--         if length line != 5
--           then
--             putStrLn "Invalid input. Please follow the guidelines."
--             runGame solnBoard gameBoard
--           else putStrLn "cool"
--       "2" -> do
--         putStrLn "Select square to solve in the format (x, y) with x representing the horizontal cell number"
--         putStrLn "and y the vertical cell number with both x and y between 1 and 9 inclusive."
--         return ()
--       "3" -> do
--         putStrLn "Solving current board..."
--         return ()
--       -- putStrLn solveBoard bdModerate
--       "4" -> do
--         putStrLn "Thank you for playing."
--         return ()
--       _ -> do
--         putStrLn "Invalid selection..."
--         runGame solnBoard gameBoard

runGame solnBoard gameBoard = do
  putStrLn $ unlines [
    "Current board:",
    formatBoard gameBoard,
    "Choose one of the following options:",
    "1. Update Square",
    "2. Get Hint",
    "3. Solve Board",
    "4. Quit"]
  line <- getLine
  case line of
    "1" -> do
      putStrLn "Select square to update in the format (x, y) with x representing the horizontal cell number and y the vertical cell number with both x and y between 1 and 9 inclusive."
      coord <- getLine
      -- Validate input format and length
      if not (isValidCoordinates coord)
        then do
          putStrLn "Invalid input. Please follow the guidelines."
          runGame solnBoard gameBoard
        else putStrLn "valid input"
    "2" -> do
      putStrLn "Select square to receive answer for in the format (x, y) with x representing the horizontal cell number and y the vertical cell number with both x and y between 1 and 9 inclusive."
      coord <- getLine
      -- Validate input format and length
      if not (isValidCoordinates coord)
        then do
          putStrLn "Invalid input. Please follow the guidelines."
          runGame solnBoard gameBoard
        else putStrLn "valid input"
    "3" -> do
      putStrLn "Solving current board..."
      -- Add your solveBoard function call here if applicable
    "4" -> putStrLn "Thank you for playing."
    _   -> do
      putStrLn "Invalid selection..."
      runGame solnBoard gameBoard

-- Validate the input format and length
isValidCoordinates :: String -> Bool
isValidCoordinates input = isValidCoordLength input && isValidCoordFormat input

isValidCoordLength :: String -> Bool
isValidCoordLength input = length input == 5

isValidCoordFormat :: String -> Bool
isValidCoordFormat (a:b:c:d:e:"")
  | a /= '(' = False
  | digitToInt b < 1 || digitToInt b > 9 = False
  | c /= ',' = False
  | digitToInt d < 1 || digitToInt d > 9 = False
  | e /= ')' = False
  | otherwise = True