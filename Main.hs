import Boards
import Data.Maybe ( fromJust, isJust )
import Data.Char (digitToInt, isDigit)
import Game ( formatBoard, getValue, putValue, isBlank )
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

runGame :: Board -> Board -> IO ()
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
      putStrLn "Select a blank square to update in the format (x,y) with x representing the horizontal cell number and y the vertical cell number with both x and y between 1 and 9 inclusive."
      coord <- getLine
      if not (isValidCoordinates coord)
        then do
          putStrLn "Invalid input. Please follow the guidelines."
          runGame solnBoard gameBoard
        else do
          let [a, x, b, y, c] = coord
          if not (isBlank (getValue gameBoard (digitToInt x - 1, digitToInt y - 1)))
            then do
              putStrLn "Chosen square is not currently blank. Choose a blank square."
              runGame solnBoard gameBoard
            else do
              putStrLn "Type in the number (between 1 and 9) you want to place in the given coordinate with"
              newValue <- getChar
              putStrLn "\n"
              if not (isDigit newValue)
                then do 
                  putStrLn "Invalid choice. Character typed is not a number"
                  runGame solnBoard gameBoard
                else do
                  if getValue solnBoard (digitToInt x - 1, digitToInt y - 1) /= newValue
                    then do
                      putStrLn "Number chosen is incorrect. Try again."
                      runGame solnBoard gameBoard
                    else do
                      let newBoard = putValue gameBoard ((digitToInt x - 1, digitToInt y - 1), newValue)
                      runGame solnBoard newBoard
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
      return ()
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
  | not (isDigit b) || digitToInt b < 1 || digitToInt b > 9 = False
  | c /= ',' = False
  | not (isDigit d)|| digitToInt d < 1 || digitToInt d > 9 = False
  | e /= ')' = False
  | otherwise = True