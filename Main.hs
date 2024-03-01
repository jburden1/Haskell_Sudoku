import Boards
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromJust, isJust)
import Game (formatBoard, getValue, isBlank, putValue)
import Solver (solveBoard)
import Sudoku (Board)
import TextFileReader (parseBoard)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException, try)

-- To run it, try:
-- ghci
-- :load Main
-- start

-- starts the program
start :: IO ()
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

-- board selection screen
selectBoard :: IO Board
selectBoard =
  do
    putStrLn "Select a game board:"
    putStrLn "1. Effortless"
    putStrLn "2. Novice"
    putStrLn "3. Easy"
    putStrLn "4. Moderate"
    putStrLn "5. Hard"
    putStrLn "6. Custom Board"
    line <- getLine
    case line of
      -- effortless board
      "1" -> do
        putStrLn "Selected effortless board..."
        return bd3
      -- novice board
      "2" -> do
        putStrLn "Selected novice board..."
        return bdNovice
      -- easy board
      "3" -> do
        putStrLn "Selected easy board..."
        return bdEasy
      -- moderately difficult board
      "4" -> do
        putStrLn "Selected moderate board..."
        return bdModerate
      -- difficult board
      "5" -> do
        putStrLn "Selected hard board..."
        return bdHard
      -- custom board from text file
      "6" -> do
        putStrLn "Type the name of the board file in the format example.txt"
        fileName <- getLine
        fileExists <- doesFileExist fileName
        parsedBoard <- parseBoard fileName
        if fileExists
          then do
            if parsedBoard /= []
              then do
                return parsedBoard
              else do
                putStrLn "File name has an invalid board..."
                selectBoard
          else do
            putStrLn "Invalid file name provided..."
            selectBoard
      _ -> do
        putStrLn "Invalid selection..."
        selectBoard

-- shows current board and allows user input
runGame :: Board -> Board -> IO ()
runGame solnBoard gameBoard = do
  putStrLn $
    unlines
      [ "Current board:",
        formatBoard gameBoard,
        "Choose one of the following options:",
        "1. Update Square",
        "2. Get Hint",
        "3. Solve Board",
        "4. Quit"
      ]
  choice <- getChar
  putStrLn "\n"
  case choice of
    -- update a particular square
    '1' -> do
      putStrLn "Select a blank square to update in the format (x,y) with x representing the horizontal cell number and y the vertical cell number with both x and y between 1 and 9 inclusive."
      coord <- getLine
      if not (isValidCoordinates coord)
        then do
          putStrLn "Invalid input. Please follow the guidelines."
          runGame solnBoard gameBoard
        else do
          let [a, x, b, y, c] = coord
          if not (isBlank (getValue gameBoard (digitToInt y - 1, digitToInt x - 1)))
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
                  if getValue solnBoard (digitToInt y - 1, digitToInt x - 1) /= newValue
                    then do
                      putStrLn "Number chosen is incorrect. Try again."
                      runGame solnBoard gameBoard
                    else do
                      let newBoard = putValue gameBoard ((digitToInt y - 1, digitToInt x - 1), newValue)
                      runGame solnBoard newBoard
    -- get hint at particular square
    '2' -> do
      putStrLn "Select square to receive answer for in the format (x, y) with x representing the horizontal cell number and y the vertical cell number with both x and y between 1 and 9 inclusive.\n"
      coord <- getLine
      if not (isValidCoordinates coord)
        then do
          putStrLn "Invalid input. Please follow the guidelines.\n"
          runGame solnBoard gameBoard
        else do
          let [a, x, b, y, c] = coord
          if not (isBlank (getValue gameBoard (digitToInt y - 1, digitToInt x - 1)))
            then do
              putStrLn "Chosen square is not currently blank. Choose a blank square.\n"
              runGame solnBoard gameBoard
            else do
              let newValue = getValue solnBoard (digitToInt y - 1, digitToInt x - 1)
              let newBoard = putValue gameBoard ((digitToInt y - 1, digitToInt x - 1), newValue)
              putStrLn "Adding hint...\n"
              runGame solnBoard newBoard
    -- shows solution
    '3' -> do
      putStrLn "Solving current board..."
      runGame solnBoard solnBoard
    -- quits the game
    '4' -> do
      putStrLn "Thank you for playing."
      return ()
    -- invalid input
    _ -> do
        putStrLn "Invalid selection..."
        runGame solnBoard gameBoard

-- validates the input format and length
isValidCoordinates :: String -> Bool
isValidCoordinates input = isValidCoordLength input && isValidCoordFormat input

-- validates the input string length
isValidCoordLength :: String -> Bool
isValidCoordLength input = length input == 5

-- validates coordinate format
isValidCoordFormat :: String -> Bool
isValidCoordFormat (a : b : c : d : e : "")
  | a /= '(' = False
  | not (isDigit b) || digitToInt b < 1 || digitToInt b > 9 = False
  | c /= ',' = False
  | not (isDigit d) || digitToInt d < 1 || digitToInt d > 9 = False
  | e /= ')' = False
  | otherwise = True
