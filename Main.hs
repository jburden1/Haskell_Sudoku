import Sudoku
import Boards
import GameEngine
import Solver
import Data.Maybe

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
            _   -> do
                putStrLn "Invalid selection..."
                selectBoard
                
runGame :: Board -> Board -> IO ()
runGame solnBoard gameBoard = 
    do
        putStrLn "Current board:"

        putStrLn "Choose one of the following options:"
        putStrLn "1. Update Square"
        putStrLn "2. Get Hint"
        putStrLn "3. Solve Board"
        putStrLn "4. Quit"







