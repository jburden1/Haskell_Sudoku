module TextFileReader where
import Data.ByteString (fromFilePath)
import Data.Maybe ( isNothing )
import Solver (solveBoard)

-- converts text in the given filepath to a board and checks if the board is valid
parseBoard :: FilePath -> IO [String]
parseBoard filePath = do
    content <- readFile filePath
    let newBoard = lines content
    if isNothing (solveBoard newBoard)
      then do
        putStrLn "invalid board uploaded"
        return []
      else do
        return (lines content)




