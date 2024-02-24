module TextFileReader where
import Data.ByteString (fromFilePath)
import Data.Maybe
import Solver (solveBoard)

-- implement
  -- parseBoard (just call readFile)
  -- validate board by solving
  -- add option on Main.hs to upload custom board
  -- will call the parseBoard and validate board functions together

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




