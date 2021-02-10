module ReadingData where
import System.IO 

readPuzzle :: String -> IO[String]
readPuzzle filename = do
    contents <- readFile filename
    let puzzle = read contents :: [String]
    return puzzle

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine