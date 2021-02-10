module PrintCrissCross where
import System.IO 

printEmptyCrissCross :: Int -> Int -> IO Int
printEmptyCrissCross x y = pomPrintEmptyCC x x y ""
    where pomPrintEmptyCC w 0 0 line = return 0
          pomPrintEmptyCC w 0 y line = do 
                                    putStrLn (line)
                                    pomPrintEmptyCC w w (y-1) ""
          pomPrintEmptyCC w x y line = pomPrintEmptyCC w (x-1) y (line ++ " -")

pomCreateEmptyCC :: Int -> Int -> Int -> String -> [String] -> [String]
pomCreateEmptyCC w 0 0 line list = list
pomCreateEmptyCC w 0 y line list = pomCreateEmptyCC w w (y-1) "" (list ++ [line])
pomCreateEmptyCC w x y line list = pomCreateEmptyCC w (x-1) (y) (line ++ "-") list



createEmptyCrissCross :: Int -> Int -> IO[String]
createEmptyCrissCross x y = return (pomCreateEmptyCC x x y "" [])


    
gridToSymbols :: [[Int]] -> [[Char]]
gridToSymbols grid= [[changeSymbol (grid!!y!!x) | x <- [0..length grid - 1]] | y <- [0..length grid - 1]]

changeSymbol:: Int -> Char
changeSymbol val = if val == 1 then '0'
                   else if val == -1 then '.'
                   else '.'

printGrid :: [[Int]]  -> IO [()]
printGrid grid = sequence $ map (putStrLn . textRepresentation) $ gridToSymbols grid

textRepresentation :: [Char] -> String
textRepresentation row = foldl (\acc y -> acc ++ (show y) ++ " ") "" row