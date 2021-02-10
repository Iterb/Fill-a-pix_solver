import ReadingData
import Solver
import PrintCrissCross
import System.IO 
import Board



main = do 
        putStr "Podaj nazwe pliku wejsciowego: \n"
        hFlush stdout
        n <- getLine
        puzzle <- readPuzzle n
        let boardSize = length puzzle

        let colorGrid = [[if j==0 || i==0 || j== boardSize + 1 || i == boardSize + 1 then -1 else 0 | j <- [0..(boardSize+1 )]] | i <- [0..(boardSize+1)]]
        let dataGrid = convertDataToDataGrid boardSize puzzle

        let boardSize2 = length dataGrid

        print puzzle
        print dataGrid
        print colorGrid
        let solution1 = twoClueLogicAdjGrid (colorGrid, dataGrid) 1
        printGrid (fst solution1)
        let solution2 = basicStepLoop ((fst solution1), dataGrid) boardSize2
        printGrid (fst solution2)
        let solution3 = twoClueLogicBetGrid ((fst solution2), dataGrid) 1
        printGrid (fst solution3)
        let solution4 = basicStepLoop ((fst solution3), dataGrid) boardSize2
        printGrid (fst solution4)

        --proponuje zeby glowny program dziala tak - 
         -- podstawowy krok aż nic się nie poprawi lub będzie rozwiązany
         -- twoClueLogicAdjGrid aż nic się nie poprawi lub będzie rozwiązany
         -- podstawowy krok aż nic się nie poprawi lub będzie rozwiązany
         -- twoClueLogicBetGrid aż nic się nie poprawi lub będzie rozwiązany
         -- i do tak do skutku (jak po całym kroku nic się nie zmieni to znaczy, że się nie da)



