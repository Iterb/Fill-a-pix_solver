module Solver where
import System.IO 
import Board
import Debug.Trace



----------------------------------------------
--Basic step of solving puzzle
----------------------------------------------
--fills squares on the basis of 1 clue. 


-- loop repeating this step until the stop criterion (no new changes) is met
basicStepLoop :: ([[Int]], [[Int]]) -> Int -> ([[Int]], [[Int]])
basicStepLoop (colorGrid, dataGrid) boardSize = 
                                if myFst (basicStepGrid (colorGrid, dataGrid) (boardSize*boardSize)) == colorGrid 
                                then basicStepGrid (colorGrid, dataGrid) (boardSize*boardSize)
                                else basicStepLoop (basicStepGrid (colorGrid, dataGrid) (boardSize*boardSize)) boardSize


-- loop applying logic to every point on the grid
basicStepGrid :: ([[Int]], [[Int]]) -> Int -> ([[Int]], [[Int]])
basicStepGrid (colorGrid, dataGrid) 1 = basicStep dataGrid colorGrid 0 0
basicStepGrid (colorGrid, dataGrid) n = basicStepGrid (basicStep dataGrid colorGrid ((n-1) `mod` (length dataGrid)) ((n-1) `div` (length dataGrid))) (n-1)

-- procceds if given point contains any clue
basicStep :: [[Int]] -> [[Int]] -> Int -> Int -> ([[Int]], [[Int]])
basicStep dataGrid colorGrid i j = if dataGrid!!i!!j == -1 then (colorGrid, dataGrid)
                                   else checkBasicLogic colorGrid i j dataGrid

-- applies basic logic. Checks how many squares corresponding to a given clue is filled or blocked. 
-- If the number of filled squares if equal to clue value then rest of the free squares are blocked and vice versa. 
checkBasicLogic :: [[Int]] -> Int -> Int -> [[Int]] -> ([[Int]], [[Int]])
checkBasicLogic colorGrid i j dataGrid
                                | (dataGrid!!i!!j) - countSquares colorGrid i j 1 == 0
                                = (fillAllSquares colorGrid i j 8 (-1), updateDataGrid dataGrid (-1) (i, j))
                                | (dataGrid!!i!!j) + countSquares colorGrid i j (-1) == 9
                                = (fillAllSquares colorGrid i j 8 1, updateDataGrid dataGrid (-1) (i, j))
                                | otherwise = (colorGrid, dataGrid)


----------------------------------------------
--Advanced step of solving puzzle
----------------------------------------------
--fills squares on the basis of 2 clues. Comes with 2 variations - for adjacent clues and for the ones separated with one square

-- loop repeating this step until the stop criterion (no new changes) is met
--to do

-- loop applying logic to every point on the grid (except outer layers)
twoClueLogicAdjGrid :: ([[Int]], [[Int]]) -> Int -> ([[Int]], [[Int]])
twoClueLogicAdjGrid (colorGrid, dataGrid) n = if (n /= (l*l)) then  twoClueLogicAdjGrid (twoClueLogicAdjStep colorGrid dataGrid a b) (n+1)
                                              else twoClueLogicAdjStep colorGrid dataGrid  (l-1) (l-1)
                                                where a = (n-1) `mod` l 
                                                      b = (n-1) `div` l 
                                                      l = length colorGrid

--corresponding function for separated case
twoClueLogicBetGrid :: ([[Int]], [[Int]]) -> Int -> ([[Int]], [[Int]])
twoClueLogicBetGrid (colorGrid, dataGrid) n =   if (n /= (l*l)) then twoClueLogicBetGrid (twoClueLogicBetStep colorGrid dataGrid i j) (n+1)
                                                else twoClueLogicBetStep colorGrid dataGrid  (l-3) (l-3)
                                                where i = restraints3 ((n-1) `mod` l) l
                                                      j = restraints3 ((n-1) `div` l) l
                                                      l = length colorGrid

-- procceds if given point contains any clue and there is at least 1 adjacent clue
twoClueLogicAdjStep :: [[Int]] -> [[Int]] -> Int -> Int-> ([[Int]], [[Int]])
twoClueLogicAdjStep colorGrid dataGrid  i j = if (((countSquaresWithInformation dataGrid i j) == 0) || ((dataGrid!!i!!j) == -1)) then (colorGrid, dataGrid)
                                              else forEveryAdjSqure (colorGrid, dataGrid) i j 8 (dataGrid!!i!!j)

-- procceds if given point contains any clue and there is at least 1 clue within 2 squares in any direction
twoClueLogicBetStep :: [[Int]] -> [[Int]] -> Int -> Int-> ([[Int]], [[Int]])
twoClueLogicBetStep colorGrid dataGrid  i j = if ((countSquaresWithInformation2Spaces dataGrid i j) == 0) || ((dataGrid!!i!!j) == -1) then (colorGrid, dataGrid)
                                              else forEveryBetSqure (colorGrid, dataGrid) i j 24 (dataGrid!!i!!j)

-- loop that checks every adjacent square for clues and proceeds if finds one  
forEveryAdjSqure :: ([[Int]], [[Int]]) -> Int -> Int -> Int -> Int -> ([[Int]], [[Int]])

forEveryAdjSqure (colorGrid, dataGrid) i j 0 val = if a!!0 == (-1,-1) then (colorGrid, dataGrid)
                                                   else twoClueLogic (colorGrid, dataGrid) i j (fst(a!!0)) (snd(a!!0)) val
                                                    where a = getAllAdjacentSquresWithInfo dataGrid i j (length dataGrid)
forEveryAdjSqure (colorGrid, dataGrid) i j n val = if (a!!n) == (-1,-1) then forEveryAdjSqure (colorGrid, dataGrid ) i j (n-1) val
                                                   else forEveryAdjSqure (twoClueLogic (colorGrid, dataGrid) i j (fst(a!!n)) (snd(a!!n)) val) i j (n-1) val
                                                    where a = getAllAdjacentSquresWithInfo dataGrid i j (length dataGrid)
         

-- loop that checks every square within 2 squares for clues and proceeds if finds one  
forEveryBetSqure :: ([[Int]], [[Int]]) -> Int -> Int -> Int -> Int -> ([[Int]], [[Int]])
forEveryBetSqure (colorGrid, dataGrid) i j 0 val = if a!!0 == (-1,-1) then (colorGrid, dataGrid)
                                                   else twoClueLogic (colorGrid, dataGrid) i j (fst(a!!0)) (snd(a!!0)) val
                                                    where a = getAllBetweenSquresWithInfo dataGrid i j (length dataGrid)
forEveryBetSqure (colorGrid, dataGrid) i j n val = if (a!!n) == (-1,-1) then forEveryBetSqure (colorGrid, dataGrid ) i j (n-1) val
                                                   else forEveryBetSqure (twoClueLogic (colorGrid, dataGrid) i j (fst(a!!n)) (snd(a!!n)) val) i j (n-1) val
                                                    where a = getAllBetweenSquresWithInfo dataGrid i j (length dataGrid)

----------------------------------------------
--From this point both methods behave the same
----------------------------------------------

-- applies advanced logic based on disjunctive union of neighborhoods to every square and it's found neighbour  
twoClueLogic :: ([[Int]],[[Int]]) -> Int -> Int -> Int-> Int -> Int ->([[Int]], [[Int]])
twoClueLogic (colorGrid, dataGrid) p1x p1y p2x p2y val = if ((e1 >= e2) && (l1 == abs(e1 - e2))) then (fillCorrectSquares1 colorGrid p1x p1y p2x p2y 1 , dataGrid)
                                                            else if ((e1 <= e2) && (l2 == abs(e1 - e2))) then (fillCorrectSquares2 colorGrid p1x p1y p2x p2y 1 ,dataGrid)
                                                            else (colorGrid, dataGrid)
                                                              where e1 = val - snd (getInfoFromDisjunctiveUnion (twoPointDisjunctiveUnion colorGrid p1x p1y p2x p2y))
                                                                    e2 = dataGrid!!p2x!!p2y - snd (getInfoFromDisjunctiveUnion (twoPointDisjunctiveUnion colorGrid p2x p2y p1x p1y))
                                                                    l1 = fst (getInfoFromDisjunctiveUnion (twoPointDisjunctiveUnion colorGrid p1x p1y p2x p2y))
                                                                    l2 = fst (getInfoFromDisjunctiveUnion (twoPointDisjunctiveUnion colorGrid p2x p2y p1x p1y))
                                                                    

-- fills or blocks correct squares                                                            
fillCorrectSquares1:: [[Int]]-> Int -> Int -> Int-> Int -> Int -> [[Int]]
fillCorrectSquares1 colorGrid p1x p1y p2x p2y 0 = fillSquaresFromList colorGrid (-1) r2
                                                  where r2 = disjunctiveUnionCoords colorGrid p2x p2y p1x p1y 
fillCorrectSquares1 colorGrid p1x p1y p2x p2y n = fillCorrectSquares1 (fillSquaresFromList colorGrid 1 r1) p1x p1y p2x p2y (n-1)
                                                  where r1 = disjunctiveUnionCoords colorGrid p1x p1y p2x p2y 
                                   
fillCorrectSquares2:: [[Int]]-> Int -> Int -> Int-> Int -> Int -> [[Int]]
fillCorrectSquares2 colorGrid p1x p1y p2x p2y 0 = fillSquaresFromList colorGrid (-1) r1
                                                  where r1 = disjunctiveUnionCoords colorGrid p1x p1y p2x p2y 
fillCorrectSquares2 colorGrid p1x p1y p2x p2y n = fillCorrectSquares2 (fillSquaresFromList colorGrid 1 r2) p1x p1y p2x p2y (n-1)
                                                  where r2 = disjunctiveUnionCoords colorGrid p2x p2y p1x p1y 


