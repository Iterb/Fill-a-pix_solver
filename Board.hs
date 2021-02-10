module Board where
import System.IO
import Prelude

----------------------------------------------
--Functions that convert input data into [[Int]]
----------------------------------------------

--create 2D list. Maps input format of puzzle into values. If no clue for square is available -> returns -1
--otherwise returns clue value 
convertDataToDataGrid :: Int -> [[Char]] -> [[Int]]
convertDataToDataGrid len m = [[if j==0 || i==0 || j== len + 1 || i == len + 1 then -1 else convertToData m (i-1) (j-1)| j <- [0..(len+1)]] | i <- [0..(len+1)]]

--returns the value based on [[Char]] and indices.  
convertToData:: [[Char]] -> Int -> Int -> Int
convertToData m i j =if m!!i!!j == '.' then -1
                     else read [(m!!i!!j)]

----------------------------------------------
--Functions that collect information from squares
----------------------------------------------

--returns the number of adjacent squares with a given value 
countSquares:: [[Int]] -> Int -> Int -> Int -> Int
countSquares colorGrid i j v = sum $ map sum ([[if colorGrid!!x!!y == v then 1 else 0| x <- [i-1..i+1]] | y <- [j-1..j+1]])

--returns the number of adjacent squares with a value diffrent than -1 (-1 because we have to exclude itself)
countSquaresWithInformation:: [[Int]] -> Int -> Int -> Int
countSquaresWithInformation dataGrid i j = (sum $ map sum ([[if dataGrid!!(restraints x (length dataGrid))!!(restraints y (length dataGrid)) /= -1 then 1 else 0| x <- [i-1..i+1]] | y <- [j-1..j+1]])) - 1 

--returns the number of squares with a value diffrent than -1 within 2 sqares in any direction (-1 because we have to exclude itself)
countSquaresWithInformation2Spaces:: [[Int]] -> Int -> Int -> Int
countSquaresWithInformation2Spaces dataGrid i j = (sum $ map sum ([[if dataGrid!!x!!y /= -1 then 1 else 0| x <- [i-2..i+2]] | y <- [j-2..j+2]])) - 1 

----------------------------------------------
--Functions that modify values in squares
----------------------------------------------

--fills all squares around given square with given value 
fillAllSquares:: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
fillAllSquares colorGrid i j 0 val = updateGrid colorGrid val (i - 1, j - 1)
fillAllSquares colorGrid i j n val = fillAllSquares (updateGrid colorGrid val ((i + (n `mod` 3) - 1), (j + (n `div` 3) - 1))) i j (n-1) val

--fills all squares from the coords list 
fillSquaresFromList:: [[Int]] -> Int -> [(Int,Int)] ->  [[Int]]
fillSquaresFromList colorGrid val [] = colorGrid
fillSquaresFromList colorGrid val (x:xs) = if x == (-1,-1) then fillSquaresFromList colorGrid val xs
                                           else fillSquaresFromList (updateGrid colorGrid val x) val xs

--safe update - changes value of given [[Int]] on given coordinates (checks if value is equal to 0 in order not to overwrite any data)
updateGrid :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
updateGrid m x (r,c) =
  if m!!r!!c == 0 then take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m
  else m

-- changes value of given [[Int]] on given coordinates 
updateDataGrid :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
updateDataGrid m x (r, c) = take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m

----------------------------------------------
--Functions that were used to compute the disjunctive union of the neighborhoods of two squares
----------------------------------------------

-- gets the number of unblocked and filled squares in the disjunctive union of 2 points neighbourhoods
getInfoFromDisjunctiveUnion :: [[Int]] -> (Int, Int)
getInfoFromDisjunctiveUnion disjointUnion =  ((sum $ map sum [[if disjointUnion!!x!!y /= (-1) then 1 else 0 | x <- [0..(length disjointUnion - 1)]] | y <- [0..(length disjointUnion - 1)]]),
                                           (sum $ map sum [[if disjointUnion!!x!!y == 1 then 1 else 0 | x <- [0..(length disjointUnion - 1)]] | y <- [0..(length disjointUnion - 1)]]))

-- puts the value of colorGrid where the neighbourhood of 2 points is disjointed otherwise returns -1  
twoPointDisjunctiveUnion :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
twoPointDisjunctiveUnion colorGrid  p1x p1y p2x p2y = [[ if (checkIfDisjointed x y p2x p2y 8 True) then colorGrid!!(restraints4 x (length colorGrid))!!(restraints4 y (length colorGrid)) else -1 | x <- [p1x-1..p1x+1]] | y <- [p1y-1..p1y+1]]

-- puts the value of coords where the neighbourhood of 2 points is disjointed otherwise returns (-1,-1)
disjunctiveUnionCoords :: [[Int]] -> Int -> Int -> Int -> Int -> [(Int,Int)]
disjunctiveUnionCoords colorGrid  p1x p1y p2x p2y = flatten2Darray [[ if (checkIfDisjointed x y p2x p2y 8 True) then (x,y) else (-1,-1) | x <- [p1x-1..p1x+1]] | y <- [p1y-1..p1y+1]]

-- checks if given point doesnt lie in the other point's neighbourhood  
checkIfDisjointed ::  Int -> Int -> Int -> Int -> Int -> Bool -> Bool
checkIfDisjointed p1x p1y p2x p2y 0 False = False
checkIfDisjointed p1x p1y p2x p2y 0 b= checkIfPointIsEqual p1x p1y p2x p2y 0 
checkIfDisjointed p1x p1y p2x p2y n False = False
checkIfDisjointed p1x p1y p2x p2y n b= checkIfDisjointed p1x p1y p2x p2y (n-1) (checkIfPointIsEqual p1x p1y p2x p2y n)

-- checks if coords of given point are equal to the other point's coords
checkIfPointIsEqual ::  Int -> Int -> Int -> Int -> Int -> Bool
checkIfPointIsEqual p1x p1y p2x p2y n = if (p1x == a && p1y == b) then False
                                        else True
                                          where a = (p2x + (n `mod` 3) - 1)
                                                b = (p2y + (n `div` 3) - 1)

-- returns [(Int,Int)] of coordinates of adjacent squares that contain any clues
getAllAdjacentSquresWithInfo:: [[Int]] -> Int -> Int -> Int -> [(Int,Int)]
getAllAdjacentSquresWithInfo dataGrid i j size = [ if (dataGrid!!(restraints (i+(n `mod` 3) - 1) size)!!(restraints (j+(n `div` 3) - 1) size)) /= (-1) then (restraints2 (i+(n `mod` 3)- 1) (j+(n `div` 3) - 1) size) else (-1,-1)| n <- [0..8]]

-- returns [(Int,Int)] of coordinates of squares that contain any clues within 2 squares in any direction
getAllBetweenSquresWithInfo:: [[Int]] -> Int -> Int -> Int -> [(Int,Int)]
getAllBetweenSquresWithInfo dataGrid i j size = [ if (dataGrid!!(i+(n `mod` 5) - 2)!!(j+(n `div` 5) - 2)) /= (-1) then (restraints2 (i+(n `mod` 5)- 2) (j+(n `div` 5) - 2) size) else (-1,-1)| n <- [0..24]]
                                        

----------------------------------------------
--Auxiliary functions
----------------------------------------------
--converts 2D 3x3 list into 1D  
flatten2Darray:: [[(Int,Int)]]-> [(Int,Int)]
flatten2Darray array= [ array!!(n `mod` 3)!!(n `div` 3) | n <- [0..8]]



--set of restraints that protects Ints from reaching illegal values 
restraints :: Int -> Int -> Int 
restraints a size = min (max 1 a) (size - 2)

restraints2 :: Int -> Int -> Int -> (Int,Int)
restraints2 a b size = if ((a >= (size - 2)) || (a <= 0) || (b >= (size - 2)) || (b <= 0)) then (-1,-1)
                       else (a,b)

restraints3 :: Int -> Int -> Int 
restraints3 a size = min (max 3 a) (size - 4)

restraints4 :: Int -> Int -> Int 
restraints4 a size = if ((a >= (size - 1)) || (a <= 0) ) then 0
                       else a

myFst::([[Int]], [[Int]]) -> [[Int]]
myFst (x, _) = x


mat :: [[Char]]
mat = ["..5....54.",
  ".5..6..5..",
  "4.2.5...44",
  ".4....1...",
  "...1..13.5",
  "...3..36..",
  ".676.4....",
  ".3..77..31",
  ".13.8...1.",
  "......3..."]

d :: [[Int]]
d = [[1, 3, 4], [1, 1, 1], [0, -1, 1]]

hard_color :: [[Int]]
hard_color = [[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,0,0,0,0,0,0,0,0,0,0,-1],[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]]
hard_data :: [[Int]]
hard_data = [[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],[-1,-1,-1,5,-1,-1,-1,-1,5,4,-1,-1],[-1,-1,5,-1,-1,6,-1,-1,5,-1,-1,-1],[-1,4,-1,2,-1,5,-1,-1,-1,4,4,-1],[-1,-1,4,-1,-1,-1,-1,1,-1,-1,-1,-1],[-1,-1,-1,-1,1,-1,-1,1,3,-1,5,-1],[-1,-1,-1,-1,3,-1,-1,3,6,-1,-1,-1],[-1,-1,6,7,6,-1,4,-1,-1,-1,-1,-1],[-1,-1,3,-1,-1,7,7,-1,-1,3,1,-1],[-1,-1,1,3,-1,8,-1,-1,-1,1,-1,-1],[-1,-1,-1,-1,-1,-1,-1,3,-1,-1,-1,-1],[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]]
ez_data :: [[Int]]
ez_data = [[-1,-1,-1,-1,-1,-1,-1],[-1,-1,-1,-1,-1,1,-1],[-1,-1,9,-1,-1,-1,-1],[-1,-1,8,8,-1,-1,-1],[-1,-1,-1,-1,-1,4,-1],[-1,4,-1,5,-1,2,-1],[-1,-1,-1,-1,-1,-1,-1]]
ez_color :: [[Int]]
ez_color = [[-1,-1,-1,-1,-1,-1,-1],[-1,0,0,0,0,0,-1],[-1,0,0,0,0,0,-1],[-1,0,0,0,0,0,-1],[-1,0,0,0,0,0,-1],[-1,0,0,0,0,0,-1],[-1,-1,-1,-1,-1,-1,-1]]

after_1_step :: [[Int]]
after_1_step = [[-1,-1,-1,-1,-1,-1,-1],[-1,1,1,1,0,0,-1],[-1,1,1,1,0,0,-1],[-1,1,1,1,0,0,-1],[-1,1,1,0,0,0,-1],[-1,1,1,0,0,0,-1],[-1,-1,-1,-1,-1,-1,-1]]