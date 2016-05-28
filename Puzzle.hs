-- ----------------------------------------------------------------------------
-- Puzzle.hs
-- WERSJA 0.1
-- ZADANIE 8 (ABC)
-- ----------------------------------------------------------------------------
module Puzzle where

data Puzzle = Puzzle Int Int Char [(Int, Int, Char)]
type Result = [[Char]]
type Solver = Int -> Int -> Char -> [(Int, Int, Char)] -> [Result]

data Test
    = SimpleTest Puzzle Result
    | CountTest  Puzzle Int

runSolver :: Solver -> Puzzle -> [Result]
runSolver solver (Puzzle m n l fs) = solver m n l fs

checkSolution :: Result -> Result -> Bool
checkSolution = ( == )

baseTests :: [Test]
baseTests = [SimpleTest (Puzzle 4 4 'C' [(3,1,'B'), (4,1,'A'), (1,2,'B'), (4,2,'C'), (1,3,'C'), (2,4,'A')]) [['B','B','C','C'], ['A','C','C','A'], ['B','B','C','C'], ['A','C','C','A']], CountTest (Puzzle 2 2 'B' [(1,1,'A'),(2,2,'A')]) 3]