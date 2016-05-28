------------------------------------------
------------------------------------------
--					--
--	Konrad Cielecki (273278)	--
-- 	   Zagadka nr 8 (ABC)		--
--					--
------------------------------------------
------------------------------------------


import Puzzle
import Checker

-------------------------------------------
--SOLVE------------------------------------
-------------------------------------------


type Board = [[Int]]
alphabet = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']

--Funkcja zwracajaca liste wszystkich rozwiazan
solve :: Solver
solve n m letter list = nextSolution n leInNumber firstResult True
	where 	firstResult = findFirst leInNumber (fill (array n m) list n m)
		leInNumber = charToInt letter alphabet 0

--Tworzy liste poprawnych rozwiazan
nextSolution :: Int -> Int -> Board -> Bool -> [Result]
nextSolution n leInNum prevResult isItFirst
	| correct2 n (nextResult) == []		= []
	| otherwise		  		= toChar (rev nextResult) : nextSolution n leInNum nextResult False			
	where nextResult = solve2 leInNum prevResult isItFirst


--Bierze rozwiazanie podane jako argument i szuka kolejnego (poprawnego)
solve2 :: Int -> Board -> Bool -> Board
solve2 leInNumber result isItNext
	| (cond1==True && cond2==True && cond3==True && isItNext==True) || result == []	= result
	| otherwise 									= solve2 leInNumber (next leInNumber result) True
	where 	resultMirror = mirror result 0				-- Zamienia kolumny w wiersze
		cond1 = correct leInNumber result			-- Sprawdza czy wiersze sa poprawne
	  	cond2 = correct leInNumber resultMirror 		-- Sprawdza czy kolumny sa poprawne
		cond3 = oneSpecial result || oneSpecial resultMirror	-- Sprawdza czy istnieje jeden "wyjatkowy" wiersz/kolumna
	
-----------------------------------------------------------------------------

-- Tworzy tablice o wymiarach 'n' x 'm' wypelniona liczbami
array :: Int -> Int -> Board
array n m = row n (row m 0)


-- Tworzy wiersz o dlugosci 'y' z elementami 'x'
row :: Int -> a -> [a]
row 0 _ = []
row y x = x:row (y-1) x


-- Wypelnia tablice elementami podanymi w specyfikacji
fill :: Board -> [(Int, Int, Char)] -> Int -> Int -> Board
fill [] _ _ _ = []
fill (x:arr) elements n m = fillRow x elements n m : fill arr elements (n-1) m


-- Wypelnia wiersz elementami podanymi w specyfikacji
fillRow :: [Int] -> [(Int, Int, Char)] -> Int -> Int -> [Int]
fillRow [] _ _ _ = []
fillRow (x:arr) elements n m = fillSlot elements n m : fillRow arr elements n (m-1)


-- Wypelnia slot odpowiednia litera (o ile zostala podana)
fillSlot :: [(Int, Int, Char)] -> Int -> Int -> Int
fillSlot [] n m = 0
fillSlot ((x,y,l):elements) n m 
	| x==n && y==m 		= 26 + charToInt l alphabet 0 
	| elements == [] 	= 0
	| otherwise 		= fillSlot elements n m


-- "Zmienia" Char w Int
charToInt :: Char -> [Char] -> Int -> Int
charToInt _ [] _ = 0
charToInt letter (x:alph) n
	| x==letter 		= n
	| otherwise 		= charToInt letter alph (n+1)  

----------------------------------------------------------------------------

-- Ustawia kazdy wiersz w 'pozycji startowej', czyli szuka takiej pierwszej kombinacji, aby wystapenia poszczegolnych liter byly sobie rowne
findFirst :: Int -> Board -> Board
findFirst _ [] = []
findFirst leInNum (x:xs) 
	| correct leInNum [x] 	= x : findFirst leInNum xs
	| otherwise 		= next leInNum [x] ++ findFirst leInNum xs


----------------------------------------------------------------------------


-- Zamienia liczby w tablicy na odpowiadajace im litery
toChar :: Board -> Result
toChar [] = []
toChar (x:xs) = rowToChar x : toChar xs

-- Zamienia liczby w liscie na odpowiadajace im litery
rowToChar :: [Int] -> [Char]
rowToChar [] = []
rowToChar (x:xs) = letterToChar k alphabet : rowToChar xs
	where k = x `mod` 26

-- Zmienia liczbe w odpowiadajaca jej litere (wyciaga x-ty element z danej listy)
letterToChar :: Int -> [a] -> a
letterToChar 0 (head:_) = head
letterToChar x (head:list) = letterToChar (x-1) list

-----------------------------------------------------------------------------

-- Odrawca tablice z wynikami
rev :: [[a]] -> [[a]]
rev [] = []
rev (x:xs) = rev xs ++ [revL x]

-- Odwraca liste
revL :: [a] -> [a]
revL [] = []
revL (x:xs) = revL xs ++ [x]

-----------------------------------------------------------------------------


-- Funkcja bierze tablice (ktora okazala sie nie spelniac kryteriow) i szuka kolejnej, ktora mozliwe, ze bedzie poprawna
next :: Int -> Board -> Board
next _ [] = []
next leInNum (x:xs)
	| snd row == True 		= fst row : xs
	| xs /= []			= fst row : next leInNum xs
	| otherwise			= []
	where row = searchNext leInNum x

-- Funkcja szuka kolejnego POPRAWNEGO wiersza
searchNext :: Int -> [Int] -> ([Int],Bool)
searchNext leInNum row 
	| zero temp == True 				= (temp,False)
	| checkV (count leInNum temp 0) == True 	= (temp,True)
	| otherwise 					= searchNext leInNum temp
	where temp = nextRow leInNum row

-- Funkcja zwraca kolejny wiersz (niekoniecznie poprawny)
nextRow :: Int -> [Int] -> [Int]
nextRow _ [] = []
nextRow leInNum (x:xs) 
	| x>25 			= x : nextRow leInNum xs
	| x==leInNum		= 0 : nextRow leInNum xs
	| otherwise		= (x+1):xs


-- Sprawdza, czy zostaly sprawdzone wszystkie kombinacje dla tego wiersza
zero :: [Int] -> Bool
zero [] = True
zero (x:xs)
	| 0<x && x<26 		= False
	| otherwise		= zero xs

-- Sprawdza, czy litery w wierszu zawieraja sie tyle samo razy
checkV ::  [Int] -> Bool
checkV []  = True
checkV [x] = True
checkV (x:y:xs) 
	| x `mod` 26 == y `mod` 26	= checkV (y:xs)
	| otherwise 			= False 

-- Zlicza wystepowanie elementow w danym wierszu
count :: Int -> [Int] -> Int -> [Int]
count leInNum list n
	| n <= leInNum 	&& temp>0	= temp : count leInNum list (n+1)
	| n <= leInNum			= count leInNum list (n+1)
	| otherwise 			= []
	where temp = howM n list

-- Zlicza wystepowanie elementu `e` w wierszu
howM :: Int -> [Int] -> Int
howM _ [] = 0
howM e (x:xs)
	| e== x `mod` 26	= 1 + howM e xs
	| otherwise 		= howM e xs

-----------------------------------------------------------------------------

-- Zamienia kolumny w wiersze
mirror :: Board -> Int -> Board
mirror [] _ = []
mirror (x:xs) n 
	| n+1< length x		= get (x:xs) n : mirror (x:xs) (n+1)
	| otherwise		= [get (x:xs) n]

-- Wyciaga `n`-ty element z kazdego wiersza i tworzy liste
get :: Board -> Int -> [Int]
get [] _ = []
get (x:xs) n = (x !! n) : get xs n


-----------------------------------------------------------------------------


-- Sprawdza, czy liczby w kazdym wierszu tablicy zawieraja sie tyle samo razy
correct :: Int -> Board -> Bool
correct _ [] = True
correct leInNum (x:xs)
	| checkV (count leInNum x 0) == True  	= correct leInNum xs
	| otherwise 				= False


-- Sprawdza, czy tablica nie zostala ucieta, jezeli tak, to znaczy, ze nie znaleziono rozwiazania
correct2 :: Int -> [a] -> [a]
correct2 n xs
	| n == length xs 	= xs
	| otherwise		= []

-- Sprawdza, czy istnieje wiersz, ktory jest caly zlozony z jednej litery
oneSpecial :: Board -> Bool
oneSpecial [] = False
oneSpecial (x:xs)
	| checkV x == True	= True
	| otherwise 		= oneSpecial xs

-------------------------------------------
--TESTY------------------------------------
-------------------------------------------

tests :: [Test]
tests = [
	SimpleTest (Puzzle 1 1 'C' []) 
		[['B']],
	SimpleTest (Puzzle 6 6 'G' [(1,1,'A'),(1,2,'C'),(1,4,'D'),(1,6,'D'),(2,1,'B'),(2,3,'D'),(2,5,'B'),(3,2,'C'),(3,3,'A'),(4,1,'F'),(4,5,'E'),(4,6,'D'),(5,2,'G'),(5,3,'D'),(5,6,'B'),(6,1,'B'),(6,2,'B'),(4,2,'G'),(6,6,'B'),(4,3,'B'),(3,5,'C'),(2,2,'B')]) 
		[['A','C','A','D','C','D'],
		 ['B','B','D','D','B','D'],
		 ['A','C','A','B','C','B'],
		 ['F','G','B','C','E','D'],
		 ['F','G','D','C','E','B'],
		 ['B','B','B','B','B','B']],
	SimpleTest (Puzzle 2 2 'D' [(1,1,'A'),(2,1,'D')]) 
		[['A','C'],
		 ['D','C']],
	SimpleTest (Puzzle 6 4 'C' [(3,1,'B'), (4,1,'A'), (1,2,'B'), (4,2,'C'), (1,3,'C'), (2,4,'A'),(5,3,'B')]) 
		[['B','B','C','C'],
		 ['A','A','A','A'],
		 ['B','B','B','B'],
		 ['A','C','A','C'],
		 ['C','C','B','B'],
		 ['C','A','C','A']],
	SimpleTest (Puzzle 4 2 'E' [(1,1,'E'),(1,2,'B')])
		[['E','B'],
		 ['B','A'],
		 ['C','C'],
		 ['A','D']],
	CountTest  (Puzzle 3 3 'B' [(1,1,'A'),(1,2,'A'),(1,3,'B')]) 0,
	CountTest  (Puzzle 6 4 'C' [(3,1,'B'), (4,1,'A'), (1,2,'B'), (4,2,'C'), (1,3,'C'), (2,4,'A'),(5,3,'B'),(1,1,'B'),(2,1,'C'),(2,2,'C')]) 2,
	CountTest  (Puzzle 2 2 'B' [(1,1,'A'),(2,1,'B')]) 3,
	CountTest  (Puzzle 4 3 'E' [(1,1,'C'),(2,1,'E'),(3,2,'A'),(1,3,'D'),(4,2,'B'),(3,3,'C')]) 3,
	CountTest  (Puzzle 2 2 'B' []) 14
	]

-------------------------------------------
--MAIN-------------------------------------
-------------------------------------------

main :: IO ()
main = checkerMain solve tests

-------------------------------------------
--OPIS METODY------------------------------
-------------------------------------------
{-

1. Najpierw jest tworzona tablica cala wypelniona liczba '0'. Kazda liczba odpowiada danej literze, (0-A,1-B,...,25-Z).
2. Nastepnie tablica jest wypelniana("fill") elementami ze specyfikacji (!! TABLICA JEST CALA ODWROCONA, czyli element (1,1,'A') jest ostatnim elementem ostatniej listy, dopiero po znalezieniu rozwiazania taka tablica jest odwracana do pierwotnej postaci i dolaczana do listy rozwiazan!!)
	- Kazda litera podana w specyfikacji odpowiada poszczegolnym liczba (26-A,27-B,...,51-Z). Sa one wieksze, aby mozna bylo odroznic STALA litere od tej, ktora mozna modyfikowac. (Przy porownywaniu liczb jest uzywana funkcja `mod`)
3. Za pomoca funkcji "findFirst" kazdy wiersz zostaje ustawiony w taki sposob, aby wystapienia roznych liter w danym wierszu byly sobie rowne.
4. Pozniej funkcja "solve2" sprawdza czy tak ustawiona tablica jest poprawnym rozwiazaniem zagadki (czyli cond1 - poprawnosc wierszy, cond2 - poprawnosc kolumn, cond3 - czy istnieje jeden wiersz/kolumna wypelniona cala jedna litera)
 	- Ta funkcja dziala jak petla - poki nie znajdzie poprawnego rozwiazania (jezeli przejdzie po wszystkich mozliwych rozwiazaniach, to zwraca liste pusta)
5. Przy kazdym znalezieniu poprawnego rozwiazania, taka tablica zostaje odwrocona("rev") i liczby zostaja zamienione na litery("toChar") po czym jest ona dolaczana do listy rozwiazan (za pomoca rekurencyjnej funkcji "nextSolution")

* Tablica na samym poczatku jest odwrocona, aby funkcja szla "po kolei" (ale mozna to bylo zrobic rowniez innym sposobem, przy szukaniu rozwiazan nie ma to tak naprawde znaczenia), 
	np. dla wywolania "solve 1 3 'C' []", kolejne rozwazane rozwiazania:
		[A,A,A]
	    	[A,A,B]
	    	[A,A,C]
		[A,B,A]
		[A,B,B]
		  ...
		[C,C,B]
		[C,C,C]

* Ogolnie program jest dosyc wolny, jedyne usprawnienie to takie, ze zawsze jest szukany POPRAWNY wiersz.

-}


