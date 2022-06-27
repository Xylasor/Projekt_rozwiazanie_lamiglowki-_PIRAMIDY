-- Projekt SPOP - rozwiazanie ladmiglowi - PIRAMIDY
-- Autor: Mateusz Hryciów, nr 283365
--
-- Program rozwiazuje lamiglowke metoda brutalna, generowane sa kolejne kwadraty
-- lacinskie i sprawdzane jest czy spelniaja one postawione wymagania.
--
-- Aby rozpoczac rozwiazywanie nalezy po uruchomieniu programu wpisac > main
-- Dla lamiglowki o zaprezentowanych nizej ograniczeniach oraz znanym rozwiazaniu
-- przedstawiono przyklad uzycia programu. Jezeli na danej pozycji nie istnieje
-- ograniczenie to nalezy wpisac 0.
-- 
-- Lamiglowka i rozwiazanie:
-- 		 3   1
-- 		 _ _ _ _  
-- 		|2 1 4 3|
-- 		|3 4 2 1|3
-- 	   4|1 2 3 4|
-- 		|4 3 1 2|
-- 		 - - - -  
-- 		
-- Przyklad uzycia:
-- > main
-- Witamy w programie sluzacym do rozwiazywania lamiglowki - PIRAMIDY
-- Podaj rozmiar lamiglowki
-- > 4
-- Podaj liste lewych ograniczen
-- > [0,0,4,0]
-- Podaj liste prawych ograniczen
-- > [0,3,0,0]
-- Podaj liste gornych ograniczen
-- > [3,0,1,0]
-- Podaj liste dolnych ograniczen
-- > [0,0,0,0]
-- Poszukuje rozwiazania...
-- Rozwiazanie lamiglowki to:
-- [2,1,4,3]
-- [3,4,2,1]
-- [1,2,3,4]
-- [4,3,1,2]


import qualified Data.List as L

main = do 
    putStrLn "Witamy w programie sluzacym do rozwiazywania lamiglowki - PIRAMIDY" 
    putStrLn "Podaj rozmiar lamiglowki"  
    input1 <- getLine
    let n = read input1 :: Int

    putStrLn "Podaj liste lewych ograniczen" 
    input <- getLine
    let left = read input :: [Int]

    putStrLn "Podaj liste prawych ograniczen" 
    input <- getLine
    let right = read input :: [Int]

    putStrLn "Podaj liste gornych ograniczen" 
    input <- getLine
    let top = read input :: [Int]

    putStrLn "Podaj liste dolnych ograniczen" 
    input <- getLine
    let bot = read input :: [Int]

    putStrLn "Poszukuje rozwiazania..."
    putStrLn "Rozwiazanie lamiglowki to:"
    solveAll (perms (generateList n)) left right top bot



-- Funkcja sluzaca do rozwiazywania zagadki.
-- Jako parametry nalezy podac liste wszystkich permutacji o n-elementach oraz listy ograniczen
-- W przypadku znalezienia rozwiazania funkcja wyswietla je na konsoli
-- Jezeli rozwiazanie nie zostalo znalezione wyswietla stosowny komunikat
solveAll [] _ _ _ _ = putStrLn "Jednak nie istnieje rozwiazanie spelaniajace podane wymagania. :("
solveAll (x:xs) left right top bot
   | solve (findLatinSqs x) left right top bot == Nothing = solveAll xs left right top bot
   | otherwise = printSolution (fromJust (solve (findLatinSqs x) left right top bot))


-- Funkcja sprawdzajaca czy dla wybranej permutacji istenieje rozwiazanie spelniajace ograniczenia
-- Jako parametry nalezy podac liste wszystkich kwadratow lacinskich pochodzacych z danej permutacji oraz
-- listy wszystkich ograniczen
-- W przypadku znalezienia satysfakcjonujacego rozwiazania jest ono zwracane, natomiast
-- jezeli zaden z kwadratow nie spelnia wymagan zwracane jest Nothing
solve [] _ _ _ _ = Nothing
solve (x:xs) left right top bot
   | ifValidRows x left && ifValidRows (map reverse x) right && ifValidRows (L.transpose x) top && ifValidRows (map reverse (L.transpose x)) bot = Just (x)
   | otherwise = solve xs left right top bot


-- Funkcja sprawdza czy dany kwadrat lacinski spelnia wybrane ograniczenie (prawe,lewe,gorne lub dolne)
-- Jako parametry nalezy podac wybrany kwadrat lacinski oraz liste zawierajaca jedno ze wspomnianych ograniczen
-- Jezeli element na liscie ograniczen jest rowny 0 to jest on ignorowany
-- Funkcja zwraca True jezeli wymagania sa spelnione oraz False w przeciwnym przypadku
ifValidRows :: [[Int]] -> [Int] -> Bool 
ifValidRows [] [] = True 
ifValidRows (x:xs) (l:ls) 
   | l == 0 = ifValidRows xs ls
   | calculatePeaks x == l = ifValidRows xs ls
   | otherwise = False 

-- Funkcja sprawdza czy dany rzad lub kolumna spelnia wymagania
-- Jako parametry nalezy podac jeden rzad lub kolumne oraz ograniczenie
-- Funkcja zlicza i zwraca liczbe szczytow widoczna z danej perspektywy
calculatePeaks :: [Int] -> Int
calculatePeaks [x] = 1
calculatePeaks (x:y:ys) | x < y = 1 + calculatePeaks (y:ys)
                        | otherwise = calculatePeaks (x:ys)

-- Funkcja generuje liste od 1 do N
generateList :: Int -> [Int]
generateList n = [1..n]

-- Funkcja generujaca kwadraty lacinskie na podstawie wybranej permutacji poprzez zamieniane rzedow
-- Jako parametry przyjmuje wybrana permutacje oraz zwraca wszystkie kwadraty lacinskie na niej oparte
-- Zrodlo funkcji: https://hackage.haskell.org/package/HaskellForMaths-0.4.8/docs/src/Math-Combinatorics-LatinSquares.html?fbclid=IwAR0pgC4Kc52p5e8DjVIJiR8Yffd2Q8fuezKZAgF-P2b5YJgGp-522u43F6c
findLatinSqs :: (Eq a) => [a] -> [[[a]]]
findLatinSqs xs = findLatinSqs' 1 [xs] where
    n = length xs
    findLatinSqs' i rows
        | i == n    = [reverse rows]
        | otherwise = concat [findLatinSqs' (i+1) (row:rows)
                             | row <- findRows (L.transpose rows) [] xs]
    findRows (col:cols) ls rs = concat [findRows cols (r:ls) (L.delete r rs)
                                    | r <- rs, r `notElem` col]
    findRows [] ls _ = [reverse ls]


-- Funkcja generujaca wszystkie permutacje danej listy liczb na podstawie listy wartosci
-- Jako parametry przyjmuje liste wartosci oraz zwraca ich wszystkie permutacje
perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:p | x <- xs, p <- perms (remove x xs)]
    where
        remove x [] = []
        remove x (y:ys) | x == y = ys
                        | otherwise = y : remove x ys

-- Funkcja wyswietla na ekranie rozwiazanie lamiglowki, ktore jest
-- pewnym kwadratem lacinskim
printSolution :: [[Int]] -> IO ()
printSolution [] = return ()
printSolution (x:xs) = do
    print x
    printSolution xs


-- Funkcja pobiera obiekt Just a i zwraca wartosc a
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Brak wartosci"