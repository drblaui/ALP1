-- helper functions that are basically higher order functions in haskell
-- This is product [a]
prod :: (Num a) => [a] -> a
prod = foldr (*) 1

-- This is Split at but it always gives the first elements
splitList :: [a] -> Int -> [a]
splitList [] offset = []
splitList xs 0 = []
splitList (x:xs) offset = x:splitList xs (offset - 1)

-- This is sum [a]
summ :: (Num a) => [a] -> a
summ = foldr (+) 0

--Aufgabe 1
{-
 [(n,m) | n <- [1..3], m <- [3,2..0], n /= m]
 1. Tupel: (1,3), da n bei 1 und m bei 3 beginnt 
 und 1 ungleich 3 ist
 
 2. Tupel: (1,2), da die Listcomprehension jedes
 Element von n mit jedem Element von m vergleicht.
 Deswegen ist die erste Stelle im Tupel noch immer 1.

 3. Tupel: (1,0), da das Tupel 1,1 nicht beachtet wird,
 denn 1 == 1

 4. Tupel: (2,3), hier geht der vergleich mit dem kompletten
 m wieder von vorne los

 Geht das dann immer weiter, bis n 1 und m 0 erreicht hat,
 erhält man folgende Liste:
 [(1,3),(1,2),(1,0),(2,3),(2,1),(2,0),(3,2),(3,1),(3,0)]
-}


--Aufgabe 2
maxProdOf :: Int -> [Int] -> [Int]
maxProdOf k xs | (k > (length xs)) = error("k exceeds the List length")
maxProdOf k [] = []
maxProdOf k (x:xs) = maxProdOfHelper k x xs 0 []


maxProdOfHelper :: Int -> Int -> [Int] -> Int -> [Int] -> [Int]
maxProdOfHelper k x [] max xs = xs
maxProdOfHelper k x (y:xs) max maxNum
        | (p > max) = maxProdOfHelper k y xs p ([x] ++ splitList ([y] ++ xs) (k-1))
        | ((p < max) || (p == max)) = maxProdOfHelper k y xs max maxNum
        where
            p = (x * prod (splitList ([y]++xs) (k-1)))


--Aufgabe 3
primzahlen :: Int -> [Int]
primzahlen n = sieb[2..n]
                where
                    sieb [] = []
                    sieb (p:xs) = p:sieb[k|k<-xs, (mod k p) > 0]


--Aufgabe 4
--stolen and modified from my own Assignment 02
trueDivisor :: Int -> [Int]
trueDivisor n = [a | a <- [1..n], n `mod` a == 0]

--If n gets bigger than 1000 the function takes a while to calculate, but whatever
perfects :: Int -> [Int]
perfects n = [a| a <- [1..(n-1)], summ(trueDivisor a) == 2*a]


-- Aufgabe 5
counts :: (Num a, Eq b) => b -> [b] -> a
-- This spits an error without the fromIntegral
counts x xs = fromIntegral(length(filter (==x) xs))
