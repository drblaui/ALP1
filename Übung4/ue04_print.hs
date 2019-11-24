-- helper functions that are basically higher order functions in haskell
-- This is product [a]
prod :: (Num a) => [a] -> a
prod = foldr (*) 1

-- This is take n [a]
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

{-
This probably takes way to many variables, but hey it works

We get our k, the first Element of the list, the rest of the list,
our current maximum product and the list of numbers that create it.
We then recursivly put together the first element and the, then
at k-1 splitted rest of the List, as a product and check wether it
exceeds our current maximum or not. 
If it does, we replace the maximum and the number list, if it 
doesn't, we just take the successor of our x and test the next
k-1 elements with it, until our original list is empty and
we can return the maxNum List
-}
maxProdOfHelper :: Int -> Int -> [Int] -> Int -> [Int] -> [Int]
maxProdOfHelper k x [] max xs = xs
maxProdOfHelper k x (y:xs) max maxNum
        | (p > max) = maxProdOfHelper k y xs p ([x] ++ splitList ([y] ++ xs) (k-1))
        | ((p < max) || (p == max)) = maxProdOfHelper k y xs max maxNum
        where
            p = (x * prod (splitList ([y]++xs) (k-1)))


--Aufgabe 3
--m bleibt für b weg
primes :: Int -> [Int]
primes n = takeWhile (<n) (sieb [2..])
            where
                sieb (l:xs) = l:sieb [x | x <- xs, mod x l /= 0]

weakGoldbachTriples :: Int -> [(Int,Int,Int)]
weakGoldbachTriples n 
    | (n `mod` 2 == 0) = error ("Input has to be an odd number")
    | otherwise = [(x,y,z) | x <- (primes n), y <- primes n, z <- primes n, (x+y+z == n), x <= y, y <= z]


{-
Since weak goldbach triples are only allowed with odd numbers(at least
the assignment says so), we only take the odd numbers until our m
-}
wGTriplesUntil :: Int -> Bool
wGTriplesUntil m = wGUHelper m 1

wGUHelper :: Int -> Int -> Bool
wGUHelper m n 
    | (n >= m) = True
    -- This is an anonymous function or a lamda expression. I use it, since it will come up in the lecture anyways
    | not(existIn False (map (\(x,y,z) -> ((x+y+z) == n)) (weakGoldbachTriples n))) = wGUHelper m (n+2)
    | otherwise = False


--Aufgabe 4
--stolen and modified from my own Assignment 02
trueDivisor :: Int -> [Int]
trueDivisor n = [a | a <- [1..n], n `mod` a == 0]

--If n gets bigger than 1000 the function takes a while to calculate, but whatever
{-
The good thing is, that we have the formula "1+2+3+6 = 2*6" on the assignment
This formula already tells us everything we have to do.

So we generate a list of numbers from 1 to n-1, because we want all
natural numbers smaller than our input. The list only maps numbers 
that are not only true divisors of our current number, but also 
it only maps if all those added equal our current number times 2

Pretty easy stuff I wrote within like 2 Minutes, if the basic formula
would have not been on the assignment, it would've taken me at least
an hour
-}
perfects :: Int -> [Int]
perfects n = [a| a <- [1..(n-1)], summ(trueDivisor a) == 2*a]


-- Aufgabe 5
counts :: (Num a, Eq b) => b -> [b] -> a
-- This spits an error without the fromIntegral and I respect that
{-
We just filter our list, so at the end we only have a list
with items that equal our x. We then look how long that list is
-}
counts x xs = fromIntegral(length(filter (==x) xs))

{-
We filter the list with the given condition and check if
we have more than one element left
-}
single :: (a -> Bool) -> [a] -> Bool
single cond xs = (length(filter cond xs) == 1)

{-
We filter the list with the given condition and check if
more than or exactly half of the original list remains
-}
mostly :: (a -> Bool) -> [a] -> Bool
mostly cond xs = (length(filter cond xs) >= ((length xs) `div` 2))

{-
I've looked up something to help me and I found out about
lamda expressions. We didn't talk about this in the 
lecture yet, but syllabus suggests we learn it, so it's
technically allowed, since it's didactic, right?
Since lamda expressions are just anonymous functions
without a name, it should be okay

This is a basic formula to convert binarys to decimal
We use foldr, because to convert binarys you start with the
least significant bit, wich is normally on the right
-}
bin2dec :: [Integer] -> Integer
bin2dec = foldr (\x y -> x + 2*y) 0