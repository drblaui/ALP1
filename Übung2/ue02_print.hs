
--Aufgabe 2
sumOfTeiler :: Int -> Int -> Int -> Int
sumOfTeiler n a b 
            | n == 0 = 0
            | ((n `mod` a == 0) || (n `mod` b == 0)) = n + sumOfTeiler (n-1) a b
            | otherwise = 0 + sumOfTeiler (n - 1) a b



--Aufgabe 3
-- Wir lassen die Signatur weg, weil Haskell sonst immer meckert
factorial n
        | n == 0 = 1
        | n > 0 = n * factorial(n-1)
        | n < 0 = error "not defined for negative numbers"


euler :: Float -> Float
euler n 
       | n == 0 = 1
       | otherwise = (1 / factorial(n)) + euler(n-1)


--Aufgabe 4
insertInPos :: Char -> Int -> [Char] -> [Char]
insertInPos char offset xs
            | ((offset < 0) || (offset > length xs )) = error "This offset is not reachable inside the list"
            | (offset == 0) = char : xs
            | otherwise =((fst(splitAt(offset) xs)) ++ (char : (snd(splitAt(offset) xs))))

--Aufgabe 5
trueDivisor :: Int -> [Int]
-- Aint list comprehension somethin beautiful 
trueDivisor n = [a | a <- [1..(n-1)], n `mod` a == 0]

--Aufgabe 6
--What the fuck?
