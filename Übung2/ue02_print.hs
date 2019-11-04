
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


euler :: Double -> Double
euler n 
       | n == 0 = 1
       | otherwise = (1 / factorial(n)) + euler(n-1)


--Aufgabe 4
--TODO: Make recurisve
{-
Ich möchte nur mal anmerken, dass diese Funktion hier nicht nur viel einfacher, sondern laufzeiteffektiver wäre:
insertInPos char offset xs
            | ((offset < 0) || (offset > length xs )) = error "This offset is not reachable inside the list"
            | (offset == 0) = char : xs
            | otherwise = ((fst(splitAt(offset) xs)) ++ (char : (snd(splitAt(offset) xs))))
-}
insertInPos :: Char -> Int -> [Char] -> [Char]
insertInPos char offset xs 
            | ((offset < 0) || (offset > length xs )) = error "This offset is not reachable inside the list"
            | (offset == 0) = char : xs
            | otherwise = insertInPosHelper [head xs] (tail xs) char offset

insertInPosHelper :: [Char] -> [Char] -> Char -> Int -> [Char]
insertInPosHelper xs ys char offset
                | (length xs == offset) = xs ++ (char:ys)
                | otherwise = insertInPosHelper (xs ++ [head ys]) (tail ys) char offset
--Aufgabe 5
trueDivisor :: Int -> [Int]
-- Aint list comprehension somethin beautiful 
trueDivisor n = [a | a <- [1..(n-1)], n `mod` a == 0]

--Aufgabe 6
--a
negation :: Int -> Int
negation n = -(n - 1)

und :: Int -> Int -> Int
und a b = a * b

oder :: Int -> Int -> Int
oder a b = negation(und (negation a) (negation b))

exoder :: Int -> Int -> Int
exoder a b = oder(und (negation a) b) (und a (negation(b)))

--b
hamming_distance :: [Int] -> [Int] -> Int
hamming_distance [] [] = 0
hamming_distance _ [] = error "Bit-Lists differ in length"
hamming_distance [] _ = error "Bit-Lists differ in length"
hamming_distance (x:xs) (y:ys) = exoder x y + hamming_distance xs ys
