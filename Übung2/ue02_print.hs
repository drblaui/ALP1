{-
 Aufgabe 1
 I think I don't really have to explain this? Since it's basic recursion

 collList 5 => (5):collList(next 5) => (5):collList(16) => (5):(16):collList(next 16)
 => (5):(16):collList(8) => (5):(16):(8):collList(next 8) => (5):(16):(8):collList(4)
 => (5):(16):(8):(4):collList(next 4) => (5):(16):(8):(4):collList(2)
 => (5):(16):(8):(4):(2):collList(next 2) => (5):(16):(8):(4):(2):collList(1)
 => (5):(16):(8):(4):(2):[1] => [5,16,8,4,2,1]
-}

--Aufgabe 2
sumOfTeiler :: Int -> Int -> Int -> Int
{-
 Assignment states we only use whole positive Numbers, which most Computer Scientists
 see as every Number without 0, so we give a error if anything is 0 or below
 n can be 0 though, because of the function wouldn't work with the input 1 1 1
-}
sumOfTeiler n a b 
        | ((n < 0) || (a < 1) || (b < 1))  = error "not defined for numbers < 0"
        -- Recursion Anchor that does not manipulate output
        | n == 0 = 0
        -- if n is divideable by a or b we add it with tbe next recursion step
        | ((n `mod` a == 0) || (n `mod` b == 0)) = n + sumOfTeiler (n-1) a b
        -- if not we dont manipulate the result and add the next recursion step
        | otherwise = 0 + sumOfTeiler (n - 1) a b



--Aufgabe 3
-- we don't add a signature here, because Haskell always complains
factorial n
        | n == 0 = 1
        | n > 0 = n * factorial(n-1)
        | n < 0 = error "not defined for negative numbers"


euler :: Double -> Double
euler n 
        -- The factorial of 0 is always 1, I don't know why it is 1 but I accept it
        | n == 0 = 1
        -- This is the formula from the assignment but recursive
        | otherwise = (1 / factorial(n)) + euler(n-1)


--Aufgabe 4
{-
Just wanted to say, that this version would be way easier and would have a better operating time
insertInPos char offset xs
| ((offset < 0) 
  || (offset > length xs )) = error "This offset is not reachable inside the list"
| (offset == 0) = char : xs
| otherwise = ((fst(splitAt(offset) xs)) ++ (char : (snd(splitAt(offset) xs))))
-}
insertInPos :: Char -> Int -> [Char] -> [Char]
insertInPos char offset xs 
        --the offset can not be negative or more than the length of the list
        | ((offset < 0) || (offset > length xs )) 
                = error "This offset is not reachable inside the list"
        -- If the offest is 0 it's just the beginning of the list
        | (offset == 0) = char : xs
        -- the helper function gets the first element and all other elements
        | otherwise = insertInPosHelper [head xs] (tail xs) char offset

{-
 I guess this is a kinda weird approach. Basically in the first step we get a single element and a whole
 list we then check if we are already where we wanted to put the char, if we're not there, we just
 simply add the first element of the list to our element and try it again. We do this as often as needed,
 until our first list is the length of the offset. If we reach the desired length, we add the first list
 together with the rest of the list with our char as new head
-}
insertInPosHelper :: [Char] -> [Char] -> Char -> Int -> [Char]
insertInPosHelper xs ys char offset
        | (length xs == offset) = xs ++ (char:ys)
        | otherwise = insertInPosHelper (xs ++ [head ys]) (tail ys) char offset

--Aufgabe 5
trueDivisor :: Int -> [Int]
-- Aint list comprehension something beautiful?
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
{-
 I know this function is bigger than it has to be, 
 but printing it without the where would look weird
-}
hamming_distance :: [Int] -> [Int] -> Int
hamming_distance xs ys
        | (not(length xs == length ys)) = error "Bit-Lists differ in length"
        -- no need to check for ys, because of lazy evaluation and the lists HAVE to be the same
        | (length xs == 0) = 0
        | otherwise = exoder hx hy + hamming_distance tx ty
        where
          hx = head xs
          hy = head ys
          tx = tail xs
          ty = tail ys
