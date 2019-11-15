--Since we are not allowed to use `elem`
-- This is basically what I feel about that: https://bit.ly/351tDa0
existIn :: Eq a => a -> [a] -> Bool
existIn x [] = False
existIn x (y:xs)
        | (x == y) = True
        | otherwise = existIn x (xs)

-- Aufgabe 1
{-
Since we cant convert directly from oct to hex, or at least
I dont know how, we first have to convert the number to a binary
There is probably an easier way to do this, but at leas this works

Note: For the whole assignment, we just expect our Input to not have
any '.' inbetween the numbers or else our bin2hex would be waaaayyy longer
-}
okt2bin :: [Char] -> [Char]
okt2bin [] = []
okt2bin (x:xs) = case x of
    '0' -> "000" ++ okt2bin xs
    '1' -> "001" ++ okt2bin xs
    '2' -> "010" ++ okt2bin xs
    '3' -> "011" ++ okt2bin xs
    '4' -> "100" ++ okt2bin xs
    '5' -> "101" ++ okt2bin xs
    '6' -> "110" ++ okt2bin xs
    '7' -> "111" ++ okt2bin xs
    otherwise -> error "No Oct"

{-
We then convert the binary to the wished for hexadecimal
This is probably also can be done a lot easier
-}
bin2hex :: [Char] -> [Char]
bin2hex xs 
        | x == "0000" = "0" ++ bin2hex y
        | x == "0001" = "1" ++ bin2hex y
        | x == "0010" = "2" ++ bin2hex y
        | x == "0011" = "3" ++ bin2hex y
        | x == "0100" = "4" ++ bin2hex y
        | x == "0101" = "5" ++ bin2hex y
        | x == "0110" = "6" ++ bin2hex y
        | x == "0111" = "7" ++ bin2hex y
        | x == "1000" = "8" ++ bin2hex y
        | x == "1001" = "9" ++ bin2hex y
        | x == "1010" = "A" ++ bin2hex y
        | x == "1011" = "B" ++ bin2hex y
        | x == "1100" = "C" ++ bin2hex y
        | x == "1101" = "D" ++ bin2hex y
        | x == "1110" = "E" ++ bin2hex y
        | x == "1111" = "F" ++ bin2hex y
        | x == [] = []
        | otherwise = error "No bin"
        where
            x = take 4 xs
            y = drop 4 xs

{-
We need to add leading zeroes, so we can divide it
into four element in bin2hex
-}
oktCreator :: [Char] -> [Char]
oktCreator [] = []
oktCreator xs = case (length xs `mod` 4) of
        0 -> xs
        1 -> "000" ++ xs
        2 -> "00" ++ xs
        3 -> "0" ++ xs

{-
First we comvert the number so an bin, then we give it
an octagonal structure and finaly we convert it to hex
-}
okt2hex :: [Char] -> [Char]
okt2hex okt = bin2hex (oktCreator (okt2bin (okt)))


--Aufgabe 2
-- Code out of lecture
ggt :: Int -> Int -> Int
ggt a b
    | b == 0 = a
    | otherwise = ggt b (a `mod` b)

{-
We take the first two numbers of the list and
replace them with their divider. We then check
the rest of the list with the divider before the item,
until only one number is left, which is our biggest possible divider
-}
ggt_of :: [Int] -> Int
ggt_of [] = error "Input was an empty list"
ggt_of ([x]) = x
ggt_of (x:y:xs) = ggt_of([ggt x y] ++ xs)

--Aufgabe 3
{-
This is simple:
Find a Element that is not any type of bracket?
Just skip it!
-}
balance :: [Char] -> Bool
balance text = bal [] text
    where
    bal :: [Char] -> [Char] -> Bool
    bal [] [] = True
    bal stapel ('(':xs) = bal (')':stapel) xs
    bal stapel ('[':xs) = bal (']':stapel) xs
    bal stapel ('{':xs) = bal ('}':stapel) xs
    bal (s:stapel) (x:xs) | s==x = bal stapel xs
    bal stapel (x:xs) | (not(existIn x ['(',')','[',']','{','}'])) = bal stapel xs
    bal _ _ = False


--Aufgabe 4
{-
Base 1 basically does not exist, so we throw an error
to avoid an stack overflow, because the calculations
on Base 1 would never stop
Base conversion from 2 to 10 is easy, you just
divide the number by the base until it reaches 0, the
rest of the operation reversed then forms your new number
-}
decTo :: Int -> Int -> [Int]
decTo n x
        | ((n <= 0) || (n >= 10)) = error ("Base " ++ show n ++ " not allowed")
        | ((x `div` n) == 0) = [x `mod` n]
        | otherwise = decTo n (x `div` n) ++ [x `mod` n]


--Aufgabe 5
{-
Recursivly working through the list until we 
either reach the end or until two elements are not in
the wished order
-}
isSorted :: Ord a => (a -> a -> Bool) -> [a] -> Bool
isSorted op [] = True
isSorted op ([x]) = True
isSorted op (x:y:xs) 
                   | (op x y) = isSorted op (y:xs)
                   | otherwise = False


--Aufgabe 6 
reverseDigits :: Int -> Int
reverseDigits x
    | (x < 0) = error "No negative Numbers allowed"
    | otherwise = reverseDigitsHelper x 0

reverseDigitsHelper :: Int -> Int -> Int
reverseDigitsHelper 0 lastDigit = lastDigit
-- This basic formula is from https://bit.ly/2NJtjqU
-- And the amount of hours spent on this one line is about 3, which is sad
reverseDigitsHelper number lastDigit = reverseDigitsHelper (number `div` 10) ((lastDigit * 10) + number `mod` 10)


--Aufgabe 7
{-
Iterating through the list with a helper that
tracks the current position and at the same
time creates the output list

I also have no idea if this is the ominous "Akkumulatortechnik",
since Miss Esponda either never explained it correctly or 
I just didn't get it, but anyways this function works 
and is recursive, so i guess should be enough?
-}
poss :: Eq a => a -> [a] -> [Int]
poss e (x:xs) = possHelper e x xs 0

possHelper :: Eq a => a -> a -> [a] -> Int -> [Int]
possHelper e x [] pos
                | (e == x) = [pos]
                | otherwise = []
possHelper e x (y:ys) pos
                | (e == x) = [pos] ++ possHelper e y ys (pos + 1)
                | otherwise = possHelper e y ys (pos + 1)


--Aufgabe 8
-- Functions out of the lecture
random :: Int -> Int
random seed = ((25173 * seed + 13849) `mod` 65536) `mod` 10000

randList :: Int -> [Int]
randList n = randList' n [random n]
                where
                    randList' n xs
                        | (length xs) == n = xs
                        | otherwise = randList' n (a:xs)
                        where
                            a = random (head xs)

{-
I don't know, if the function was supposed to look like this
or even to what it does, since it wasn't explained, because it 
only gives Extra Points, but this sould at least give enough points
to compensate for poss if needed and also help me with that missing
pont from the last assignment......right?
-}
{-
Get Element, put it in checked off list,
test if next element is in that list and
give back the element list
-}
randWithoutReps :: Int -> [Int]
randWithoutReps n = helper (randList n) []
                            where 
                                helper :: [Int] -> [Int] -> [Int]
                                helper (x:xs) ys
                                    | (existIn x ys) = ys
                                    | otherwise = helper xs (ys ++ [x])
