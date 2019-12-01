-- Aufgabe 1
{-
Selection Sort runs with O(n²) as far as I know, 
BUT either the calculateFirst or deleteElem
are not in the normal Implementation or
I made a big mistake.
This just calculates either the biggest or 
smallest Element of the list, puts it at the beginning and 
does the same for the rest of the list without the element
-}
selectSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
selectSort op [x] = [x] -- O(1)
selectSort op xs = first:selectSort op (deleteElem (first) xs) -- T(n) = n * n * n = O(n³)
            where
                first = calculateFirst op xs -- O(n)
{-
T(n) = 1 + n³ = O(n³), 
da sowohl calculateFirst, als auch delete Elem
und selectSort an sich zum berechnen jeweils lineare Wachstumsgeschwindigkeit,
also n rekursive Aufrufe haben.
Ich weiß, dass das unglaublich ineffizient ist, aber wir sollen ja
NOCH keine effizienten Algorithmen bauen
-}

{-
Simple Thing:
List has one Element, it's both the smallest and the biggest
List has more than one, we just check every element with its 
neighbour and sort out the elements that are smaller/bigger
(depends on operator) until we have one item left and
the recursion anchor hits
-}
calculateFirst :: Ord a => (a -> a -> Bool) -> [a] -> a
calculateFirst _ [x] = x -- O(1)
calculateFirst op (x:y:xs) = calculateFirst op ((if (op x y) then x else y):xs) -- O(n)
{-
T(n) = 1 + n = O(n), 
da unsere Rekursion n mal aufgerufen wird und 
_ [x] = x konstante Zeit braucht
-}

{-
Cycle through the list, until we hit the first
occurance and give back list without the Element
-}
deleteElem :: Eq a => a -> [a] -> [a]
deleteElem _ [] = [] -- O(1)
deleteElem x (y:ys) 
    | x == y = ys -- O(1)
    | otherwise = y : deleteElem x ys -- O(n)
{-
T(n) = 1 + 1 + n = O(n), d
a unsere Rekursion n mal aufgerufen wir und
_ [] = [] und x == y = ys konstante Zeit braucht
-}

--Aufgabe 2
isSorted :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
isSorted cmp xs = and(zipWith cmp xs (tail xs))
{-
 tail xs müsste mit O(1) laufen, da es immer nur das erste Element entfernt
 zipWith cmp xs (tail xs) müsste mit O(n) laufen, da es nur n Elemente miteinander
 verbindet
 and(zipWith cmp xs (tail xs)) müsste mit O(n²) laufen, da zipWith n mal aufgerufen
 wird und and selbst auch n mal durch die Liste iteriert.
 
 Also: T(n) = 1 * n * n = O(n²)
-}

--Aufgabe 3
mult :: Integer -> Integer -> Integer
mult n 0 = 0 -- O(1)
mult n m = mult n (m-1) + n -- O(n)
{-
 T(n) = 1 + n = O(n), 
 da mult n mal rekursiv aufgerufen wird und sowohl (m-1) als auch + n
 konstante Zeit brauchen 
-}

russMult :: Integer -> Integer -> Integer
russMult n 0 = 0 -- O(1)
russMult n m | (mod m 2) == 0 = russMult (n+n) (div m 2) -- O(n)
             | otherwise = russMult (n+n) (div m 2) + n -- O(n)
{-
T(n) = 1 + n + n = 1 + 2*n = O(n)
-}

-- Aufgabe 4
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs | isSorted (<=) xs = xs -- O(n²)
              | otherwise = bubbleSort (moveBubble xs) -- n * n = O(n²)
              where
                moveBubble [] = [] -- O(1)
                moveBubble [x] = [x] -- O(1)
                moveBubble (x:y:rest) | (<=) x y = x:moveBubble (y:rest) -- O(n)
                                      | otherwise = y:moveBubble (x:rest)-- O(n)

{-
 n² + n² + 1 + 1 + n + n = O(n²)
-}

{-
As you can see, I just added 8 little 
chars. The "[]" around the isSorted Output 
and the "++ [xs]" at the recursion call, because that already saves every list
Note: I could also use xs:traceBubbleSort, but then the list is in the wrong order
-}
traceBubbleSort :: Ord a => [a] -> [[a]]
traceBubbleSort xs | isSorted (<=) xs = [xs]
                   | otherwise = traceBubbleSort(moveBubble xs) ++ [xs]
                   where
                    moveBubble [] = []
                    moveBubble [x] = [x]
                    moveBubble (x:y:xs) | (<=) x y = x:moveBubble (y:xs)
                                        | otherwise = y:moveBubble (x:xs)


--Aufgabe 5
{-
Just give back the whole list, but every time with
one Element less
-}
allSuffixes :: Ord a => [a] -> [[a]]
allSuffixes [] = [] -- O(1)
allSuffixes (x:xs) = [x:xs] ++ allSuffixes xs --O(n)
--T(n) = 1 + n = O(n)

{-
Check every element, until they are different and
then give back the list
-}
prefix :: Ord a => [a] -> [a] -> [a]
prefix [] [] = [] -- O(1)
prefix _ [] = [] -- O(1)
prefix [] _ = [] -- O(1)
prefix (x:xs) (y:ys) 
    | (x == y) = [x] ++ prefix xs ys -- O(n)
    | otherwise = [] -- O(1)
-- T(n) = 1 + 1+ 1 + n + 1 = O(n)

{-
Check every element with the next Item and save the
current longest in a variable until the list is empty, 
then return the longest Element with its length
-}
largestPrefix :: Ord a => [[a]] -> (Int, [a])
largestPrefix [[]] = (0,[]) -- O(1)
largestPrefix (x:y:xs) = lPHelper x y xs [] 0 -- O(2^n)
{-
T(n) = 1 + 2^n = O(2^n)
-}

lPHelper :: Ord a => [a] -> [a] -> [[a]] -> [a] -> Int -> (Int, [a])
lPHelper x y [] max len 
        | (length(prefix x y) > len) = ((length(prefix x y), (prefix x y))) -- 1 * n * 1 * n * n = O(n³)
        | otherwise = (len, max) --O(1)
lPHelper x y (z:xs) max len
        | (length (prefix x y) > len) = lPHelper y z xs (prefix x y) (length(prefix x y)) -- 1*n * n * n * 1 * n = O(n⁴)
        | otherwise = lPHelper y z xs max len -- O(n)
{-
T(n) = n³ + n⁴ + n = O(2^n), either I miscalculated extremely or I write actually inefficient
-}

{-
First get allSuffixes of the list, 
then sort the output, so we have words next
to eachother with the same starting letters
then just check every word with their neighbour
for the longest prefix and finally give back the
second index of the tuple, because 
Miss Esponda wants largestPrefix to give back a
tuple for reasons that are way beyond my humble mind
-}
maxLengthRepSeq :: Ord a => [a] -> [a]
maxLengthRepSeq [] = [] -- O(1)
maxLengthRepSeq xs = snd(largestPrefix (bubbleSort(allSuffixes xs))) -- 1 * 2^n * n² * n = O(2^n)
{-
T(n) = 1 + 2^n = O(2^n)
-}