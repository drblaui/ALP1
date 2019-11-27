-- Aufgabe 1
selectSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
selectSort op [x] = [x] -- O(1)
selectSort op xs = calculateFirst op xs:selectSort op (deleteElem (calculateFirst op xs) xs) -- T(n) = n * n * n = O(n³)
{-
T(n) = O(n³), da sowohl calculateFirst, als auch delete Elem
und selectSort an sich zum berechnen jeweils lineare Wachstumsgeschwindigkeit,
also n rekursive Aufrufe haben.
Ich weiß, dass das unglaublich ineffizient ist, aber wir sollen ja
NOCH keine effizienten Algorithmen bauen
-}

calculateFirst :: Ord a => (a -> a -> Bool) -> [a] -> a
calculateFirst _ [x] = x -- O(1)
calculateFirst op (x:y:xs) = calculateFirst op ((if (op x y) then x else y):xs) -- O(n)
{-
T(n) = O(n), da unsere Rekursion n mal aufgerufen wird und 
_ [x] = x konstante Zeit braucht
-}

deleteElem :: Eq a => a -> [a] -> [a]
deleteElem _ [] = [] -- O(1)
deleteElem x (y:ys) 
    | x == y = ys
    | otherwise = y : deleteElem x ys -- O(n)
{-
T(n) = O(n), da unsere Rekursion n mal aufgerufen wir und
_ [] = [] konstante Zeit braucht
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
 
 Also: T(n) = O(n²)
-}

--Aufgabe 3
mult :: Integer -> Integer -> Integer
mult n 0 = 0 -- O(1)
mult n m = mult n (m-1) + n -- O(n)
{-
 T(n) = O(n), da mult n mal rekursiv aufgerufen wird und sowohl (m-1) als auch + n
 konstante Zeit brauchen 
-}

russMult :: Integer -> Integer -> Integer
russMult n 0 = 0 -- O(1)
russMult n m | (mod m 2) == 0 = russMult (n+n) (div m 2) -- O(n)
             | otherwise = russMult (n+n) (div m 2) + n -- O(n)


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
 n² + n² + 1 + 1 + n + n = O(n²) für Listen der Länge n im Worst Case
-}

traceBubbleSort :: Ord a => [a] -> [[a]]
traceBubbleSort xs | isSorted (<=) xs = [xs]
                   | otherwise = traceBubbleSort(moveBubble xs)
                   where
                    moveBubble [] = []
                    moveBubble [x] = [x]
                    moveBubble (x:y:xs) | (<=) x y = x:moveBubble (y:xs)
                                        | otherwise = y:moveBubble (x:xs)

--Aufgabe 5
allSuffixes :: Ord a => [a] -> [[a]]
allSuffixes [] = [] -- O(1)
allSuffixes (x:xs) = [x:xs] ++ allSuffixes xs --O(n)
--O(n)

prefix :: Ord a => [a] -> [a] -> [a]
prefix [] [] = [] -- O(1)
prefix _ [] = [] -- O(1)
prefix [] _ = [] -- O(1)
prefix (x:xs) (y:ys) 
    | (x == y) = [x] ++ prefix xs ys -- O(n)
    | otherwise = [] -- O(1)
-- O(n)

largestPrefix :: Ord a => [[a]] -> (Int, [a])
largestPrefix [[]] = (0,[]) -- O(1)
largestPrefix (x:y:xs) = lPHelper x y xs [] 0 -- ?????

lPHelper :: Ord a => [a] -> [a] -> [[a]] -> [a] -> Int -> (Int, [a])
lPHelper x y [] max len 
        | (length(prefix x y) > len) = ((length(prefix x y), (prefix x y))) -- 1 * n * 1 * n * n = O(n³) ?????
        | otherwise = (len, max) --O(1)
lPHelper x y (z:a:xs) max len
        | (length (prefix x y) > len) = lPHelper z a xs (prefix x y) (length(prefix x y))
        | otherwise = lPHelper z a xs max len

-- Length sollte O(1) brauchen, wegen dem Faltungsoperator??