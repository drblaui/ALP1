-- Aufgabe 1
selectSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
selectSort op [x] = [x] -- O(1)
selectSort op xs = calculateFirst op xs:selectSort op (deleteElem (calculateFirst op xs) xs) -- T(n) = n * n * n = n³
{-
T(n) = O(n³), da sowohl calculateFirst, als auch delete Elem
und selectSort an sich zum berechnen jeweils lineare Wachstumsgeschwindigkeit,
also n rekursive Aufrufe haben.
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
    | x == y = deleteElem x ys
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
russMult n m | (mod m 2) == 0 = russMult (n+n) (div m 2)
             | otherwise = russMult (n+n) (div m 2) + n -- O(n)?