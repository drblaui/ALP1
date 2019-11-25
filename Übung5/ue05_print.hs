-- Aufgabe 1
selectSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
selectSort op [x] = [x] -- O(1)
selectSort op xs = calculateFirst op xs:selectSort op (deleteElem (calculateFirst op xs) xs) -- T(n) = n * n * n = n³
{-
T(n³) = O(n³), da sowohl calculateFirst, als auch delete Elem
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