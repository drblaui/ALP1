-- Aufgabe 1
selectSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
selectSort op [x] = [x]
selectSort op xs = calculateFirst op xs:selectSort op (deleteElem (calculateFirst op xs) xs)

calculateFirst :: Ord a => (a -> a -> Bool) -> [a] -> a
calculateFirst _ [x] = x
calculateFirst op (x:y:xs) = calculateFirst op ((if (op x y) then x else y):xs)

deleteElem :: Eq a => a -> [a] -> [a]
deleteElem _ [] = []
deleteElem x (y:ys) 
    | x == y = deleteElem x ys
    | otherwise = y : deleteElem x ys
