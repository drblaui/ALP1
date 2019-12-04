{-# LANGUAGE NPlusKPatterns #-}
import SimpleBT
-- 1. Aufgabe
data Length = Foot Double | Centimeter Double 
            | Yard Double | Kilometer Double 
            | Inch Double | Meter Double
            | Mile Double
            deriving Show

foot2cm :: Length -> Length
foot2cm (Foot ft) = Centimeter (ft * 30.48)

inch2cm :: Length -> Length
inch2cm (Inch ic) = Centimeter (ic * 2.54)

yard2m :: Length -> Length
yard2m (Yard yd) = Meter (yd / 1.0936)

mile2km :: Length -> Length
mile2km (Mile mi) = Kilometer (mi / 0.62137)


-- 2. Aufgabe
data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
            deriving (Eq, Ord, Show, Enum)
--Aufgabe sagt literally NICHTS AUS FRAU ESPONDA ICH HASSE MEIN LEBEN
leap_year :: Int -> Bool
leap_year y = ((mod y 4) == 0) && (((mod y 100) /= 0) || (mod y 400) == 0)

weekDay :: Int -> Int -> Int -> String
weekDay day month year
    | checkArgs day month year = dayNames !! (mod (day + x + (31 * m0) `div` 12) 7)
    | otherwise = error "One of the arguments is not a valid date value"
        where
            y0 = year - ((14 - month) `div` 12)
            x = y0 + y0 `div` 4 - y0 `div` 100 + y0 `div` 400
            m0 = month + 12 * ((14 - month) `div` 12) - 2
            checkArgs d m y
                | elem m [1,3,5,7,8,10,12] = d < 32 && d > 0 && y > 0
                | elem m [4,6,9,11] = d < 31 && d > 0 && y > 0
                | (m == 2) = (((leap_year y) && d < 30) || d < 29) && d > 0 && y > 0
                | otherwise = False
dayNames = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"] 

-- 3. Aufgabe

insertLeaves :: Integer -> SimpleBT -> SimpleBT
insertLeaves 0 tree = tree
insertLeaves n (N L rTree) = N (N L L) (insertLeaves (n-1) rTree)
insertLeaves n L = N L L 
insertLeaves n (N lTree rTree) = N (insertLeaves n lTree) (insertLeaves n rTree)

-- tree = genSimpleBT 3 -> N (N (N L L) (N L L)) (N (N L L) (N L L))
-- insertLeaves 2 tree -> N (N (N (N L L) (N L L)) (N L L)) (N (N L L) (N L L))
-- 

-- 4. Aufgabe
data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a)
                    deriving (Show, Eq)

insert :: (Ord a) => a -> BSearchTree a -> BSearchTree a
insert k Nil = Node k Nil Nil
insert k (Node x ltree rtree)
                | k < x = Node x (insert k ltree) rtree
                | otherwise = Node x ltree (insert k rtree)
    
list2Tree :: (Ord a) => [a] -> BSearchTree a
list2Tree [] = Nil
list2Tree (x:xs) = insert x (list2Tree xs)

postOrder :: (Ord a) => BSearchTree a -> [a]       
postOrder Nil = []  
postOrder (Node x ltree rtree) = postOrder ltree ++ postOrder rtree ++ [x]

-- 5. Aufgabe
--a
data Queue a = Empty | Queue [a] deriving (Eq, Show)

enqueue :: a -> Queue a -> Queue a
enqueue x Empty = Queue ([x])
enqueue x (Queue xs) = Queue (xs ++ [x])

dequeue :: (Eq a) => Queue a -> (a, Queue a)
dequeue (Queue xs) 
            | isEmpty(Queue xs) = error "Cant remove Element of Empty List"
            | otherwise = (head xs, Queue $ tail xs)

isEmpty :: (Eq a) => Queue a -> Bool
isEmpty Empty = False
isEmpty (Queue xs) = True

makeQueue :: Queue a
makeQueue = Empty

showQueue :: (Show a) => Queue a -> String
showQueue (Queue [x]) = show x
showQueue (Queue (x:xs)) = show x ++ ", " ++ showQueue (Queue xs)

{-
If I kill myself, see this as my suicide note
-}