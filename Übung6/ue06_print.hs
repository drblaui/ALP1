{-# LANGUAGE NPlusKPatterns #-}
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
data SimpleBT = L | N SimpleBT SimpleBT deriving (Eq, Show)

type Height = Integer

genSimpleBT :: Height -> SimpleBT
genSimpleBT 0 = L
genSimpleBT (n+1) = N (genSimpleBT n) (genSimpleBT n)

-- tree = genSimpleBT 3 -> N (N (N L L) (N L L)) (N (N L L) (N L L))
-- insertLeaves 2 tree -> N (N (N (N L L) (N L L)) (N L L)) (N (N L L) (N L L))

-- 4. Aufgabe
data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a)
                    deriving (Show, Eq)

postOrder :: (Ord a) => BSearchTree a -> [a]         
postOrder (Node x xl xr) = postOrder xl ++ postOrder xr ++ [x]

-- 5. Aufgabe
--a
data Queue a = Queue [a] deriving (Eq, Show)

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])

dequeue :: (Eq a) => Queue a -> (a, Queue a)
dequeue (Queue xs) 
            | isEmpty(Queue xs) = error "Cant remove Element of Empty List"
            | otherwise = (head xs, Queue $ tail xs)

isEmpty :: (Eq a) => Queue a -> Bool
isEmpty (Queue xs) 
            | (xs == []) = True
            | otherwise = False

makeQueue :: Queue a
makeQueue = Queue []