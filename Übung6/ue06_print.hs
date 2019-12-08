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
data Weekday =  Sunday | Monday | Tuesday | Wednesday 
            | Thursday | Friday | Saturday
            deriving (Eq, Ord, Show, Enum)

type Date = (Int, Int, Int)

leap_year :: Int -> Bool
leap_year y = ((mod y 4) == 0) && (((mod y 100) /= 0) || (mod y 400) == 0)

weekDay :: Date -> Weekday
weekDay (day,month,year)
    | checkArgs day month year = [Sunday .. Saturday] !! (mod (day + x + (31 * m0) `div` 12) 7)
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

-- 3. Aufgabe
{-- simple trees (algebraic data type explained in the lecture) --}
{-- Author: M. Esponda --}

data SimpleBT = L | N SimpleBT SimpleBT  deriving Show

type Height = Integer

genSimpleBT :: Height -> SimpleBT
genSimpleBT   0  =  L
genSimpleBT (n+1) = N (genSimpleBT n) (genSimpleBT n)

nodes :: SimpleBT -> Integer
nodes L = 1
nodes (N leftT rightT) = 1 + nodes leftT + nodes rightT

height :: SimpleBT -> Integer
height L = 0
height (N lt rt) = (max (height lt) (height rt)) + 1

joinTrees :: SimpleBT -> SimpleBT -> SimpleBT
joinTrees leftTree rightTree = N leftTree rightTree

balanced :: SimpleBT -> Bool
balanced  L = True
balanced  (N lt rt) = (balanced lt) && (balanced rt) && height lt == height rt

{- The following functions visualize the simple binary trees (SimpleBT)
   you are not supposed to understand the semantics of the functions.
   There are many details, which are not relevant for the lecture understanding.
-}
   
{- node = "N" + "|" (below it)
   horizontal line = "---" between the subtrees
-}
paintTree :: SimpleBT -> ([[Char]], Int)
paintTree L = ([" L  "], 1)
paintTree (N lTree rTree) = ([nodeLine, nodeHLine, horLine] ++ subTrees, newNodePos)
            where                   
                (lNodePicture, leftNodePos)  = paintTree lTree
                (rNodePicture, rigthNodePos) = paintTree rTree
                     
                ltNewPicture = moveTreePos lNodePicture rNodePicture
                rtNewPicture = moveTreePos rNodePicture lNodePicture
                
                {- write spaces in between if necessary -}
                moveTreePos :: [String] -> [String] -> [String]
                moveTreePos str1 str2 | length str1 >= length str2 = str1
                                      | otherwise = str1 ++ (take rowsToFill (repeat spaces))
                                           where
                                              spaces = gen (length (head str1))  " "
                                              rowsToFill = (length str2) - (length str1)
                     
                leftWidth = length (head lNodePicture)
                rightWidth = length (head rNodePicture)
                width = leftWidth + rightWidth            

                hLineLength = (leftWidth - leftNodePos) + rigthNodePos
                newNodePos = leftNodePos + (div hLineLength 2)

                horLine  = (gen leftNodePos " ") ++ "*" ++ gen (hLineLength - 1) "-" ++ "*"
                                                 ++ gen (width - hLineLength - leftNodePos - 1) " "
                nodeLine  = (gen newNodePos " ") ++ "N" ++ gen (width - newNodePos - 1) " "
                nodeHLine = (gen newNodePos " ") ++ "|" ++ gen (width - newNodePos - 1) " " 
               
                                      
                     
                subTrees = zipWith (++) ltNewPicture rtNewPicture
                                              
{- concatenates n times the String str -}
gen :: Int -> [a] -> [a]
gen n str = take n (foldr (++) [] (repeat str))

{- insert the necesary new lines to show the rows of the list picture on the screen -}
printCharList list = putStr (foldr (++) [] (map (++"\n") list))

{- print a simple binary tree -}
printSimpleBT tree = printCharList (fst (paintTree tree))

{- Ich muss zugeben, dass ich diesen Lösungsansatz von Leander Tolksdorf habe,
aber meine Idee an sich dieselbe war-}
insertLeaves :: Integer -> SimpleBT -> SimpleBT
insertLeaves 0 tree = tree
insertLeaves 1 L = N L L
insertLeaves n L = N (insertLeaves (next n 'H') L) (insertLeaves (next n 'L') L)
insertLeaves n (N ltree rtree)
            | n <= leaves ltree = N (insertLeaves n ltree) rtree
            | n > leaves rtree = N (insertLeaves (next n 'H') ltree) (insertLeaves (next n 'L') rtree)


next :: Integer -> Char -> Integer
next n dir = if (n `mod` 2 /= 0) 
             then case dir of
                    'H' -> ((n+1) `div` 2)
                    'L' -> ((n-1) `div` 2)
             else (n `div` 2)

leaves :: SimpleBT -> Integer
leaves L = 1
leaves (N ltree rtree) = leaves ltree + leaves rtree

{- Works if you want to delete 1 Leaf, why I dont know
but people that are not insane don't want to delete more than one leaf
in a fucking Binary tree, because that is fucking stupid-}
deleteLeaves :: Integer -> SimpleBT -> SimpleBT
deleteLeaves 0 tree = tree
deleteLeaves 1 (N L L) = L
deleteLeaves n L = N (deleteLeaves (next n 'H') L) (deleteLeaves (next n 'L') L) 
deleteLeaves n (N ltree rtree)
                | n <= leaves ltree = N ltree (deleteLeaves n rtree)
                | n > leaves rtree = N (deleteLeaves (next n 'H') ltree) (deleteLeaves (next n 'L') rtree)

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

inOrder :: (Ord a) => BSearchTree a -> [a]
inOrder Nil = []
inOrder (Node x ltree rtree) = inOrder ltree ++ x : inOrder rtree
{-
These Solutions were all created with the correctness of list2Tree in mind
so it works if we create a Tree from a list
-}
postOrder :: (Ord a) => BSearchTree a -> [a]       
postOrder Nil = []  
postOrder (Node x ltree rtree) = postOrder ltree ++ postOrder rtree ++ [x]

oneChild :: (Ord a) => BSearchTree a -> Bool
oneChild (Node x Nil Nil) = False 
oneChild (Node x ltree rtree) 
            | (((ltree == Nil) && not(rtree == Nil))|| (not(ltree == Nil) && (rtree == Nil))) = True
            | otherwise = ((oneChild ltree) || (oneChild rtree))

complete :: (Ord a) => BSearchTree a -> Bool
complete Nil = True
complete (Node _ Nil Nil) = True
complete (Node _ (Node _ Nil Nil) Nil) = False
complete (Node _ Nil (Node _ Nil Nil)) = False
complete (Node _ ltree rtree) = ((complete ltree) && (complete rtree))

--successor :: (Ord a) => a -> BSearchTree a -> Maybe a
--successor x tree =foldl(\y z -> (y==x) = z) (inOrder tree)
--Look through list and get the next best element


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