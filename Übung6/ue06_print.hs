{-# LANGUAGE NPlusKPatterns #-}
-- 1. Aufgabe
data Length = Foot Double | Centimeter Double 
            | Yard Double | Kilometer Double 
            | Inch Double | Meter Double
            | Mile Double
            deriving Show

-- All these conversions come from the the DuckDuckGo converter
-- This one -> https://duckduckgo.com/?q=cm+1+inch&t=ffnt&atb=v168-1&ia=answer
-- 1 Foot == 30.48 Meter 
foot2cm :: Length -> Length
foot2cm (Foot ft) = Centimeter (ft * 30.48)

-- 1 Inch == 2.54 Cemtimeter
inch2cm :: Length -> Length
inch2cm (Inch ic) = Centimeter (ic * 2.54)

-- 1 Yard == 0.9144 Meter
yard2m :: Length -> Length
yard2m (Yard yd) = Meter (yd * 0.9144)

-- 1 Mile == 1.609344 Kilometer
mile2km :: Length -> Length
mile2km (Mile mi) = Kilometer (mi * 1.609344)


-- 2. Aufgabe
-- Got this somewhat out of the lecture
data Weekday =  Sunday | Monday | Tuesday | Wednesday 
            | Thursday | Friday | Saturday
            deriving (Eq, Ord, Show, Enum)

-- So the date looks cleaner
type Date = (Int, Int, Int)

leap_year :: Int -> Bool
leap_year y = ((mod y 4) == 0) && (((mod y 100) /= 0) || (mod y 400) == 0)

weekDay :: Date -> Weekday
weekDay (day,month,year)
    -- Basically I replaced the array with another one
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

{- 
I lost so much precious life time on this thing and I ofically quit trying
This is impossible and nobody would EVER need to remove more than one leaf out of a binary tree
And this function also only works if you want to remove one leaf, so thats that
-}
deleteLeaves :: Integer -> SimpleBT -> SimpleBT
deleteLeaves 0 tree = tree
deleteLeaves 1 (N L L) = L
deleteLeaves 1 L = L
deleteLeaves n (N _ (N L L)) = N (deleteLeaves (next n 'H') L)  L
deleteLeaves n (N (N L L) _) = N L (deleteLeaves (next n 'L') L)
deleteLeaves n (N ltree rtree)
                | n <= leaves ltree = N ltree (deleteLeaves n rtree)
                | n > leaves rtree = N (deleteLeaves (next n 'H') ltree) (deleteLeaves (next n 'L') rtree)


-- 4. Aufgabe
-- Out of lecture
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
{-
Post Order is as defined in the Lecture the traversion of the binary tree from
the left tree to the right tree to the root and well that is pretty easy
-}
postOrder :: (Ord a) => BSearchTree a -> [a]       
postOrder Nil = []  
postOrder (Node x ltree rtree) = postOrder ltree ++ postOrder rtree ++ [x]

{-
If we have (Node x Nil Nil) we know that the tree at that point does not have a node with just one child
But if one Tree has Nil on one side and Not Nil on the other side, we know we have an onlyChild and we can stop looking
-}
oneChild :: (Ord a) => BSearchTree a -> Bool
oneChild (Node x Nil Nil) = False 
oneChild (Node x ltree rtree) 
            | (((ltree == Nil) && not(rtree == Nil)) || (not(ltree == Nil) && (rtree == Nil))) = True
            | otherwise = ((oneChild ltree) || (oneChild rtree))

{-
A Binary Search tree is Complete if its balanced so we recusivly look if every node has two or no children
-}
complete :: (Ord a) => BSearchTree a -> Bool
complete Nil = True
complete (Node _ Nil Nil) = True
complete (Node _ (Node _ Nil Nil) Nil) = False
complete (Node _ Nil (Node _ Nil Nil)) = False
complete (Node _ ltree rtree) = ((complete ltree) && (complete rtree))

{-
Since I googled successor, because I forgot we actually have
the lecture slides, I know that a successor is just the next item
in an indorder list

So we convert our tree to a inorder List with the function from the lecture
and then just look through the list until we find the item and look if anything comes after it
-}
successor :: (Ord a) => a -> BSearchTree a -> Maybe a
successor k tree = successorHelper k (inOrder tree)

successorHelper :: (Ord a) => a -> [a] -> Maybe a
successorHelper k [] = Nothing
successorHelper k (x:[])
        | (k == x) = Nothing
        | otherwise = Nothing
successorHelper k (x:y:xs)
        | (k == x) = Just y
        | otherwise = successorHelper k (y:xs)

-- 5. Aufgabe
--a
data Queue a = Queue [a] [a]

{-
Since we have to keep two lists and have
to reverse the second one and use it as the first, if
the first list is empty, we just make a function that checks
that and apply it to literally every step we take on the list
to be sure that our datatype stays true to itself
In ALP2 we learned a name for that property but I forgot
-}
fixQueue :: Queue a -> Queue a
fixQueue (Queue [] ys) = Queue (reverse ys) []
fixQueue (Queue xs ys) = Queue xs ys

-- I had problems with dequeue, so I just worked around the problem
rest :: Queue a -> Queue a
rest (Queue (x:xs) ys) = Queue xs ys

-- We just add an element at the end of the second list and check if our type is still right
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue ys xs) = fixQueue(Queue ys (xs ++ [x]))

-- We remove the firt elememt of the first list, after we checked if the list property is right
-- we also check the list property afterwards
-- Also can't remove anything from empty lists, duh
dequeue :: (Eq a) => Queue a -> Queue a
dequeue (Queue xs ys)
        | isEmpty (Queue xs ys) = error "Cant remove Element from Empty List"
        | otherwise = fixQueue(rest (fixQueue(Queue xs ys)))

--easy pattern matching
isEmpty :: (Eq a) => Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty (Queue xs ys) = False

-- An empty Queue are just two empty lists
makeQueue :: Queue a
makeQueue = Queue [] []

--b
--these two lines took about 2 hours of my life away
instance Show a => Show (Queue a) where
    show (Queue a b) = showQueue (Queue a b)

-- I chose to display lists with a "," between the elements
showQueue :: (Show a) => Queue a -> String
showQueue (Queue [] []) = " "
showQueue (Queue (x:xs) []) = show x
showQueue (Queue [] (y:ys)) = show y
showQueue (Queue (x:xs) (y:ys)) = show x ++ "," ++ showQueue (Queue xs []) ++ "," ++ showQueue(Queue [] ys) ++ "," ++ show y 

--c
instance Eq a => Eq (Queue a) where
    x == y = isEqual x y

{-
Since Esponda didn't say, what Queue == Queue means for her,
we just say the Eq returns true, if the queues have the
same items in the same order
-}
isEqual :: (Eq a) => Queue a -> Queue a -> Bool
isEqual (Queue xs ys) (Queue zs as) = ((xs == zs) && (ys == as)) 

-- I guess Vergleichs-Infix-Operaoren are <,>,<= and >= so yeah
instance Ord a => Ord (Queue a) where
    x < y = isSmaller x y
    --x > y is just the reverse result of isSmaller
    x > y = not(isSmaller x y)
    x <= y = isSmallerEqual x y
    --x >= y is just the reverse result of isSmallerEqual
    x >= y = not(isSmallerEqual x y)

{- 
Thankfully in haskell you can directly compare lists, so we just use that
Since i have no idea how haskell compares lists, i make them into the queue in the right order
-}
isSmaller :: (Ord a) =>  Queue a -> Queue a -> Bool
isSmaller (Queue xs ys) (Queue zs as) = ((xs ++ (reverse ys)) < (zs ++ reverse (as)))

{-
Same as isSmaller
-}
isSmallerEqual :: (Ord a) => Queue a -> Queue a -> Bool
isSmallerEqual (Queue xs ys) (Queue zs as) = ((xs ++ (reverse ys)) <= (zs ++ (reverse as)))


-- Aufgabe 6
{-
Of course I don't know much about Multitrees, so I got 
some suggestions from https://wiki.haskell.org/99_questions/70B_to_73

 I cant believe that I even try this
 A "Multitree" datatype is pretty similar to our Binary Tree, except it has
 a list of children and not just two, the children themselves can just be a tree again
 Also we don't use a "Nil", because of how we build a tree
 If our tree is: 
 NodeA 'a' [NodeA 'b' []] our Nil would automatically just be the []
 It just is easier than typing:
 NodeA 'a' [NodeA 'b' Nil]
-}
data ABaum a = NodeA a [ABaum a] deriving (Eq, Show)

--Since the functions "nodes" and "height" are already defined I name them different

{-
This is the same nodes function as for SimpleBT, but 
we just map the function to every item in the array and then just
sum them. (I am allowed to use map and sum, since there are no restrictions on the assignment)
-}
nodesA :: ABaum a -> Integer
nodesA (NodeA _ stump) = 1 + sum(map nodesA stump)

{-
We say the root has the height 0.
Then we just map our array out to get all the stump lengths 
and take the largest possible number
-}
heightA :: ABaum a -> Integer
heightA (NodeA _ []) = 0
heightA (NodeA _ stump) = maximum (map heightA stump) + 1

{-
I have no idea how or why this works, this came to live at about
2 in the morning and after 2 whole hours of just reading error messages
I'm actually afraid to touch anything here, but I guess the [(mapTree func y)]
just cycles through every possible child there is recusivly.
Believe me I did not get this from the Internet. I've searched very long only to 
not find it
-}
mapTree :: (a -> b) -> ABaum a -> ABaum b
mapTree func (NodeA x []) = NodeA (func x) []
mapTree func (NodeA x (y:stump)) = NodeA (func x) [(mapTree func y)]


{-
If I kill myself, see this as my suicide note
-}