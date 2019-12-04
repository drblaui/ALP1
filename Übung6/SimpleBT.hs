{-# LANGUAGE NPlusKPatterns #-}

module SimpleBT where

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

{- some simple trees for testing -}
tree0 = genSimpleBT 2
tree1 = genSimpleBT 4
tree2 = joinTrees (genSimpleBT 3) (genSimpleBT 4)
tree3 = joinTrees (genSimpleBT 4) (joinTrees (genSimpleBT 1) (genSimpleBT 2))

test_nodes = nodes (genSimpleBT  4)
test_heigth = height  (N (N (N L L) L) (N L L))

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

{- print some of the tree examples -}
paint_t0 = printSimpleBT tree0
paint_t1 = printSimpleBT tree1
paint_t2 = printSimpleBT tree2
paint_t3 = printSimpleBT tree3

