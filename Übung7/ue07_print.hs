{-# LANGUAGE NPlusKPatterns #-}
{-
TODO: 
Kill myself
-}
-- 1. Aufgabe
data B = T | F deriving Show
data Nat = Zero | S Nat -- deriving Show

-- Funktionen von Frau Esponda
instance Show Nat where
    show = showNats
 
showNats n = show (nat2Int n)

nat2Int :: Nat -> Int
nat2Int  Zero = 0
nat2Int (S a) =  1 + (nat2Int a)

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat (n+1) = (S (int2Nat n))

addN ::  Nat -> Nat -> Nat
addN a Zero  = a
addN a (S b) = S (addN a b)

subN :: Nat -> Nat -> Nat
subN m Zero = m
subN m (S n) = predN (subN m n) ---- (n-m) gleich 0, wenn m>n

predN :: Nat -> Nat
predN Zero = Zero
predN (S n) = n

ebenN :: Nat -> B
ebenN Zero = T
ebenN (S (S a)) = ebenN a
ebenN   _  = F

(<<) :: Nat -> Nat -> B
(<<) Zero (S _)  = T
(<<) (S a) (S b) = (<<) a b
(<<)   _    _    = F

multN :: Nat -> Nat -> Nat
multN _ Zero  = Zero
multN a (S b) = addN a (multN a b)

foldn :: (Nat -> Nat) -> Nat -> Nat -> Nat
foldn h c Zero  = c
foldn h c (S n) = h (foldn h c n)

orB :: B -> B -> B
orB F F = F
orB _ _ = T

iff :: B -> a -> a -> a
iff T a _ = a
iff F _ b = b

maxN :: Nat -> Nat -> Nat
maxN a b = iff ((<<) a b) b a

powN' :: Nat -> Nat -> Nat
powN' m = foldn (multN m) (S Zero)

data ZInt = Z Nat Nat --deriving Show

addZ :: ZInt -> ZInt -> ZInt
addZ (Z a b) (Z c d) = Z (addN a c) (addN b d)

subZ :: ZInt -> ZInt -> ZInt
subZ (Z a b) (Z c d) = Z (addN a d) (addN b c)

simplifyZ :: ZInt -> ZInt
simplifyZ (Z Zero b) = Z Zero b
simplifyZ (Z a Zero) = Z a Zero
simplifyZ (Z (S a) (S b)) = simplifyZ (Z a b)

--Funktionen, die ich noch brauchte
-- I'm not even trying to explain how this works, because I'm happy it does
divN :: Nat -> Nat -> Nat
divN a b = iff((<<) a b) (Zero) ( S (divN (subN a b) b))

--mod a b is just a - (b * (a/b))
modN :: Nat -> Nat -> Nat
modN a b = subN a (multN b (divN a b))

-- a
eqB :: B -> B -> B
eqB F T = F
eqB T F = F
eqB _ _ = T

notB :: B -> B
notB T = F
notB F = T

xorB :: B -> B -> B
xorB T F = T
xorB F T = T
xorB _ _ = F

eqN :: Nat -> Nat -> B
eqN Zero Zero = T
eqN Zero _ = F
eqN _ Zero = F
eqN _ _ = T

oddN :: Nat -> B
oddN (Zero) = F
oddN (S (Zero)) = T
oddN (S (S n)) = oddN n

fibonacci :: Nat -> Nat
fibonacci Zero = addN Zero Zero
fibonacci (S (Zero)) = addN Zero (S(Zero))
fibonacci n = addN (fibonacci(subN n (S(Zero)))) (fibonacci(subN n (S(S(Zero)))))

isTeilerN :: Nat -> Nat -> B
isTeilerN a b = iff ((eqN) (modN a b) Zero) T F

ggtN :: Nat -> Nat -> Nat
ggtN a Zero = a
ggtN a b = ggtN b (modN a b)

--b
{-
Since n - m are just m predecessors of n, 
we fold down m times with the predN function
the foldn function gets called m times, gives out
our n and then (because of recursion) applies
predN m times onto n
-}
subN' :: Nat -> Nat -> Nat
subN' = foldn predN

--c
-- Anscheinend ist Z a b = b - a für die Zahl
eqZ :: ZInt -> ZInt -> B
eqZ (Z a b) (Z c d) = eqB (eqN a c) (eqN b d)

(<<<) :: ZInt -> ZInt -> B
(<<<) (Z a b) (Z c d) = orB ((<<) a c) ((<<) b d)

-- Daneben schreiben, was es tut
negZ :: ZInt -> ZInt
negZ (Z a Zero) = Z a Zero
negZ (Z Zero a) = Z a Zero
negZ a = negZ(simplifyZ a)

-- maxN but with the smaller operator for ZInt
maxZ :: ZInt -> ZInt -> ZInt
maxZ a b = iff ((<<<) a b) b a

multZ :: ZInt -> ZInt -> ZInt
multZ (Z a b) (Z c d) = Z (multN a c) (multN b d)

absZ :: ZInt -> ZInt
absZ (Z a Zero) = Z Zero a
absZ (Z Zero a) = Z Zero a
absZ a = absZ(simplifyZ a) 

powZ :: ZInt -> Nat -> ZInt
powZ (Z a b) n = Z (powN' a n) (powN' b n)

isTeilerZ :: ZInt -> ZInt -> B
isTeilerZ (Z a b) (Z c d) = eqB (isTeilerN a c) (isTeilerN b d)

ggtZ :: ZInt -> ZInt -> ZInt
ggtZ (Z a b) (Z c d) = Z (ggtN a c) (ggtN b d)

--d
zint2Int :: ZInt -> Int
zint2Int (Z Zero Zero) = 0
zint2Int (Z a b) = (nat2Int b) - (nat2Int a)
-- TODO: Lmao
int2ZInt :: Int -> ZInt
int2ZInt n 
    | n < 0 = Z (int2Nat (abs n)) (Zero)
    | otherwise = Z (Zero) (int2Nat n)

--e
instance Show ZInt where
    show = showZInts

showZInts n = show(zint2Int n)


-- 2. Aufgabe
data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a)
                    deriving (Show, Eq)

-- Using list2tree, because I'm way to lazy to type in trees by hand
insert :: (Ord a) => a -> BSearchTree a -> BSearchTree a
insert k Nil = Node k Nil Nil
insert k (Node x ltree rtree)
                | k < x = Node x (insert k ltree) rtree
                | otherwise = Node x ltree (insert k rtree)
    
list2Tree :: (Ord a) => [a] -> BSearchTree a
list2Tree [] = Nil
list2Tree (x:xs) = insert x (list2Tree xs)

-- I hate my life, why does she have to fucking fuck us with this shit
mapTree :: (Ord a, Ord b) => (a -> b) -> BSearchTree a -> BSearchTree b
mapTree _ Nil = Nil
mapTree func (Node a ltree rtree) = Node (func a) (mapTree func ltree) (mapTree func rtree)

--foldTree :: (Ord a) => b -> (a -> b -> b -> b) -> BSearchTree a -> b WTF?
-- (neuElem) (function) (Baum)
--NEHMEN wir mal an, dass Frau Esponda ihre Signatur NICHT verkackt hat, sehe foldTree so aus:
foldTree :: (Ord a) => b -> (a -> b -> b -> b) -> BSearchTree a -> b
foldTree b _ Nil = b
foldTree b func (Node x ltree rtree) = func x (foldTree b func ltree) (foldTree b func rtree)
--Diese Funktion funktionier bestimmt, weil ich sie aus dem Internet habe
-- Allerdings kann ich sie nicht testen, weil ich nicht weiß, was der Input ist

-- Nehmen wir an, dass Frau Esponda Frau Esponda ist:

foldTree' :: (Ord a) => b -> (a -> b -> b) -> BSearchTree a -> b
foldTree' b f Nil = b
foldTree' b f (Node x ltree rtree) = foldTree' (f x (foldTree' b f rtree)) f ltree

-- 3. Aufgabe
-- unfold (prädikat) (funktion) (manipulationsfunktion) (element)
unfold p f g x 
               | p x =  []
               | otherwise = f x : unfold p f g (g x)


map' :: (Eq a) => (a -> b) -> [a] -> [b]
map' f = unfold (==[]) (f.head) tail

-- since iterate runs indefinetly, we have to create a
--function that accepts an input and always gives back false
-- Also iterate's first step does not modify x, so we just add it at
--the beginnig of the list
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:unfold (\x -> False) f f x

-- This is just after the basic formula and since it gives out the wrong 
-- list, we reverse dat shit
dec2bin :: Integer -> [Integer]
dec2bin = reverse . (unfold (== 0) (\x -> (mod x 2)) (\x -> (div x 2)))