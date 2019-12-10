{-# LANGUAGE NPlusKPatterns #-}
-- 1. Aufgabe
data B = T | F deriving Show
data Nat = Zero | S Nat

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

data ZInt = Z Nat Nat deriving Show

addZ :: ZInt -> ZInt -> ZInt
addZ (Z a b) (Z c d) = Z (addN a c) (addN b d)

subZ :: ZInt -> ZInt -> ZInt
subZ (Z a b) (Z c d) = Z (addN a d) (addN b c)

simplifyZ :: ZInt -> ZInt
simplifyZ (Z Zero b) = Z Zero b
simplifyZ (Z a Zero) = Z a Zero
simplifyZ (Z (S a) (S b)) = simplifyZ (Z a b)

--Funktionen, die ich noch brauchte
divN :: Nat -> Nat -> Nat
divN a b = int2Nat((nat2Int a) `div` (nat2Int b))

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
eqN a b = if ((nat2Int a) == (nat2Int b)) then T else F

oddN :: Nat -> B
oddN (Zero) = F
oddN (S (Zero)) = T
oddN (S (S n)) = oddN n

fibonacci :: Nat -> Nat
fibonacci Zero = addN Zero Zero
fibonacci (S (Zero)) = addN Zero (S(Zero))
fibonacci n = addN (fibonacci(subN n (S(Zero)))) (fibonacci(subN n (S(S(Zero)))))

isTeilerN :: Nat -> Nat -> B
isTeilerN a b = if (nat2Int a) `mod` (nat2Int b) == 0 then T else F

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
eqZ :: ZInt -> ZInt -> B
eqZ (Z a b) (Z c d) = eqB (eqN a c) (eqN b d)

(<<<) :: ZInt -> ZInt -> B
(<<<) (Z a b) (Z c d) = orB ((<<) a c) ((<<) b d)

{-TODO: negZ when I know what the fuck it is-}

-- MaxN but with the smaller operator for ZInt
maxZ :: ZInt -> ZInt -> ZInt
maxZ a b = iff ((<<<) a b) b a

multZ :: ZInt -> ZInt -> ZInt
multZ (Z a b) (Z c d) = Z (multN a c) (multN b d)

{- abs == Betrag TODO: absZ, when I know what ZInt actually really is-}

powZ :: ZInt -> Nat -> ZInt
powZ (Z a b) n = Z (powN' a n) (powN' b n)

isTeilerZ :: ZInt -> ZInt -> B
isTeilerZ (Z a b) (Z c d) = eqB (isTeilerN a c) (isTeilerN b d)

ggtZ :: ZInt -> ZInt -> ZInt
ggtZ (Z a b) (Z c d) = Z (ggtN a c) (ggtN b d)