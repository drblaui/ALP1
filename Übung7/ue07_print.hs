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