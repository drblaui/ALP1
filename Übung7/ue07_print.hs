{-# LANGUAGE NPlusKPatterns #-}
-- 1. Aufgabe
data B = T | F deriving Show
data Nat = Zero | S Nat
-- Functionen von Frau Esponda

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

-- a
{-
TODO: eqN und isTeilerN
-}
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

oddN :: Nat -> B
oddN (Zero) = F
oddN (S (Zero)) = T
oddN (S (S n)) = oddN n

fibonacci :: Nat -> Nat
fibonacci Zero = addN Zero Zero
fibonacci (S (Zero)) = addN Zero (S(Zero))
fibonacci n = addN (fibonacci(subN n (S(Zero)))) (fibonacci(subN n (S(S(Zero)))))

isTeilerN :: Nat -> Nat -> B
isTeilerN _ Zero = F
isTeilerN Zero _ = T
isTeilerN a b = isTeilerN (subN a b) b
{-
ggtN :: Nat -> Nat -> Nat
ggtN a 0 = a
ggtN a b = ggtN b (a `mod` b)
--`mod` is equal a - (b * (a/b))-}

divN :: Nat -> Nat -> Nat
divN Zero _ = Zero
divN (S a) b = subN (S a) (multN a b)
--floordiv