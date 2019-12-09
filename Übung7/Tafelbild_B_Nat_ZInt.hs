{-# LANGUAGE NPlusKPatterns #-}
{-- Vorlesungsbeispiel. Algebraische Datentypen --}
{-- Author: M. Esponda --}

-- import Prelude hiding (Maybe, Just, Nothing)

module ZIntegers where

data B = T | F deriving Show

orB :: B -> B -> B
orB F F = F
orB _ _ = T

andB :: B -> B -> B
andB T T = T
andB _ _ = F

data Nat = Zero | S Nat -- deriving Show

instance Show Nat where
   show = showNats

showNats n = show (nat2Int n)

succN :: Nat -> Nat
succN n = S n

predN :: Nat -> Nat
predN Zero = Zero
predN (S n) = n

addN ::  Nat -> Nat -> Nat
addN a Zero  = a
addN a (S b) = S (addN a b)

ebenN :: Nat -> B
ebenN Zero = T
ebenN (S (S a)) = ebenN a
ebenN   _  = F

multN :: Nat -> Nat -> Nat
multN _ Zero  = Zero
multN a (S b) = addN a (multN a b)

subN :: Nat -> Nat -> Nat
subN m Zero = m
subN m (S n) = predN (subN m n) ---- (n-m) gleich 0, wenn m>n

factorial :: Nat -> Nat
factorial Zero = S Zero
factorial (S a) = multN (S a) (factorial a)

powN :: Nat -> Nat -> Nat   -- the case 0^0 is not defined!
powN (S b)  Zero = S Zero
powN (S b) (S e) = multN (S b) (powN (S b) e)
powN Zero  (S e) = Zero

(<<) :: Nat -> Nat -> B
(<<) Zero (S _)  = T
(<<) (S a) (S b) = (<<) a b
(<<)   _    _    = F

maxN :: Nat -> Nat -> Nat
maxN a b = iff ((<<) a b) b a

foldn :: (Nat -> Nat) -> Nat -> Nat -> Nat
foldn h c Zero  = c
foldn h c (S n) = h (foldn h c n)

foldnP :: (a -> a) -> a -> Nat -> a
foldnP h c Zero = c
foldnP h c (S n) = h (foldnP h c n)

plus' :: Nat -> Nat -> Nat
plus' m n = foldn succN m n

plus :: Nat -> Nat -> Nat
plus = foldn succN

multN' :: Nat -> Nat -> Nat
multN' m = foldn (addN m) Zero

iff :: B -> a -> a -> a
iff T a _ = a
iff F _ b = b

powN' :: Nat -> Nat -> Nat
powN' m = foldn (multN m) (S Zero)

data ZInt = Z Nat Nat -- deriving Show

addZ :: ZInt -> ZInt -> ZInt
addZ (Z a b) (Z c d) = Z (addN a c) (addN b d)

subZ :: ZInt -> ZInt -> ZInt
subZ (Z a b) (Z c d) = Z (addN a d) (addN b c)

simplifyZ :: ZInt -> ZInt
simplifyZ (Z Zero b) = Z Zero b
simplifyZ (Z a Zero) = Z a Zero
simplifyZ (Z (S a) (S b)) = simplifyZ (Z a b)

{- help functions for Testing --}
--  Natural to Integer

nat2Int :: Nat -> Int
nat2Int  Zero = 0
nat2Int (S a) =  1 + (nat2Int a)

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat (n+1) = (S (int2Nat n))
