{-# LANGUAGE NPlusKPatterns #-}
-------------------------  Utility Functions  --------------------------
type PRFunction = ([Integer] -> Integer)
-------------------------   Grundfunktionen   --------------------------
z :: PRFunction
z xs = 0

s :: PRFunction
s [x] = x + 1

p :: Integer -> PRFunction
p 1 (a:b) = a
p n (a:b) = p (n-1) b

------------------------- Kompositionsschema --------------------------
compose :: PRFunction -> [PRFunction] -> [Integer] -> Integer
compose f gs xs = f [g xs | g <- gs]

-------------------------  Rekursionsschema  --------------------------
pr :: PRFunction -> PRFunction -> PRFunction -> [Integer] -> Integer
pr rec g h (0:xs) = g xs
pr rec g h ((n+1):xs) = h((rec(n:xs)):n:xs)

-------------------------      sonstige       --------------------------
--Vorgänger
pred' :: PRFunction
pred' = pr pred' (const 0) (p 2)

--Addition
add :: PRFunction
add = pr add (p 1) (compose s [(p 1)])

--Subtraktion
sub :: PRFunction
sub = compose sub' [(p 2), (p 1)]

sub' :: PRFunction
sub' = pr sub' (p 1) (compose pred' [(p 1)])

--Multiplikation
mult :: PRFunction
mult = pr mult z (compose add [(p 1), (p 3)])

--if
iff :: PRFunction
iff = pr iff (p 2) (p 3)

--not
nott :: PRFunction
nott = pr nott (const 1) (const 0)

--isZero
isZero :: PRFunction
isZero = nott

--Ungerade
ungerade :: PRFunction
ungerade = pr ungerade (const 0) (compose nott [(p 1)])

--Hälfte
half :: PRFunction
half = pr half (const 0) (compose add [(p 1), (compose ungerade [(p 2)])])

--Minimum
mini :: PRFunction
mini = compose sub [(p 1), compose sub [(p 1), (p 2)]]

--Exponential
expo :: PRFunction
expo = compose exp' [(p 2), (p 1)]

exp' :: PRFunction
exp' = pr exp' (const 1) (compose mult [(p 1), (p 3)])
------------------------- Aufgaben --------------------------
--Aufgabe 1 

f :: PRFunction
f = compose add [(compose mult [compose pF [p 1], compose h [(p 3), (p 1), (p 2)]]),compose k [p 3]]

pF :: PRFunction
pF = s

h :: PRFunction
h = compose add [(compose add [(p 1), (p 2)]), (p 3)]

k :: PRFunction
k = compose add [(p 1), const 2]

-- Aufgabe 2
maxi :: PRFunction
maxi = compose sub [compose add [(p 1), (p 2)], compose mini [(p 1), (p 2)]]

-- Aufgabe 3
und :: PRFunction
und = compose mult [(p 1), (p 2)]

equal :: PRFunction
equal = compose und [compose isZero [compose sub [(p 1), (p 2)]], compose isZero [compose sub [(p 2), (p 1)]]]

-- Aufgabe 4
f2 :: PRFunction
f2 = compose add [(p 1), compose half [compose mult [compose add [(p 1), (p 3)], compose add [compose add [(p 3), (p 2)], const 2]]]]

p2 :: PRFunction
p2 = compose sub [compose expo [const 2, p 1], const 1]
