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
{-
Since I think that how pr and compose work ist pretty clear,
I really just test it here, because explaining it would just be
me saying what the functions do and I think that is extremely trivial
-}

--Aufgabe 1 
f :: PRFunction
f = compose add [(compose mult [compose pF [p 1], compose h [(p 3), (p 1), (p 2)]]),compose k [p 3]]

{-
I just assigned some primitive recursive functions to pF, h and k, 
because I'm to lazy to test it otherwise and also it wouldn't work
without defining these functions anyways
Also I called p pF here, because p is our projection function
-}
pF :: PRFunction
pF = s

h :: PRFunction
h = compose add [(compose add [(p 1), (p 2)]), (p 3)]

k :: PRFunction
k = compose add [(p 1), const 2]

-- Aufgabe 2
-- a)
maxi :: PRFunction
maxi = compose sub [compose add [(p 1), (p 2)], compose mini [(p 1), (p 2)]]

--b)
fac :: PRFunction
fac = pr fac (const 1) (compose mult[compose s [p 2], (p 1)])

-- Aufgabe 3
--a)
und :: PRFunction
und = compose mult [(p 1), (p 2)]

--b)
equal :: PRFunction
equal = compose und [compose isZero [compose sub [(p 1), (p 2)]], compose isZero [compose sub [(p 2), (p 1)]]]

-- Aufgabe 4
--a)
f2 :: PRFunction
f2 = compose add [(p 1), compose half [compose mult [compose add [(p 1), (p 3)], compose add [compose add [(p 3), (p 2)], const 2]]]]

--b)
p2 :: PRFunction
p2 = compose sub [compose expo [const 2, p 1], const 1]

--c)
abst :: PRFunction
abst = compose sub [compose maxi [(p 1), (p 2)], compose mini [(p 1), (p 2)]]

--d)
f3 :: PRFunction
f3 = pr f3 (const 1) (compose add [(p 1), compose s [p 2]])

------------------------- Tests --------------------------
{-
I just test about 3 cases for every function, 
so it becomes clear that they should work
-}
testF101 = f[3..5]
testF102 = f[4,1,19]
testF103 = f[100, 10, 1]

testMax01 = maxi[5,2]
-- Fun Fact: Big numbers sure like to cause a stack overflow
testMax02 = maxi[1000000, 40]
testMax03 = maxi[1290, 8973]

testFac01 = fac[1]
testFac02 = fac[9]
testFac03 = fac[3]

testAnd01 = und[1,1]
testAnd02 = und[1,0]
testAnd03 = und[0,0]

testEq01 = equal[10,10]
testEq02 = equal[10,9]
testEq03 = equal[1,58]

testF201 = f2[3..5]
testF202 = f2[4,1,9]
testF203 = f2[100, 10, 1]

testP201 = p2[10]
testP202 = p2[3]
testP203 = p2[1]

testAbst01 = abst[10,10]
testAbst02 = abst[9,30]
testAbst03 = abst[56, 2]

testF301 = f3[0]
testF302 = f3[20]
testF303 = f3[6]

main = do
    print "Test for Number 1"
    print "Test with 3,4,5 should return 55"
    print testF101
    print "Test with 4,1,19 should return 141"
    print testF102
    print "Test with 100, 10, 1 should return 11214"
    print testF103
    print "Test for Number 2"
    print "Test for max Function"
    print "Test with 5 and 2 should return 5"
    print testMax01
    print "Test with 1000000 and 40 should return 1000000"
    print testMax02
    print "Test with 1290 and 8973 should return 8973"
    print testMax03
    print "Test for Faculty function"
    print "Test with 1 should return 1"
    print testFac01
    print "Test with 9 should return 362880"
    print testFac02
    print "Test with 3 should return 6"
    print testFac03
    print "Test for Number 3"
    print "Test for and Function"
    print "Test with 1 and 1 should return 1"
    print testAnd01
    print "Test with 1 and 0 should return 0"
    print testAnd02
    print "test with 0 and 0 should return 0"
    print testAnd03
    print "Test for equal function"
    print "Test for 10 and 10 should return 1(True)"
    print testEq01
    print "Test for 10 and 9 should return 0(False)"
    print testEq02
    print "Test for 1 and 58 should return 0(False)"
    print testEq03
    print "Test for Number 4"
    print "Test for f Function"
    print "Test with 3, 4 and 5 should return 47"
    print testF201
    print "Test with 4, 1 and 9 should return 82"
    print testF202
    print "Test with 100, 10 and 1 should return 756"
    print testF203
    print "Test for p Function"
    print "Test with 10 should return 1023"
    print testP201
    print "Test with 3 should return 7"
    print testP202
    print "Test with 1 should return 1"
    print testP203
    print "Test for abst Function"
    print "Test with 10 and 10 should return 0"
    print testAbst01
    print "Test with 9 and 30 should return 21"
    print testAbst02
    print "Test with 56 and 2 should return 54"
    print testAbst03
    print "Test for f function"
    print "Test with 0 should return 1"
    print testF301
    print "Test with 20 should return 221"
    print testF302
    print "Test with 6 should return 22"
    print testF303