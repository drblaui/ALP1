--Aufgabe 3a
pow2 = (2 **)
pow2' = \x ->  x * x * 1.0

f = (div 100)
g = (* 2)
h = (+ 3)

--Aufgabe 3b
after = (f.g.h)
after' = \x -> f(g(h(x)))


-- Aufgabe 4
reverse' ys = foldl (\xs x -> x:xs) [] ys

--Aufgabe 5
-- I hate this this is all wrong
fix :: (a -> a) -> a
fix f = let {x = f x} in x

collatz = \x -> if (x == 1) then [1] else (if (mod x 2 == 0) then x:(collatz (div x 2)) else x:(collatz (x * 3 + 1)))