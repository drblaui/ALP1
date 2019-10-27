--Aufgabe 1
isParenthesis :: Char -> Bool
--Easy check with a big if
isParenthesis x = if (x == '[' || x == ']' || x == '(' || x == ')' || x == '{' || x == '}')
                   then True
                   else False


--Aufgabe 2
leap_year :: Int -> Bool
--Easy Online, as suggested by the assignment
leap_year x = ((x `mod` 4) == 0 && not ((x `mod` 100) == 0)) ||(x `mod` 400) == 0


--Aufgabe 3
--a, b und c
weekday :: Int -> Int -> Int -> [Char]
weekday day month year
                       -- Easy Guards Check, we do not check for days in month because we want to stay easy, lazy and most of all readable
                       | ((day < 1) || (day > 31))     = show(day) ++ " is an illegal day value"
                       | ((month < 1) || (month > 12)) = show(month) ++ " is an illegal month value"
                       | year < 0                      = show(year) ++ " is an illegal year value"
                       --Case-Verteiler, damit wir auch von der Berechnung in [Char] Form kommen
                       | otherwise                     = case (mod (day + x + (31 * m0) `div` 12) 7) of
                                                                                                     0 -> "Sunday"
                                                                                                     1 -> "Monday"
                                                                                                     2 -> "Tuesday"
                                                                                                     3 -> "Wednesday"
                                                                                                     4 -> "Thursday"
                                                                                                     5 -> "Friday"
                                                                                                     6 -> "Saturday"
                                                         --Formula out of the lecture, so we wont explain it
                                                         where
                                                          y0 = year - ((14 - month) `div` 12)
                                                          x = y0  + y0 `div` 4 - y0 `div` 100 + y0 `div` 400
                                                          m0 = month + 12 * ((14 - month) `div` 12) - 2



--Aufgabe 4
--already given functions
paintChars f size = putStrLn (genChars f size)

genChars :: ((Int, Int, Int) -> Char) -> Int -> [Char]
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)
                       
         
diag (x,y,size) = if (x==y) then 'a' else ' '

quad (x,y,size) = if (x>s && x<3*s && y>s && y<3*s) then ' ' else '+'
                  where
                    s = div size 4

gitter (x,y,size) = if k || p  then ' ' else '0'
                    where
                         k = (mod x space)==0
                         p = (mod y space)==0
                         space = div size 4

-- REMEMBER THE GRID STARTS AT 1

-- Holy Shit I cant believe it was this easy
diags (x,y,size) = if ((x `mod` 15) == (y `mod` 15)) then ' ' else '0'

rectangles (x,y,size) 
                      | (x <= s) = '*'
                      | (((x > s) && (x <= (s + t))) && (y > s)) = '8'
                      | ((x > (s + t)) && (y > s + t)) = '|'
                      | otherwise = ' '
                      where
                        s = div size 2
                        t = div size 4

flags (x,y,size)
                 | ((x <= s) && (x `mod` 8 <= 4) && (y <= s)) = '|'
                 | ((x > s) && (y <= s) && (y `mod` 8 <= 4)) = '*'
                 | ((y > s) && (x `mod` 8 <= 4)) = '+'
                 | otherwise = ' '
                 where
                  s = div size 2

 
                  
{- Why tf is this so hard. Am I just retarded? -}
circle (x,y,size)
                  | ((x <= y) && (y < s)) = '.'
                  | ((t < y) && (y < s)) = '.'
                  | (not(x <= y) && (y >= s)) = '.'
                  | (not(t < y) && (y >= s)) = '.'
                  | otherwise = '#'
                  where
                    s = div size 2
                    t = size - x

circles (x,y,size)
                   | (x == (size `div` 2)) = '.'
                   | otherwise = '*'

{- Testbeispiele -}

test1 = paintChars diag 10
test2 = paintChars quad 60
test3 = paintChars gitter 60
test4 = paintChars rectangles 60
test5 = paintChars flags 60
test6 = paintChars diags 60
test7 = paintChars circle 60
test8 = paintChars circles 60

--29
--22
