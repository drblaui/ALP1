-- Alexander Rudolph, Florian Formanek
--Tutorium 06

{- Unter jeder Funktion einen Testlauf machen 
  Für Schriftlich: Namen, Tutorium, Tutorname-}

--Aufgabe 1
isParenthesis :: Char -> Bool
--Easy check with a big if
isParenthesis x = if (x == '[' || x == ']' || x == '(' || x == ')' || x == '{' || x == '}')
                  then True
                  else False

test01A1 = isParenthesis '['
test02A1 = isParenthesis 'a'

--Aufgabe 2
--Well I mean these are basically all the same, but I mean it does what the assignment wants, right?
--a
leap_year01 :: Int -> Bool
leap_year01 x = if(((x `mod` 4) == 0 && not ((x `mod` 100) == 0)) ||(x `mod` 400) == 0) 
                then(True) 
                else(False)

test01A2A = leap_year01 2020
test02A2A = leap_year01 2019
--b
leap_year02 :: Int -> Bool
leap_year02 x 
            | (((x `mod` 4) == 0 && not ((x `mod` 100) == 0)) ||(x `mod` 400) == 0) = True
            | otherwise = False

test01A2B = leap_year02 2020
test02A2B = leap_year02 2019
--c
leap_year :: Int -> Bool
leap_year x = ((x `mod` 4) == 0 && not ((x `mod` 100) == 0)) ||(x `mod` 400) == 0

test01A2C = leap_year 2020
test02A2C = leap_year 2019


--Aufgabe 3
weekday :: Int -> Int -> Int -> [Char]
weekday day month year
                       -- Easy Guards Check where we check for every possible case
                       -- 3b
                       | ((not (leap_year year) && month == 2 && day > 28) || (month == 2 && day > 29) || ((elem month [4, 6, 9, 11]) && day > 30) || ((elem month [1,3,5,6,8,10,12]) && day > 31) || day < 1) = error(show(day) ++ " is an illegal day value")
                       | ((month < 1) || (month > 12)) = error (show(month) ++ " is an illegal month value")
                       | year < 0                      = error (show(year) ++ " is an illegal year value")
                       --Case of, because we want to get [Char] from a Int
                       -- This corresponds to both 3a AND 3c
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

test01A3 = weekday 29 2 2020


--Aufgabe 4
--already given functions
paintChars f size = putStrLn (genChars f size)

genChars :: ((Int, Int, Int) -> Char) -> Int -> [Char]
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)
                       

diags :: (Int, Int, Int) -> Char
-- Since there is a new Diagonal Line every 15th Char, we just look if our current coordinate is on a 15th place of anything
diags (x,y,size) = if ((x `mod` 15) == (y `mod` 15)) then ' ' else '0'

testDiags = paintChars diags 60

rectangles :: (Int, Int, Int) -> Char
{-
  The height halves with every new type of char, we just divide the size by two
  The first "block" of chars is always half the whole size and the other ones are half the size of the first block
-}
rectangles (x,y,size) 
                      | (x <= s) = '*'
                      | (((x > s) && (x <= (s + t))) && (y > s)) = '8'
                      | ((x > (s + t)) && (y > s + t)) = '|'
                      | otherwise = ' '
                      where
                        s = div size 2
                        t = div size 4

testRectangles = paintChars rectangles 60

flags :: (Int, Int, Int) -> Char
{-
  If I'm honest I just got this from trying around a bit
  Since there are always 8 Chars (4 Spaces and whatever other chars) we just divide the coordinate by 8 and if the rest is smaller than 4, it means were in the bottom half of the 8, so we paint chars
  The Stars basically work the same, but with the y coordinate
-}
flags (x,y,size)
                 | ((x <= s) && (x `mod` 8 <= 4) && (y <= s)) = '|'
                 | ((x > s) && (y <= s) && (y `mod` 8 <= 4)) = '*'
                 | ((y > s) && (x `mod` 8 <= 4)) = '+'
                 | otherwise = ' '
                 where
                  s = div size 2

testFlags = paintChars flags 60

{-
  These dont look exactly like in the Assignment, but they're atleast worth some extra Point, right?
-}
 
circles :: (Int, Int, Int) -> Char
--We calculate the distance from the current coordinate and the center point. The distance chances, what Char we print
circles (x,y,z)
    | (x-p)^2+(y-p)^2 < ((div (div z 2) 2)-1)^2  = ' '
    | (x-p)^2+(y-p)^2 < (p-1)^2  = '.'         
    | otherwise = '*'
        where p = div z 2  

testCircles = paintChars circles 60

circle :: (Int, Int, Int) -> Char
--Works basically the same like circles
circle (x,y,z)
    | (x-p)^2+(y-p)^2 > ((div ((div z 3)*2) 2)-1)^2 && x-y>=0 && z-x-y>0 = '#'
    | (x-p)^2+(y-p)^2 > ((div ((div z 3)*2) 2)-1)^2 && x+y-z>0 && y>=x  = '#'
    | (x-p)^2+(y-p)^2 > ((div ((div z 3)*2) 2)-1)^2 && y-x>=0 = '.'
    | (x-p)^2+(y-p)^2 > ((div ((div z 3)*2) 2)-1)^2 && z-y-x<=0 = '.'
    | otherwise = ' '
        where p = div z 2

testCircle = paintChars circle 60

main = do
  print "Test for Number 1."
  print "Putting in '[' should return True"
  print test01A1
  print "Putting in 'a' should return False"
  print test02A1
  print "Test for Number 2."
  print "We test everything with 2020(True) and then 2019(False)"
  print "For A we get:"
  print "2020"
  print test01A2A
  print "2019"
  print test02A2A
  print "For B we get:"
  print "2020"
  print test01A2B
  print "2019"
  print test02A2B
  print "And for C we get:"
  print "2020"
  print test01A2C
  print "2019"
  print test02A2C
  print "Test for Number 3."
  print "Testing for 29 2 2020 should return True"
  print test01A3
  print "Testing for 0 10 2000 should return an Error but we don't try that out, because the program would cancel"
  print "Test for Number 4."
  print "All pictures are painted with size 60"
  testDiags
  print ""
  testRectangles
  print ""
  testCircles
  print ""
  testFlags
  print ""
  testCircle