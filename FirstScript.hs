module FirstScript where
import Data.Char
import Data.List

size :: Integer
size = 12+13

-- The function to square an integer
square :: Integer -> Integer
square n = n*n

-- The function to double an integer
double :: Integer -> Integer
double n = 2*n

-- The function to double the square of an integer
doubleSquare :: Integer -> Integer
doubleSquare n = double(square n)

-- The function to square the square of an integer
fourthPower :: Integer -> Integer
fourthPower n = square (square n)

-- The function to return the factorial of an integer
factorialInteger :: Integer -> Integer
factorialInteger n = product [1..n]

-- The function to return the factorial of an integer
factorialInt :: Int -> Int
factorialInt n = product [1..n]

-- The function to return the hypotenuse of two integers
norm :: Double -> Double -> Double
norm x y = sqrt ((x*x) + (y*y))

-- function returns true if all args are different
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

-- returns golden ratio
phi :: Float
phi = (1 + sqrt 5) / 2

-- function returns the fractional (decimal) part of a float
fractionOfFloat :: Float -> Float
fractionOfFloat x = x - (fromIntegral(floor x))

-- function returns true if one number is the middle of three
between :: Int -> Int -> Int -> Bool
between x y z = (y > x && y < z) || (y < x && y > z)

-- function returns the middle number of three
middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
    | between y x z = x
    | between x y z = y
    | otherwise     = z

-- function returns the middle number of three (local definition)
middleNumberLocal :: Int -> Int -> Int -> Int
middleNumberLocal x y z
    | between y x z = x
    | between x y z = y
    | otherwise = z
    where
        between x y z = (y > x && y < z) || (y < x && y > z)

-- function converts a char to digit if it is a digit
charToDigit :: Char -> Int
charToDigit x = if (isDigit(x)) then (ord x) - 48 else 0

-- function returns true if a year is a leap year
isLeapYear :: Int -> Bool
isLeapYear x
    | x `mod` 400 == 0 = True
    | x `mod` 4 == 0 && x `mod` 100 /= 0 = True
    | otherwise = False

-- returns the number of days in a year
yearDays :: Int -> Int
yearDays x
    | isLeapYear x = 366
    | otherwise    = 365

-- function triples each int in a list
tripleAll :: [Int] -> [Int]
tripleAll ns = [3*n | n <- ns]

-- function squares a list of integers
squareAll :: [Int] -> [Int]
squareAll ns = [n^2 | n <- ns]

-- function capitalise letters
capitalise :: String -> String
capitalise l = [toUpper n | n <- l]

-- function capitalise letters and discards non letters
capitaliseLetters :: String -> String
capitaliseLetters xs = [toUpper x | x <- xs, isAlpha x] 

-- function returns list of numbers that evenly divide the input
divisors :: Int -> [Int]
divisors x = [s | s <- [1..x], x `mod` s == 0]

-- function returns true if param is palindrome
isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

-- function returns true if param is palindrome ignoring case and non-letters
isPalindromeIgnore :: String -> Bool
isPalindromeIgnore x = isPalindrome (capitaliseLetters x)

-- function returns string of words backwards
backwards :: String -> String
backwards x = unwords (reverse (words x))

-- function reverses each word and retains order
backwardsInOrder :: String -> String
backwardsInOrder xs = unwords (reverse (words (reverse xs)))

-- function returns string with title padded with .
contentsLine :: String -> Int -> String
contentsLine x y =
    x ++ replicate ((40 - (length x + length (show y)))) '.' ++ show y

entries :: [(String, Int)] 
entries = [("Hello", 1), ("Second", 2), ("Third", 3)]

-- function formats table of contents as single string
toc :: [(String, Int)] -> String
toc xs = unlines [contentsLine x y | (x, y) <- xs]

-- function tests if a list is non empty
nonEmpty :: [a] -> Bool
nonEmpty [] = False
nonEmpty (x:xs) = True

-- function returns tail of non empty list
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- function redifnes sum
sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

-- functionr returns true if any inputs are true
or2 :: [Bool] -> Bool
or2 [] = False
or2 (x:xs) = x || or2 xs 

-- function is a rewritten zip3
zip32 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip32 (x:xs) (y:ys) (z:zs) = (x,y,z): zip32 xs ys zs
zip32 _ _ _ = []

-- function finds the largest element in a list of Ints
maximum2 :: Int -> [Int] -> Int
maximum2 y (x:xs) = maximum2 (max x y) xs
maximum2 y _ = y

-- function returns largest value in non empty list
myMaximum :: [Int] -> Int
myMaximum (x:xs) = maximum2 x xs
myMaximum _ = 0

-- function is a non recursive version of elem
elem2 :: Eq a => a -> [a] -> Bool
elem2 x [] = False
elem2 x (y:ys) = x == y || or2 [n == x | n <- ys]

-- function retuns number of times a Char occurs in String
count :: Char -> [Char] -> Int
count x [] = 0
count x (y:ys) = (if x == y then 1 else 0) + count x ys 

-- function rewrites take
drop2 :: Int -> [a] -> [a]
drop2 n [] = []
drop2 n (x:xs)
    | n > 0 = drop2 (n - 1) xs
    | otherwise = x:xs

-- function returns substring of letters at the front
takeWord :: [Char] -> [Char]
takeWord [] = []
takeWord (c:cs)
    | isAlpha c = c:takeWord cs
    | otherwise = []

-- function returns substring of letters at the back
dropWord :: [Char] -> [Char]
dropWord [] = []
dropWord (c:cs)
    | isAlpha c = dropWord cs
    | otherwise = c:cs

-- function returns range
range :: Int -> Int -> [Int]
range x y
    | x > y = []
    | x == y = [x]
    | otherwise = x:(range (x+1) y)

--function orders two lists
merge :: Ord a => [a] -> [a] -> [a]
merge (y:ys) [] = y:ys
merge [] (x:xs) = x:xs
merge (x:xs) (y:ys) = if (x<y) then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)  
merge _ _ = []

-- function returns odd indexed elements as a list
odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x:(evens xs)

-- function returns even indexed ekements as a list
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = odds xs

-- function sorts an unordered list
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = merge (mergeSort (odds x)) (mergeSort (evens x))

-- function returns list copitalised
capitalise2 :: String -> String
capitalise2 xs = map toUpper xs

-- function selects capital letters from the list
capitals :: String -> String
capitals xs = filter isUpper xs

-- function selects capitals and discards non letters
capitalise3 :: String -> String
capitalise3 xs = filter isAlpha (capitalise2 xs) 

-- function is a non recursive reverse
reverse2 :: [a] -> [a]
reverse2 xs = foldr f [] xs
    where f x y = y ++ [x]

-- function redefines length using foldr
length2 :: [a] -> Int
length2 = foldr addOne 0
    where addOne x n = 1 + n

-- function removes first non letter
filterFirstNonLetter :: [Char] -> [Char]
filterFirstNonLetter [] = []
filterFirstNonLetter (x:xs)
    | isAlpha x = x:filterFirstNonLetter xs
    | otherwise = xs

-- functions generalises above function
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p (x:xs)
    | p x = x:filterFirst p xs
    | otherwise = xs

-- function returns the squares from 1 to 20
squares2 :: [Int] -> [Int]
squares2 = map (^2)

-- function returns square numbers less than 500
squareNumbers :: [Int] -> [Int]
squareNumbers xs = takeWhile (< 500) (map (^2) [1..]) 

count2 :: Eq a => a -> [a] -> Int
count2 x ys = length (filter (== x) ys) 

-- function sums the squares
sumSquares :: [Int] -> Int
sumSquares = sum . map (^2)

data Colour = Red | Green | Blue
    | Yellow | Cyan | Magenta
    | Black | White
    deriving Show

inverseColour :: Colour -> Colour
inverseColour Red = Cyan
inverseColour Green = Magenta
inverseColour Blue = Yellow
inverseColour Yellow = Blue
inverseColour Cyan = Red
inverseColour Magenta = Green
inverseColour Black = White
inverseColour White = Black

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

data Shape
    = Circle Double
    | Rectangle Double Double
    deriving Show

rotateRight :: Shape -> Shape
rotateRight (Circle r) = Circle r
rotateRight (Rectangle w h) = Rectangle h w

scale :: Double -> Shape -> Shape
scale s (Circle r) = Circle (r * s)
scale s (Rectangle w h) = Rectangle (w * s) (h * s)