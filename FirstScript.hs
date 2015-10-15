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
backwards x = reverse x

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