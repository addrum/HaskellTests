module FirstScript where

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