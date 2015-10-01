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
factorial :: Integer -> Integer
factorial n = product [1..n]

-- The function to return the hypotenuse of two integers
norm :: Double -> Double -> Double
norm x y = sqrt ((x*x) + (y*y))