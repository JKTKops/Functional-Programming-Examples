{- PARTIAL FUNCTION APPLICATION -}
{-
+ is an "infix function
it takes two arguments and goes between them.
In order to write an infix function in haskell without arguments,
we need to surround it in parenthesis, like (+).
-}

-- Remember how square x = x * x was the same as square = \x -> x * x?
-- Well what about:
-- add :: Int -> Int -> Int? Again, notice the "->". "Function from Int to (Function from Int to Int)".
add :: Int -> Int -> Int
add x y = x + y
-- We can translate this to:
-- add = \x -> (\y -> x + y)
-- Then add x = \y -> x + y
-- So add 1 evaluates to the function (\y -> 1 + y)!!
-- We can get this behavior from (+) too, because (+) itself is a function.

addOne :: Int -> Int
addOne x = x + 1
-- addOne = add 1
-- addOne = (+1)

addTwo :: Int -> Int
addTwo x = x + 2
-- addTwo = add 2
-- addTwo = (+2)

addThree :: Int -> Int
addThree x = x + 3
-- addThree = add 3
-- addThree = (+3)


{- FUNCTIONS AS PARAMETERS -}

{- PATTERN MATCHING
remember earlier, we defined
factorial 1 = 1
factorial n = n?
Haskell checks the cases top-down and "matches" the pattern.

(x:xs) is a pattern that matches a non-empty list.
It binds the first element to x, and the rest of the list to xs
-}

-- could also be squareAll :: Num a => [a] -> [a]
squareAll :: [Int] -> [Int]
squareAll [] = []
squareAll (x:xs) = (x * x) : (squareAll xs)

-- could also be halfAll :: Fractional a => [a] -> [a]
halfAll :: [Double] -> [Double]
halfAll [] = []
halfAll (x:xs) = (x / 2) : (halfAll xs)

-- this one can be ... :: Num a => [Int] -> [[a]]
-- because the first argument of replicate has to be Int
transformAllToEmptyListOfLength :: [Int] -> [[Int]]
transformAllToEmptyListOfLength [] = []
transformAllToEmptyListOfLength (x:xs) = (replicate x 0) : (transformAllToEmptyListOfLength xs)


{- Abstract the pattern:
  funcAll :: [a] -> [b]
  funcAll [] = []
  funcAll (x:xs) = (func x) : (funcAll xs) where func :: a -> b

  see "examples.hs" in this folder
-}
