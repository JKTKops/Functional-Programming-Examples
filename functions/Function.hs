-- A "lambda expression" - "\" looks like a lambda if you squint really hard
\x -> x + 1
\name -> "Hello: " ++ name

-- We can (and should, usually) also specify the type of a lambda expression
(\x -> x + 1) :: Int -> Int
(\name -> "Hello: " ++ name) :: String -> String

-- What if we want multiple arguments? Haskell lambdas can only have one argument, so...
-- We'll talk about this soon :)

factorial :: Int -> Int -- Declare name and type
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- we can make this look "more like" the procedural one by saying
factorial n = foldr (*) 1 [1..n]
-- But this is harder to read in many cases and actually ends up doing more work.
-- We'll talk about functions like foldr later;
-- foldr in this case essentially is putting the * operator between every element
-- of the list [1, 2, ..., n], so we get 1 * 2 * ... * n.

-- Giving a lambda expression a name
square = \x -> x * x
-- This is the same as
square x = x * x


-- Note that this file will not compile because
-- loose lambda expressions are not valid Haskell syntax.
