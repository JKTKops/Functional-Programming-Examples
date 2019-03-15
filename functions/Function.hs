factorial :: Int -> Int -- Declare name and type
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- A "lambda expression" - "\" looks like a lambda if you squint really hard
(\x -> x + 1)

-- Giving a lambda expression a name
square = (\x -> x * x)
-- This is the same as
square x = x * x


-- Note that this file will not compile because
-- loose lambda expressions are not valid Haskell syntax.
