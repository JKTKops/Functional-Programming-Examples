-- https://projecteuler.net/problem=1

-- People generally don't like sharing solutions to Project Euler,
-- so we recommend solving this problem yourself before looking at our solution.
-- We've left a large amount of whitespace here to try and hide it :)
-- Our solution starts on line 100





























































































nums :: [Int]
nums = [1..999]

mul3or5 :: [Int] -> [Int]
mul3or5 = filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0)

main = print . sum . mul3or5 $ nums

-- or with list comprehension --
answer :: Int
answer = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- The inner workings of list comprehensions are beyond the scope
-- of what we can talk about in an hour, but internally it
-- is using an even more general (!!!) version of filter.
