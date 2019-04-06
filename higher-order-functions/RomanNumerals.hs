data Letter = I | V | X | L | C | D | M deriving (Show, Enum, Bounded)

-- showList is defined in GHC.Show, which is a standard library module
-- So we have to use a worse name :(
listShow :: (Show a) => [a] -> [Char]
listShow = concat . showAll
  where
    showAll [] = []
    showAll (x:xs) = (show x) : showAll xs

nthDigit :: Int -> Int -> Int
nthDigit n i = floor $ (fromIntegral $ i `mod` 10 ^ n) / (10 ^ (n - 1))

thousands :: Int -> [Letter]
thousands i = replicate (floor $ (fromIntegral i) / 1000) M

letterMap :: Letter -> Int -> [Letter]
letterMap s i
  | i < 4 = replicate i s
  | i == 4 = [s, m]
  | i < 9 = m : replicate (i - 5) s
  | i == 9 = [s, l]
    where
      -- succ :: (Enum a) => a -> a
      m = succ s
      l = succ $ succ s

translate :: Letter -> Int -> Int -> [Letter]
translate letter n i = letterMap letter (nthDigit n i)

hundreds :: Int -> [Letter]
hundreds = translate C 3

tens :: Int -> [Letter]
tens = translate X 2

ones :: Int -> [Letter]
--ones = translate I 1
ones = translate I 1

toRoman :: Int -> [Letter]
toRoman i = (thousands i) ++ (hundreds i) ++ (tens i) ++ (ones i)
-- The above line has a pattern of its own.
-- It can be abstracted, but we're a few steps away from that.

main = putStrLn . listShow . toRoman $ 12345
