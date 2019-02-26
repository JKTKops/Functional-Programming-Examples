factorial :: Int -> Int -- Declare name and type
factorial 1 = 1
factorial n = n * factorial (n - 1)
