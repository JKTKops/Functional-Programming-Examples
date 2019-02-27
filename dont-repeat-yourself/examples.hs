demo1 :: Int -> Int
demo1 x = (x * x + 3) + 1

demo2 :: Int -> Int
demo2 x = (x * x + 3) * 2

demo3 :: Int -> Int
demo3 x = if (x * x + 3) < 10
  then 0
  else 1



help :: Int -> Int
help x = x * x + 3

better1 :: Int -> Int
better1 x = 1 + help x

better2 :: Int -> Int
better2 x = 2 * help x

better3 :: Int -> Int
better3 x = if (help x) < 10
  then 0
  else 1
