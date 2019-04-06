map_ :: (a -> b) -> [a] -> [b]
map_ _ [] = []
map_ f (x:xs) = (f x) : (map_ f xs)

squareAll = map (\x -> x * x)
halfAll = map (/2)
mapToEmptyLists = map (\x -> replicate x 0)


-- "p" for "predicate," or a function that is either "True" or "False"
filter_ :: (a -> Bool) -> [a] -> [a]
filter_ _ [] = []
filter_ p (x:xs) = if (p x)
  then x : rest
  else rest
  where
    rest = filter p xs


-- Suppose I have a database with customers IDs, which correspond to
-- Customers who all have some data: a "total spent" and an address.
-- Our store also has an address. Let's say we want to find
-- only those who have spent over $1000 and who live within 10 miles:

type Database = (String, [Int])

type Customer = (Double, Int)

closeHighSpenders :: Database -> [Customer]
closeHighSpenders = filter (\customer -> distanceMiles (addressOf customer) storeAddress < 10)
  . filter (\customer -> totalSpending customer > 1000)
  . map customerFromId
  . listOfIds

listOfIds :: Database -> [Int]
listOfIds (_, ids) = ids

customerFromId :: Int -> Customer
customerFromId x = (fromIntegral x * 234.56, x)       -- we don't really have a database so we have to make stuff up

totalSpending :: Customer -> Double
totalSpending (t, _) = t

addressOf :: Customer -> Int
addressOf (_, a) = a

storeAddress :: Int
storeAddress = 12

distanceMiles :: Int -> Int -> Int
distanceMiles x y = abs (y - x)

main = print $ closeHighSpenders ("my database", [0..10])
