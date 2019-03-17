-- There's a similar typeclass called applicative,
-- for applying functions in contexts to values in the same context.
--
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- Applicative also requires a way to "lift" a function or value
-- into the context, called "pure."
--
-- pure :: (Applicative f) => a -> f a

-- Lists are an applicative functor:
testPure :: [Int]
testPure = pure 5

testAp :: [Int]
testAp = [(+1), (*2)] <*> [3, 4]

-- So is Maybe:
testPureMaybe :: Maybe Int
testPureMaybe = pure 5

testApMaybe :: Maybe Int -> Maybe Int
testApMaybe x = pure (+4) <*> x

main = do
  print testPure
  print testAp
  print testPureMaybe
  print (testApMaybe $ Just 2)
  print (testApMaybe $ Nothing)
