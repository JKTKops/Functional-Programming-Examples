-- What if I want to filter for a specific element?
filterFor :: a -> [a] -> [a]
filterFor x = filter (\y -> y == x)

-- But how do we know that type "a" has an == function?

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- correct:
filterFor :: Eq a => a -> [a] -> [a]
filterFor x = filter (\y -> y == x)
-- even better:
filterFor x = filter (== x)

-- With this definition of Eq, an instance of Eq has to define both == AND /=.
-- That's both repetitive and wasteful. We can give them defaults:

class Eq a where
  (==) :: a -> a -> Bool
  (==) x y = not (x /= y)
  (/=) :: a -> a -> Bool
  (/=) x y = not (x == y)
  {-# MINIMAL (==) | (/=) #-}
-- If we don't force the compiler to make the user define one, then it thinks
-- it has a valid implementation of both, but really there's a circular definition.
-- The "MINIMAL pragma" tells the compiler that a "minimal complete definition"
-- contains either a definition for (==) or a definiton for (/=).



-- How can we make our own types "instances" of Eq?
-- Two options:
data Color = Red | Green | Blue deriving (Eq)
-- "Deriving" is only possible for the built-in classes (for now...)
-- And only for some of them.

-- Alternatively we can declare our type and then explicitly make it an "Eq instance"
data Suit = Hearts | Clubs | Spades | Diamonds
instance Eq Suit where
  (==) Hearts Hearts = True
  (==) Clubs Clubs = True
  (==) Spades Spades = True
  (==) Diamonds Diamonds = True
  (==) _ _ = False


-- What about container types? Let's use Maybe, which we've seen before:
data Maybe a = Just a | Nothing
instance (Eq a) => Eq (Maybe a) where
  (==) (Just a) (Just b) = a == b
  (==) Nothing Nothing = True
  (==) _ _ = False

-- Derivable typeclass which can be used to convert data to a String form
-- the "print" method is defined as "putStrLn . show".
-- (print :: (Show a) => a -> String)
class Show a where
  show :: a -> String
