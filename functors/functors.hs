-- So that this file compiles, code with namespace conflicts is commented out.

-- We can think of a functor as a way of putting values into a context
-- where it makes sense to apply functions to values in that context.
-- For example, the context of a list is a context of having multiple (0 or more)
-- values. With Maybe, the context is having 0 or 1 values. 
-- There are other contexts too; the Tree type further down the page has a
-- context of structure.

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) = fmap . const

-- note: const :: a -> b -> a; const constant _ = constant

instance Functor [] where
  fmap = map
-}


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)

instance Functor Tree where
  fmap = treeMap


squareAll :: (Functor f) => f Int -> f Int
squareAll = fmap (\x -> x * x)

showAll :: (Functor f, Show a) => f a -> f String
showAll = fmap show


-- Don't worry about how this main works, yet!
-- The "do" notation is for a typeclass called "Monad."
main =  do
  putStrLn "Which case do you want to see?"
  input <- getLine
  let choice = (read input :: Int)
  case choice of
    0 -> print $ squareAll [1, 2, 3, 4, 5]
    1 -> print $ squareAll (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 (Node 5 Empty Empty) Empty))
    2 -> print $ showAll [1, 2, 3, 4, 5]
    3 -> print $ showAll (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 (Node 5 Empty Empty) Empty))
    otherwise -> (putStrLn "That's not a valid option, try again") >> main
    
{-
We expect functors to satisfy a rule:
fmap id functor = functor
-}
