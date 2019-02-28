data Bool = True | False

data TrafficLight = Red | Yellow | Green


-- bad, repetetive ADTs
data MaybeInt = JustInt Int | NothingInt

data MaybeBool = JustBool Bool | NothingBool

data MaybeList = JustList [] | NothingList

-- Good, general ADT - this one comes packaged with Haskell!
data Maybe a = Just a | Nothing

data List a = Empty | Cons a (List a)
exampleList3 :: List Int
exampleList3 = Cons 1 (Cons 2 (Cons 3 (Empty)))

exampleList4 :: List Bool
exampleList4 = Cons True (Cons False (Empty))
