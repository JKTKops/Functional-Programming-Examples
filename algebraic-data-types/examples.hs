
-- data Bool = True | False

data TrafficLight = Red | Yellow | Green


data ListOfInt = End | Node Int ListOfInt
exampleList1 :: ListOfInt
exampleList1 = Node 1 (Node 2 (Node 3 (Node 4 (End))))

data ListOfBool = End' | Node' Bool ListOfBool
exampleList2 :: ListOfBool
exampleList2 = Node' True (Node' True (Node' False (Node' True (End'))))

data List a = Empty | Cons a (List a)
exampleList3 :: List Int
exampleList3 = Cons 1 (Cons 2 (Cons 3 (Empty)))

exampleList4 :: List Bool
exampleList4 = Cons True (Cons False (Empty))
