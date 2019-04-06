main :: IO ()
main = print triples

triples :: [(Int, Int, Int)] -- list of triples of integers
triples = [ (a, b, c) | a <- [1..10], b <- [a..10], c <- [b..10], a^2 + b^2 == c^2 ]
