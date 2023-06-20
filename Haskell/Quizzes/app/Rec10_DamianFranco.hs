studentName = ["Franco", "Damian"]
netID = "dfranco24@unm.edu"

increasing :: Ord a => [a] -> Bool
increasing xs = and $ map (\(x,y) -> x <= y) $ zip xs (tail xs)

-- define localMaxima using a list comprehension and zip3
--  such that, given a list of orderable items
--              returns a list of the items that are greater than
--              their predecesor and their successor
localMaxima :: Ord a => [a] -> [a]
localMaxima xs = [y | (x,y,z) <- zip3 (xs) (tail xs) (drop 2 xs), x < y && y > z]

-- [ ??? | x <- ??? zip3 ???, ??? ]

tests = [localMaxima [1..10] == [], localMaxima [1,2,1,7,8,3,9,9,10] == [2,8]]