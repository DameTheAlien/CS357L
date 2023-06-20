
studentName = ["Franco", "Damian"]
netID = "dfranco24@unm.edu"


-- Problem 1, stutter
stutter :: [a] -> [a]
stutter [] = []
stutter (x:xs) = x : x : stutter xs

p1tests = [(stutter "Hello World") == "HHeelllloo  WWoorrlldd", (stutter [1,2,3]) == [1,1,2,2,3,3]]


-- Problem 2, compress
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress (dropWhile (\f -> f == x) xs))

p2tests = [compress "HHeelllloo WWoorrlldd" == "Helo World",  compress [1,2,2,3,3,3] == [1,2,3]]


-- Problem 3, findIndices
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices pred [] = []
findIndices pred ls = [i | (x, i) <- zip ls [0..], pred x]

p3tests = [findIndices (< 'a') "AbCdef" == [0,2], findIndices (== 0) [1,2,0,3,0] == [2,4]]


-- Problem 3.5, intersect
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] [] = []
intersect ls0 ls1 = [i | i <- ls0, any (== i) ls1]

p35tests = [intersect "abc" "cat" == "ac", intersect [1,2,3] [8] == [], intersect [3,2,1] [1,2,3] == [3,2,1]]


-- Problem 4, isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] [] = True
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys 

p4tests = ["foo" `isPrefixOf` "foobar", not $ isPrefixOf [1,2,3] [4,5,6]]


-- Problem 5, isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf ls0 ls1 = isPrefixOf (reverse ls0) (reverse ls1)

p5tests = ["bar" `isSuffixOf` "foobar", not $ isSuffixOf [1,2,3] [4,5,6]]


-- Problem 6, dot
dot :: [Int] -> [Int] -> Int
dot [] [] = 0
dot (x:xs) (y:ys) =  x * y + (dot xs ys)

p6tests = [[0,0,1] `dot` [0,1,0] == 0]


-- Problem 7, increasing
increasing :: (Ord a) => [a] -> Bool
increasing [] = False
increasing (x:xs) = if (null xs)
                    then True
                    else (if (x > head xs) then False else increasing xs)

p7tests = [increasing "ABCD", not $ increasing [100,99..1]]


-- Problem 8, decimate
decimate :: [a] -> [a]
decimate [] = []
decimate ls = let (h, t) = splitAt 9 ls in h ++ decimate (drop 1 t)

p8tests = [decimate [1..21] == [1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,21]]


-- Problem 9, encipher
encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher ls0 ls1 [] = []
encipher ls0 ls1 (s:sx) = ls1 !! (head (findIndices (== s) ls0)) : encipher ls0 ls1 sx

p9tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this"]


-- Problem 10, prefixSum
prefixSum :: (Num a) => [a] -> [a]
prefixSum [] = []
prefixSum (x:xs) = helper (x:xs) 0
                   where helper [] acc = []
                         helper (x:xs) acc = (acc + x) : helper xs (acc + x)

p10tests = [prefixSum [1..10] == [1,3,6,10,15,21,28,36,45,55], prefixSum [2, 5] == [2, 7]]


-- Problem 11, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select _ [] [] = []
select pred (x:xs) (y:ys) = if (pred x) 
                            then y : rest 
                            else rest 
                            where rest = select pred xs ys

p11tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]


-- Problem 12, numbers
numbers :: [Int] -> Int
numbers [x] = x
numbers (x:xs) = helper (x:xs) 0
                 where helper [] iter = iter 
                       helper (x:xs) iter = helper xs ((iter * 10) + x)
p12tests = [numbers [1..4] == 1234]


tests = [p1tests,p2tests,p3tests,p35tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests,p12tests]
likelyCorrect = and $ map and tests