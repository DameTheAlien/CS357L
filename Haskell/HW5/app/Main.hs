
studentName = ["Franco", "Damian"]
netID = "dfranco24@unm.edu"

-- Note: you cannot put a = b, where b is some built in function. I.e. no myTakeWhile = takeWhile
-- Problem 1, myTakeWhile
myTakeWhile :: (a-> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (s:sx) = if (pred s) 
                          then s : myTakeWhile pred sx
                          else []

p1tests = [myTakeWhile (/= ' ') "This is practice." == "This"]


-- Problem 2, mySpan
mySpan :: (a->Bool) -> [a] -> ([a],[a])
mySpan _ [] = ([], [])
mySpan pred (s:sx) = if (pred s)
                     then (s : xs, ys)
                     else ([], (s:sx))
                     where (xs, ys) = mySpan pred sx

p2tests = [mySpan (/= ' ') "This is practice." == ("This"," is practice.")]


-- Problem 3, combinations3
combinations3 :: Ord a => [a] -> [[a]]
combinations3 ls = helper (length ls) 3 ls
                   where helper size n (x:xs) = if (n == 0) then [[]]
                                                else if (n >= size) then [(x:xs)]
                                                else if (null (x:xs)) then []
                                                else map (x :) (helper (size - 1) (n - 1) xs) ++ helper (size - 1) n xs


p3tests = [combinations3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]


-- Problem 4, runLengthEncode
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode [] = []
runLengthEncode (x:xs) = helper 1 x xs
                         where helper acc c [] = [(c, acc)]
                               helper acc c (x:xs) = if (x == c)
                                                     then helper (acc + 1) c xs
                                                     else (c, acc) : helper 1 x xs

p4tests = [runLengthEncode [4,2,2,1,1,1,1,4,4,4,4] == [(4,1),(2,2),(1,4),(4,4)], runLengthEncode "foo" == [('f',1),('o',2)]]


-- Problem 5, runLengthDecode
runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode [] = []
runLengthDecode (x:xs) = replicate (snd x) (fst x) ++ runLengthDecode xs

p5tests = [runLengthDecode [(4,1),(2,2),(1,4),(4,4)] == [4,2,2,1,1,1,1,4,4,4,4], (runLengthDecode $ runLengthEncode "foobar") == "foobar"]


-- Problem 6, splitText
splitText :: Ord a => (a -> Bool) -> [a] -> [[a]]
splitText _ [] = []
splitText pred str = helper str pred []
                     where helper [] _ [] = []
                           helper [] _ r = [r]
                           helper (x:xs) c [] = if (c x) 
                                                then helper xs c [x] 
                                                else helper xs c []
                           helper (x:xs) c r = if (c x)
                                               then helper xs c (r ++ [x])
                                               else r : helper xs c []

p6tests = [splitText (/= ' ') "This is practice." == ["This","is","practice."]]


-- Problem 7, encipher
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices pred [] = []
findIndices pred ls = [i | (x, i) <- zip ls [0..], pred x]

encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher ls0 ls1 [] = []
encipher ls0 ls1 (s:sx) = ls1 !! (head (findIndices (== s) ls0)) : encipher ls0 ls1 sx

p7tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this",encipher [1..10] (map (\x -> x*x) [1..10]) [10,9..1] == [100,81,64,49,36,25,16,9,4,1],encipher [10,9..0] [10,9..0] [0..10] == [0,1,2,3,4,5,6,7,8,9,10],encipher (['A','C'..'Z'] ++ ['B','D'..'Z']) [1..26] ['A'..'Z'] == [1,14,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,22,10,23,11,24,12,25,13,26]]


-- Problem 8, goldbach
goldbach :: Int -> [(Int, Int)]
goldbach n = if ((mod n 2) == 0) 
             then [(a, b)| a <- takeWhile (< n) primes, b <- takeWhile (< n) primes, n == a + b && a < b || n == a + b && a == b]
             else []
             where primes = helper [2..]
                            where helper (x:xs) = x : helper [i | i <- xs, (mod i x) > 0]

p8tests = [goldbach 6 == [(3,3)], goldbach 14 == [(3,11),(7,7)]]


-- Problem 9, increasing
increasing :: (Ord a) => [a] -> Bool
increasing [] = False
increasing (x:xs) = if (null xs)
                    then True
                    else (if (x > head xs) then False else increasing xs)

p9tests = [increasing "ABBD", not $ increasing [100,99..1]]


-- Problem 10, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select _ [] [] = []
select pred (x:xs) (y:ys) = if (pred x) 
                            then y : rest 
                            else rest 
                            where rest = select pred xs ys

p10tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]


-- Problem 11, combinations
combinations :: Ord a => Int -> [a] -> [[a]]
combinations n ls = helper (length ls) n ls
                    where helper size n (x:xs) = if (n == 0) then [[]]
                                                 else if (n >= size) then [(x:xs)]
                                                 else if (null (x:xs)) then []
                                                 else map (x :) (helper (size - 1) (n - 1) xs) ++ helper (size - 1) n xs

p11tests = [combinations 3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]


-- Note: Uncomment the pNtests and in tests below and in tests once you have given a definiton for problem 12

-- Problem 12, ComplexInteger, real, imaginary
data ComplexInteger a b = ComplexInteger {real :: a, 
                                          imaginary :: b}

p12tests = [real (ComplexInteger 1 2) == 1, imaginary (ComplexInteger 2 3) == 3]

-- Problem 13, Eq

instance (Eq x, Eq y) => Eq (ComplexInteger x y) where
    (ComplexInteger a b) == (ComplexInteger c d) = a == c && b == d


p13tests = [(ComplexInteger 1 2) /= (ComplexInteger 3 4)]


-- Problem 14, Show
instance (Show a, Show b, Num a, Num b, Eq a, Eq b, Ord a, Ord b) 
          => Show (ComplexInteger a b) where
    show (ComplexInteger x y) = if (x == 0)
                                then (show y) ++ "i"
                                else if (y == 0)
                                then (show x)
                                else if (y < 0) 
                                then (show x) ++ (show y) ++ "i"
                                else (show x) ++ "+" ++ (show y) ++ "i"


p14tests = [(show $ ComplexInteger 1 2) == "1+2i", (show $ ComplexInteger 1 0) == "1", (show $ ComplexInteger 0 1) == "1i"]

-- Problem 15, Num
instance (Num a, Num b, Integral a, Integral b) => Num (ComplexInteger a b) where
   (ComplexInteger x y) + (ComplexInteger u v) = ComplexInteger (x + u) (y + v)
   (ComplexInteger x y) * (ComplexInteger u v) = ComplexInteger ((x * u) - ((fromIntegral (y)) * (fromIntegral (v)))) 
                                                                          (((fromIntegral (x)) * (fromIntegral (v))) 
                                                                         + ((fromIntegral (y)) * (fromIntegral (u))))
  
   fromInteger i = fromIntegral i + 0
   abs j = j
   signum k = k
   negate l = l

p15tests = [(ComplexInteger 1 2) * (ComplexInteger 3 4) == (ComplexInteger (-5) 10)]


tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests]++[p12tests,p13tests,p14tests,p15tests]
likelyCorrect = (and [and t | t <- tests], if length tests < 15 then "lacking ComplexInteger tests?" else "")
