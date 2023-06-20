import Data.List

studentName = ["Franco", "Damian"]
netID = "dfranco24@unm.edu"


-- Problem 1, bits2num
bits2num :: Num a => [Char] -> a
bits2num ls = helper (reverse ls) 0
              where helper [] _ = 0
                    helper (x:xs) acc = if ((fromEnum x) == 48)
                                        then 0 * (2^acc) + helper xs (acc + 1)
                                        else 1 * (2^acc) + helper xs (acc + 1)

p1tests = [bits2num "1011000" == 88]


-- Problem 2, num2bits
num2bits :: Integral a => a -> [Char]
num2bits 0 = []
num2bits n = '0' : reverse (helper n) 
             where helper 0 = []
                   helper n = if (n < 0)
                              then []
                              else if ((mod n 2) == 0)
                              then '0' : helper (div n 2)
                              else '1' : helper (div n 2)

p2tests = [num2bits 87783 == "010101011011100111"]


-- Problem 3, variance
variance :: (Num a, Fractional a) => [a] -> a
-- variance = undefined
variance xs = (sum [(x - y)^2 | x <- xs, y <- [(sum xs / (len xs 0))]]) / (len xs 0)
              where len [] acc = acc
                    len (l:ls) acc = len ls (acc + 1) 

p3tests = [variance [1..10] == 8.25]

-- Problem 4, difference
difference :: Eq a => [a] -> [a] -> [a]
difference [] _ = []
difference _ [] = []
difference xs ys = (nub xs) \\ (nub ys)

p4tests = [difference "ABCD" "AD" == "BC", difference "ABCDCBA" "AD" == "BC"]


-- Problem 5, splits
combinations :: Ord a => Int -> [a] -> [[a]]
combinations n ls = helper (length ls) n ls
                    where helper size n (x:xs) = if (n == 0) then [[]]
                                                 else if (n >= size) then [(x:xs)]
                                                 else if (null (x:xs)) then []
                                                 else map (x :) (helper (size - 1) (n - 1) xs) ++ helper (size - 1) n xs
splits ::  Ord a => [a] -> [([a], [a])]
splits xs = helper ((length xs) - 1) 1 xs
            where helper 0 _ xs = []
                  helper n k xs = (zip (reverse (combinations k xs)) (combinations n xs)) ++ (helper (n - 1) (k + 1) xs)

p5tests = [sort (splits "abc") == sort [("c","ab"),("b","ac"),("bc","a"),("a","bc"),("ac","b"),("ab","c")]]


-- Problem 6, argmin
argmin ::  (Ord a) => (t -> a) -> [t] -> t
argmin f (x:xs) = helper f (x:xs) x
                where helper f [] ans = ans
                      helper f (y:ys) ans = if ((f ans) < (f y))
                                            then helper f ys ans
                                            else helper f ys y

p6tests = [argmin length ["ABC","EF","GHIJ","K"] == "K"]


data Htree a = HLeaf Double a | HFork Double [a] (Htree a) (Htree a) deriving (Show, Eq)
-- Problem 7, bogus

instance (Ord a) => Ord (Htree a) where
    (HLeaf x _) < (HLeaf y _) = x < y
    (HLeaf x _) < (HFork y _ _ _) = x < y
    (HFork x _ _ _) < (HLeaf y _) = x < y
    (HFork x _ _ _) < (HFork y _ _ _) = x < y
    (HLeaf x _) <= (HLeaf y _) = x <= y
    (HLeaf x _) <= (HFork y _ _ _) = x <= y
    (HFork x _ _ _) <= (HLeaf y _) = x <= y
    (HFork x _ _ _) <= (HFork y _ _ _) = x <= y

-- encode character using Huffman coding tree
encode (HFork _ _ (HLeaf _ l) (HLeaf _ r)) c = if c == l then "0" else "1"
encode (HFork _ _ (HLeaf _ l) v@(HFork _ rs _ _)) c =
    if c == l then "0" else '1':(encode v c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HLeaf _ r)) c =
    if c == r then "1" else '0':(encode u c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HFork _ rs _ _)) c =
    if c `elem` ls then '0':(encode u c) else '1':(encode v c)

-- decode message using Huffman coding tree
decode t [] = []
decode t (x:xs) = loop t (x:xs)
    where loop (HLeaf _ l) xs = l:(decode t xs)
          loop (HFork _ _ u v) ('0':xs) = loop u xs
          loop (HFork _ _ u v) ('1':xs) = loop v xs

merge [] ys = ys
merge xs [] = xs
merge u@(x:xs) v@(y:ys) = if x < y then x:(merge xs v) else y:(merge u ys)

bogus :: Ord a => [(Double, a)] -> Htree a
bogus (x:xs) = HFork (fst x)[(snd x)] (HLeaf (fst x) (snd x)) (HLeaf (fst x) (snd x))

p7tests = let xs = [(0.30,'e'), (0.14,'h'), (0.1,'l'), (0.16,'o'), (0.05,'p'), (0.23,'t'), (0.02,'w')] in 
                [(decode (bogus xs) $ concatMap (encode (bogus xs)) "hello") == "hello", 
                 concatMap (encode (bogus xs)) "hello" /= concatMap (encode (bogus xs)) "oellh"]


-- Problem 8, church
church :: Int -> (a -> a) -> a -> a
church = (foldr (.) id .) . (. repeat) . take

p8tests = [church 4 tail "ABCDEFGH" == "EFGH", church 100 id 9001 == 9001]


data Btree a = BLeaf a | BFork (Btree a) (Btree a) deriving (Show, Eq, Ord)
-- Problem 9, trees
makeBtree :: [a] -> Btree a
makeBtree [x] = (BLeaf x)
makeBtree xs = BFork (makeBtree (take m xs)) (makeBtree (drop m xs))
               where m = (length xs) `div` 2

trees :: (Ord t) => [t] -> [Btree t]
trees xs = helper (permutations xs)
           where helper [] = []
                 helper (x:xs) = (makeBtree x) : (helper xs)

p9tests = [(sort $ trees "ABCDE") !! 114 == BFork (BLeaf 'A') (BFork (BFork (BFork (BLeaf 'E') (BLeaf 'B')) (BLeaf 'C')) (BLeaf 'D')),
           length (trees [0..4]) == 1680]

bases = "AGCT"
-- Problem 10, insertions
insertions :: String -> [String]
insertions [] = []
insertions str = concat [insertAt x str | x <- bases]
                where insertAt :: Char -> String -> [String]
                      insertAt c [] = [[c]] 
                      insertAt c (x:xs) = (c : x : xs) : (map (x :) (insertAt c xs))

p10tests = [insertions "GC" == ["AGC","GAC","GCA","GGC","GGC","GCG","CGC","GCC","GCC","TGC","GTC","GCT"]]


-- Problem 11, deletions
deletions :: [a] -> [[a]]
deletions [] = []
deletions (x:xs) = xs : (map (x :) (deletions xs))

p11tests = [deletions "AGCT" == ["GCT","ACT","AGT","AGC"]]


-- Problem 12, substitutions
substitutions :: String -> [String]
substitutions [] = []
substitutions str = concat [insertAt x str | x <- bases]
                    where insertAt :: Char -> String -> [String]
                          insertAt c [] = [[c]] 
                          insertAt c (x:xs) = if (null xs) then [[c]] else (c : xs) : (map (x :) (insertAt c xs))

p12tests = [substitutions "ACT" == ["ACT","AAT","ACA","GCT","AGT","ACG","CCT","ACT","ACC","TCT","ATT","ACT"]]


-- Problem 13, transpositions
transpositions :: [a] -> [[a]]
transpositions [] = []
transpositions (x:xs) = if (null xs) 
                        then [] 
                        else ((head xs) : x : (tail xs)) : (map (x :) (transpositions xs))

p13tests = [transpositions "GATC" == ["AGTC","GTAC","GACT"]]


tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests,p12tests,p13tests]
likelyCorrect = let results = [and t | t <- tests] in (and results, filter (not.snd) $ zip [1..] results)