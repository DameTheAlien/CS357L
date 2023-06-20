import Data.List
-- Fill in your name and net ID below
 -- replacing Last with your last name(s)
 -- replacing First with your first name(s)
 -- and replacing netID@unm.edu with your unm email address
studentName = ["Franco", "Damian"]
netID = "dfranco24@unm.edu"

-- If you accept the academic honesty pledge, replace the "" below with
     -- "I accept and agree to the academic honesty pledge"
academicHonestyPledge = "I accept and agree to the academic honesty pledge"


-- Fill out the following defintions in wichever order you choose
 -- your final submission should evaluate without errors.
-- Helpers MUST be defined using where or let syntax
-- Where type signatures are defined, you CAN NOT change them
-- You may use any functions in Prelude or Data.List (imported)

-- Quick comment on this test, this one was 100% more difficult than
-- what was expected for me personally. I put my work that did not make
-- the cut under the undefined functions to, hopefully, get some partial
-- credit. 
-- DID NOT FINISH THE FOLLOWING:
--  - Second Breakfast
--  - All fold tree functions

-- ================= Breakfast ================= 
duplicates :: Eq a => [a] -> Bool
duplicates ls = let nonDupLs = (nub ls) in
                if ((length ls) > (length nonDupLs)) 
                then True
                else False

duplicatesTests = [
                     not $ duplicates [1..10]
                    ,duplicates "ABCAD"
                    ,not $ duplicates "BAD"
                    ,duplicates [1,2,1]
                  ]


-- ============= Second Breakfast ============== 
gaps :: (Enum t, Eq t) => [t] -> Bool
gaps (l:ls) = if (duplicates (l:ls) == True)
              then True
              else helper ls (fromEnum(l) + 1)
                   where helper [] acc = False
                         helper (x:xs) acc = if (fromEnum(x) /= acc)
                                             then True
                                             else helper xs (acc + 1)

gapsTests = [
              not $ gaps [1..10]
             ,not $ gaps "ABCD"
             ,gaps "ABD"
             ,gaps [1,2,3,5,6]
             ,gaps "ABBC"
            ]

-- ============= Second Breakfast ============== 
data Lulz a = Lulz [[a]] deriving (Show, Eq)
lulz0 = Lulz [[1,2],[3,4]]
lulz1 = Lulz [[4,3],[2,1]]
lulz2 = Lulz [[4,3],[8,9],[2,1]]

mapLulz :: (a -> b) -> Lulz a -> Lulz b
mapLulz = undefined
{-
mapLulz _ (Lulz [[]]) = Lulz [[]]
mapLulz f (Lulz [[x]]) = Lulz [[(f x)]] ++ mapLulz (Lulz [[x]]) 
-}

mapLulzTests = [mapLulz even lulz0 == Lulz [[False,True],[False,True]]]

zipLulz :: Lulz a -> Lulz b -> Lulz (a, b)
zipLulz = undefined
{-
zipWithLulz (Lulz [[]]) (Lulz [[]]) = Lulz [[]]
zipWithLulz (Lulz [[x]]) (Lulz [[y]]) = Lulz [[ ([[x : y]]) ++ zipWithLulz (Lulz [[x]])  (Lulz [[y]]) 
-}

zipLulzTests = [
                 zipLulz lulz0 lulz1 == Lulz [[(1,4),(2,3)],[(3,2),(4,1)]]
                ,zipLulz lulz0 lulz2 == Lulz [[(1,4),(2,3)],[(3,8),(4,9)]]
                ,zipLulz lulz2 lulz0 == Lulz [[(4,1),(3,2)],[(8,3),(9,4)]]
                ,zipLulz lulz2 lulz2 == Lulz [[(4,4),(3,3)],[(8,8),(9,9)],[(2,2),(1,1)]]
                ,zipLulz (Lulz ["testing", "strings"]) (Lulz [zip [1..5] ['a'..], zip [2,4..10] ['b','d'..]]) == Lulz [[('t',(1,'a')),('e',(2,'b')),('s',(3,'c')),('t',(4,'d')),('i',(5,'e'))],[('s',(2,'b')),('t',(4,'d')),('r',(6,'f')),('i',(8,'h')),('n',(10,'j'))]]
                ]

zipWithLulz :: (a -> b -> c) -> Lulz a -> Lulz b -> Lulz c
zipWithLulz = undefined
{-
zipWithLulz (Lulz [[]]) (Lulz [[]]) = Lulz [[]]
zipWithLulz (Lulz [[x]]) (Lulz [[y]]) = Lulz [[ ([[x : y]]) ++ zipWithLulz (Lulz [[x]])  (Lulz [[y]]) 
-}

zipWithLulzTests = [
                     zipWithLulz (+) lulz0 lulz1 == Lulz [[5,5],[5,5]]
                    ,zipWithLulz (+) lulz0 lulz2 == Lulz [[5,5],[11,13]]
                    ,zipWithLulz (+) lulz2 lulz0 == Lulz [[5,5],[11,13]]
                    ,zipWithLulz (+) lulz2 lulz2 == Lulz [[8,6],[16,18],[4,2]]
                    , zipWithLulz (\x y -> x : [snd y]) (Lulz ["testing", "strings"]) (Lulz [zip [1..5] ['a'..], zip [2,4..10] ['b','d'..]]) == Lulz [["ta","eb","sc","td","ie"],["sb","td","rf","ih","nj"]]
                   ]

-- ============ The Final Frontier ============= 
data Tree a = Empty | Leaf a | Fork a (Tree a) (Tree a) deriving (Show, Eq)

bar = Fork 0 (Fork 1 (Fork 2 Empty (Leaf 3)) (Leaf 4)) (Fork 5 Empty (Leaf 6))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Fork n xt yt) = Fork (f n) (mapTree f xt) (mapTree f yt)

mapTreeTests = [mapTree (+ 1) bar == Fork 1 (Fork 2 (Fork 3 Empty (Leaf 4)) (Leaf 5)) (Fork 6 Empty (Leaf 7))]

-- list representation of the tree
listTree :: Tree a -> [a]
listTree Empty = []
listTree (Leaf x) = [x]
listTree (Fork n xt yt) = [n] ++ (listTree xt) ++ (listTree yt)

-- you may assume positive Ints only
maxTree :: Tree Int -> Int
maxTree Empty = maximum (listTree (Empty))
maxTree (Leaf x) = maximum (listTree (Leaf x))
maxTree (Fork n xt yt) = maximum (listTree (Fork n xt yt))

maxTreeTests = [maxTree bar == 6]


contents :: Tree a -> [a]
contents Empty = []
contents (Leaf x) = listTree (Leaf x)
contents (Fork n xt yt) = listTree (Fork n xt yt)

contentsTests = [contents bar == [0,1,2,3,4,5,6]]


interior :: Tree a -> [a]
interior Empty = []
interior (Leaf x) = []
interior (Fork n xt yt) = [n] ++ (interior xt) ++ (interior yt)

interiorTests = [interior bar == [0,1,2,5]]


frontier :: Tree a -> [a]
frontier Empty = []
frontier (Leaf x) = [x]
frontier (Fork n xt yt) = (frontier xt) ++ (frontier yt)

frontierTests = [frontier bar == [3,4,6]]


foldTree :: (a -> b) -> (a -> b -> b -> b) -> b -> Tree a -> b
foldTree = undefined
{-
foldTree f g (Empty) = g
foldTree f g (Leaf x) = g (f x)
foldTree f g (Fork n xt yt) = g (f n) (foldTree f g xt) (foldTree f g yt)
-}


-- Note that the following problems MUST be defined in terms of foldTree
--  failure to do so will result in 0s for problems defined by other means

mapTreeFold :: (a -> b) -> Tree a -> Tree b
mapTreeFold = undefined
{-
mapTreeFold f (Fork n xt yt) = foldTree (\x -> (f x)) Fork (Fork n xt yt)
-}

mapTreeFoldTests = [mapTreeFold (+ 1) bar == Fork 1 (Fork 2 (Fork 3 Empty (Leaf 4)) (Leaf 5)) (Fork 6 Empty (Leaf 7))]


-- you may assume positive Ints only
maxTreeFold :: Tree Int -> Int
maxTreeFold = undefined
{-
maxTreeFold (Fork n xt yt) = foldTree (\x -> (listTree x)) maximum (Fork n xt yt)
-}

maxTreeFoldTests = [maxTreeFold bar == 6]


contentsFold :: Tree a -> [a]
contentsFold = undefined
{-
contentsFold (Fork n xt yt) = foldTree (\x -> x) (\x y z -> (x ++ y ++ z)) (Fork n xt yt)
-}

contentsFoldTests = [contentsFold bar == [0,1,2,3,4,5,6]]


interiorFold :: Tree a -> [a]
interiorFold = undefined
{-
interiorFold (Fork n xt yt) = foldTree (\x -> []) (\x y z -> ([z] ++ y ++ z)) (Fork n xt yt)
-}

interiorFoldTests = [interiorFold bar == [0,1,2,5]]


frontierFold :: Tree a -> [a]
frontierFold = undefined
{-
frontierFold (Fork n xt yt) = foldTree (\x -> [x]) (\x y z -> ([] ++ y ++ z)) (Fork n xt yt)
-}

frontierFoldTests = [frontierFold bar == [3,4,6]]



tests = [
         ("duplicates",duplicatesTests)
        ,("gaps",gapsTests)
        ,("mapLulz",mapLulzTests)
        ,("zipLulz",zipLulzTests)
        ,("zipWithLulz",zipWithLulzTests)
        ,("mapTree",mapTreeTests)
        ,("maxTree",maxTreeTests)
        ,("contents",contentsTests)
        ,("interior",interiorTests)
        ,("frontier",frontierTests)
        ,("mapTreeFold",mapTreeFoldTests)
        ,("maxTreeFold",maxTreeFoldTests)
        ,("contentsFold",contentsFoldTests)
        ,("interiorFold",interiorFoldTests)
        ,("frontierFold",frontierFoldTests)
        ]
likelyPass = and $ map (and.snd) tests