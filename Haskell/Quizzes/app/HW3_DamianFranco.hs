-- Damian Franco
-- CS-558
-- Homework 3
import Data.Char

-- Problem 3.1 (a)
data AltTree a b = ALeaf a 
                 | BLeaf b
                 | Node (AltTree a b) (AltTree a b)
                 deriving (Eq, Show)

tree1 = Node (Node (ALeaf 1) (BLeaf "test 1"))
             (Node (BLeaf "test 2") (Node (ALeaf 2) (ALeaf 3)))

tree2 = Node (Node (ALeaf 10) (BLeaf True))
             (Node (Node (BLeaf False) (BLeaf True)) 
                   (Node (ALeaf 40) (ALeaf 50)))

tree3 = Node (Node (ALeaf 100) (BLeaf 'a'))
             (Node (BLeaf 'b') 
                   (Node (ALeaf 200) 
                         (Node (BLeaf 'c') (BLeaf 'd'))))

--Problem 3.1 (b)
altTreeMap :: (a -> c) -> (b -> d) -> AltTree a b -> AltTree c d
altTreeMap f _ (ALeaf a) = ALeaf (f a)
altTreeMap _ g (BLeaf b) = BLeaf (g b)
altTreeMap f g (Node a b) = Node (altTreeMap f g a) (altTreeMap f g b)

mapTest1 = altTreeMap (\x -> x + 1) (\y -> y ++ " DONE") tree1 
mapTest2 = altTreeMap (\x -> x / 2) (\y -> not y) tree2
mapTest3 = altTreeMap (\x -> x * 2) (\y -> toUpper y) tree3

--Problem 3.1 (c)
altTreeFold:: (a -> c) -> (b -> c) -> (c -> c -> c) -> AltTree a b -> c
altTreeFold f _ fNode (ALeaf a) = f a
altTreeFold _ g fNode (BLeaf b) = g b
altTreeFold f g fNode (Node a b) = fNode (altTreeFold f g fNode a) (altTreeFold f g fNode b)

foldTest1 = altTreeFold (\x -> (show x) ++ " ") (\y -> y ++ " AND ") (++) tree1 
foldTest2 = altTreeFold (\x -> x * 2) (\y -> if True then 1000 else 1) (+) tree2
foldTest3 = altTreeFold (\x -> x:[]) (\y -> (fromEnum y):[]) (++) tree3 