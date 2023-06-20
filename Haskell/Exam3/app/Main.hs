
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



-- =============== Come Together =============== 
futile :: (a -> Bool) -> (a -> a) -> a -> a
futile p f x = if (p x)
               then x
               else futile p f (f x)

futileTest = [futile (> 100) (* 2) 1 == 128, 
              futile ((== ' ') . head) tail "This is the end." == " is the end."]


-- =========== Tomorrow Never Knows ============ 
divide :: [a] -> Int -> [[a]]
divide [] n = []
divide ls n = if (mod (length (take n ls)) n == 0) 
              then take n ls : divide (drop n ls) n
              else []

divideTest = [divide ['A'..'Z'] 7 == ["ABCDEFG","HIJKLMN","OPQRSTU"],
              divide [0,3,5,7] 2 == [[0,3],[5,7]]]


-- ============= A Day in the Life ============= 
binaries :: Int -> [String]
binaries n = sequence (replicate n "01")

binariesTest = [binaries 1 == ["0","1"],
                binaries 2 == ["00","01","10","11"],
                binaries 3 == ["000","001","010","011","100","101","110","111"]]


-- ======== Within You and Without You ========= 
setEqual :: (Eq a, Ord a) => [a] -> [a] -> Bool
setEqual xs ys = let sortedx = sort xs
                     sortedy = sort ys 
                     in
                     if (sortedx == sortedy)
                     then True
                     else False 


setEqualTest = [not ("ABCD" `setEqual` "ABC"),
                not ("ABC" `setEqual` "ABCD"),
               [0,2,1,3] `setEqual` [0,1,2,3]]


-- ============ Here Comes the Sun ============= 
increasing :: [[a]] -> [[a]]
increasing [] = []
increasing (x:xs) = sortBy comp (x:xs)
                    where comp a b = if ((length a) > (length b))
                                     then GT
                                     else LT


increasingTest = [increasing ["Ever", "wondered", "how", "many", "dinosaurs", "ever", "roamed", "the", "Earth"] == ["how","the","Ever","many","ever","Earth","roamed","wondered","dinosaurs"],
                  increasing ["Precambrian", "Cambrian", "Ordovician", "Silurian"] == ["Cambrian","Silurian","Ordovician","Precambrian"],
                  increasing ["a", "1", "22", "bb"] == ["a","1","22","bb"]]


tests = [("futile",futileTest),(" divide", divideTest),(" binaries", binariesTest),(" setEqual", setEqualTest), ("increasing",increasingTest)]
likelyPass = and $ map (and.snd) tests
