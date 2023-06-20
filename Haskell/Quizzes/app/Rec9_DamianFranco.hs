studentName = ["Franco", "Damian"]
netID = "dfranco24@unm.edu"

genome = 'A':'G':'C':"T"

-- given a string, which you can assume contains some collection of items from the genome
--      set, insert each item from the genome at each position in the string parameter
insertions :: String -> [String]
insertions [] = []
insertions str = concat [insertAt x str | x <- genome]
                where insertAt :: Char -> String -> [String]
                      insertAt c [] = [[c]] 
                      insertAt c (x:xs) = (c : x : xs) : (map (x :) (insertAt c xs))

insertionsTest = insertions "AT" == ["AAT","AAT","ATA","GAT","AGT","ATG","CAT","ACT","ATC","TAT","ATT","ATT"]


-- given a list of items, give the lists which
--      delete an item at each position in the parameter
deletions :: [a] -> [[a]]
deletions [] = []
deletions (x:xs) = xs : (map (x :) (deletions xs))

deletionsTest = deletions genome == ["GCT","ACT","AGT","AGC"]


-- given a string, which you can assume contains some collection of items from the genome
--      set, replace each item from the genome at each position in the string parameter
substitutions :: String -> [String]
substitutions [] = []
substitutions str = concat [insertAt x str | x <- genome]
                    where insertAt :: Char -> String -> [String]
                          insertAt c [] = [[c]] 
                          insertAt c (x:xs) = if (null xs) then [[c]] else (c : xs) : (map (x :) (insertAt c xs))

substitutionsTest = substitutions genome == ["AGCT","AACT","AGAT","AGCA","GGCT","AGCT","AGGT","AGCG","CGCT","ACCT","AGCT","AGCC","TGCT","ATCT","AGTT","AGCT"]


-- given a list of items, give the lists which
--      transpose two adjacent items at each pair in the parameter
transpositions :: [a] -> [[a]]
transpositions [] = []
transpositions (x:xs) = if (null xs) then [] else ((head xs) : x : (tail xs)) : (map (x :) (transpositions xs))

transpositionsTest = transpositions genome == ["GACT","ACGT","AGTC"] -- AGCT

tests = [insertionsTest, deletionsTest, substitutionsTest, transpositionsTest]
likelyCorrect = (and tests, filter (not.snd) $ zip [1..] tests)