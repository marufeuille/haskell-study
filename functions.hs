-- The 'find' element from list function.
find :: Eq a => [a] -> a -> Maybe a
find [] _ = Nothing
find (x:xs) n
    | x == n = Just x
    | otherwise = find xs n

-- simple qsort function
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = sort ([ x' | x' <- xs, x' < x]) ++ x: [ x' | x' <- xs, x' == x] ++ sort [ x' | x' <- xs, x' > x]


-- 8-queen solver
-- check if queen is able to place this place
check' :: [Int] -> Int -> Bool
check' pasts x
    | x `elem` pasts = False
    | all (==False) existsOnPath = True
    | otherwise = False
    where
        len = length pasts
        existsOnPath = [p' == x - diff || p' == x + diff |(p', diff) <- zip pasts (reverse [1..len])]

-- 8-queen solver entry point
solveEightQueenProblem :: [Int] -> [[Int]]
solveEightQueenProblem pasts
    | row == 8 = [pasts]
    | null nextCandidates = []
    | otherwise = concat [solveEightQueenProblem next  | next <- nexts]
    where
        row = length pasts
        nextCandidates = filter (check' pasts) [0..7]
        nexts = [pasts ++ [candidate] |candidate <- nextCandidates]

-- knapsack problem solver 
solveKnapsackProblem :: Int -> [Int] -> [Int] -> Int
solveKnapsackProblem currentCapacity (weight:weights) (value:values)
    | length weights /= length values = error "invalid array length"
    | currentCapacity < weight = solveKnapsackProblem currentCapacity weights values
    | otherwise = max (solveKnapsackProblem currentCapacity weights values) (value + solveKnapsackProblem (currentCapacity - weight) weights values)
solveKnapsackProblem currentCapacity [] [] = 0
solveKnapsackProblem 0 _ _ = 0

-- subset sum problem solver
solveSubsetSumProblem :: Int -> [Int]  -> Bool 
solveSubsetSumProblem remainTotal (value:values)
    | remainTotal < value = solveSubsetSumProblem remainTotal values
    | otherwise = solveSubsetSumProblem (remainTotal - value) values || solveSubsetSumProblem remainTotal values
solveSubsetSumProblem 0 _ = True
solveSubsetSumProblem _ [] = False

