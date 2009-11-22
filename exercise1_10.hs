removeFst :: Int -> [Int] -> [Int]
removeFst n [] = []
removeFst n (x:xs) | n == x = xs
                   | otherwise = x : removeFst n xs
-- like ... removeFst 2 [1,2,3] ---> [1,3]

-- if you wish, here is a generalized version...

removeAll :: Int -> [Int] -> [Int]
removeAll n [] = []
removeAll n (x:xs) | n == x = removeAll n xs
                   | otherwise = x : removeFst n xs
