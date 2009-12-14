count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | x == c = 1 + (count c xs)
               | otherwise = (count c xs)
               
{-
  example
  count 'a' "aaa"
-}



