b :: Int -> String -> String
b _ [] = []
b n (x:xs) = (replicate n x) ++ (b (n+1) xs)

blowup :: String -> String
blowup s = b 1 s

-- blowup :: String -> String
-- blowup [] = ""
-- blowup [x] = [x]
-- blowup (x:xs) = [x] ++ (blowup xs) ++ [l(xs)] 
-- blowup (x:xs) = (times [x] (length xs + 1))  ++ (blowup xs)
               
{-
  example
  count "bang!"
  ---> "baannngggg!!!!!"
-}



