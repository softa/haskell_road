b :: Int -> String -> String
b _ [] = []
b n (x:xs) = (replicate n x) ++ (b (n+1) xs)

blowup :: String -> String
blowup s = b 1 s
              
{-
  example
  count "bang!"
  ---> "baannngggg!!!!!"
-}



