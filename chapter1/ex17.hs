prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = x==y && prefix xs ys

subs :: String -> String -> Bool
subs [] ys = True
subs xs [] = False
subs xs (y:ys) | prefix xs (y:ys) = True
               | otherwise = (subs xs ys)


