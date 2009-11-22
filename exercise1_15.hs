minS :: String -> Char
minS [x] = x
minS (x:xs) = min x (minS xs)

strString :: String -> String
strString [] = ""
strString [x] = [x]
strString (x:xs) | x <= minS(xs) = x : (strString xs)
                 | otherwise = strString (xs ++ [x])

