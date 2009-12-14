lengths :: [[a]] -> [Int]
lengths l = map length l
{-
  This is my version __without__ map
  I just couldn't figure out how to do it with map, and I'm ready to accept it is impossible,
  since map always will produce a list.
  Based on his solution i think this guy is also confused about it:
  http://sdasrath.blogspot.com/2009/05/20090510-haskell-road-to-logic-math-and.html
  Look as ex21b.hs for the solution that is in the book's website, using sum (cheaters!). 
-}
sumLengths :: [[a]] -> Int
sumLengths [] = 0
sumLengths (x:xs) = length x + sumLengths xs
