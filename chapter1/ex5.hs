divides d n = rem n d == 0
ld n = ldf 2 n 
ldf k n | divides k n = k 
        | k^2 >  n    = n 
        | otherwise   = ldf (k+1) n
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False 
         | otherwise = ld n == n
         
{-
  Take a look at:
  http://primes.utm.edu/notes/by_year.html
  
  Try out some primes discovered a long time ago based on Mersenne numbers, like
  prime0 (2 ^ 17 - 1) -- by Pietro Cataldi 1588
  prime0 (2 ^ 19 - 1) -- by Pietro Cataldi 1588
  prime0 (2 ^ 31 - 1) -- by Euler 1772 
  prime0 (2 ^ 127 - 1) -- by Lucas 1876 (mothafucka hum? if your PC makes it...) 
  Hey, this is the largest known Prime (2009 Nov 22):
  prime0 (2 ^ 43112609 - 1) by E_Smith, Woltman, Kurowski, et al. [GIMPS, PrimeNet] 2008
-}
