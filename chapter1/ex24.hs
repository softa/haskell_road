{-
Nothing happens, because it curries ("apply a curry") the function.
Another example is this:
inc n = (+1) n
This does the same job:
inc = (+1)
-}

ldp :: Integer -> Integer
-- ldp n = ldpf primes1 n
ldp = ldpf primes1

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0  = p
              | p ^ 2 > n     = n
              | otherwise     = ldpf ps n
              
primes1 :: [Integer]
primes1 = 2 : filter prime [3..]
prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n

