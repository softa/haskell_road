infixr 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

infixr 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y =  x == y

infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y =  x /= y

valid1 :: (Bool -> Bool) -> Bool
-- valid1 bf = (bf True) && (bf False)
-- Done right!
valid1 bf = and [bf p | p <- [True, False]]

excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

valid2 :: (Bool -> Bool -> Bool) -> Bool
-- valid2 bf = (bf True True) && (bf True False) && (bf False True) && (bf False False)
-- Done right!
valid2 bf = and [bf p q | p <- [True, False], q <- [True,False]]
valid3 bf = and [bf p q r | p <- [True, False], q <- [True,False], r <- [True,False]]
valid4 bf = and [bf p q r s | p <- [True, False], q <- [True,False], r <- [True,False], s <- [True,False]]

form1 p q = p ==> (q ==> p)
form2 p q = (p ==> q) ==> p
complete_de_morgan p q = not (p && q) <=> (not p || not q)

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [bf1 p q <=> bf2 p q | p <- [True, False], q <- [True, False]] 

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [bf1 p q r <=> bf2 p q r | p <- [True, False], q <- [True, False], r <- [True, False]] 

de_morgan1 p q = not (p && q) 
de_morgan2 p q = (not p || not q)

formula3 p q = p
formula4 p q = (p <+> q) <+> q























truth_table1 :: (Bool -> Bool) -> [Bool]
truth_table1 f = [f p | p <- [True,False]]

{-
*Main> truth_table1 (\p -> p)
[True,False]
*Main> truth_table1 (\p -> not p)
[False,True]
*Main> truth_table1 (\p -> p || not p)
[True,True]
*Main> truth_table1 (\p -> p && not p)
[False,False]
-}

truth_table2 :: (Bool -> Bool -> Bool) -> [Bool]
truth_table2 f = [f p q | p <- [True,False], q <- [True,False]]

{-


1.
*Main> (\p q -> p || q ==> p) True True    
True
*Main> (\p q -> p || q ==> p) True False
True
*Main> (\p q -> p || q ==> p) False True
False
*Main> (\p q -> p || q ==> p) False False
True
MORE PROOF HERE:
  *Main> truth_table2 (\p q -> p || q ==> p)
  [True,True,False,True]
  
2. 
If we have two sentencial letters, we have 2² truth "rows". So if we have 4 rows, we have 4² combinations of values, that is, being _really_ explicit, 16. Look:
*Main> [[p,q,r,s] | p <- [True, False], q <- [True, False], r <- [True, False], s <- [True, False]]
[[True,True,True,True],[True,True,True,False],[True,True,False,True],[True,True,False,False],[True,False,True,True],[True,False,True,False],[True,False,False,True],[True,False,False,False],[False,True,True,True],[False,True,True,False],[False,True,False,True],[False,True,False,False],[False,False,True,True],[False,False,True,False],[False,False,False,True],[False,False,False,False]]
*Main> length [[p,q,r,s] | p <- [True, False], q <- [True, False], r <- [True, False], s <- [True, False]]
16
See???

3.Yeah, I can! I'm just looking to e_ou.hs and figuring out:
  1. [True,True,True,True] - or, a Tautology!
    *Main> truth_table2 (\p q -> p || not p)
    [True,True,True,True]
  2. [True,True,True,False]
    *Main> truth_table2 (\p q -> p || q)
    [True,True,True,False]
  3. [True,True,False,True]
    *Main> truth_table2 (\p q -> p || q ==> p)
    [True,True,False,True]
  4.  [True,False,True,True]
    *Main> truth_table2 (\p q -> p ==> q) 
    [True,False,True,True]
  5. [True,False,True,False]
    ?
    
  6. [True,False,False,True]
    *Main> truth_table2 (\p q ->  p <+> not q)    
    [True,False,False,True]
    *Main> truth_table2 (\p q -> p <=> q)
    [True,False,False,True]
        
  7. [True,False,False,False]
    *Main> truth_table2 (\p q -> p && q)    
    [True,False,False,False]

  8. [False,True,True,True]
    *Main> truth_table2 (\p q -> (p || q) <=> q)
    [True,False,True,True]

  9. [False,True,True,False]
    *Main> truth_table2 (\p q -> p <=> q)
    [True,False,True,True]
  
  10. [False,True,False,True]

  11. [False,True,False,False]
  
  12; [False,False,True,True]
  
  14. [False,False,True,False]
  
  15. [False,False,False,True]
  
  16. [False,False,False,False]
  


         

-}
