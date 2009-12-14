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

logEquiv2 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [bf1 p q r <=> bf2 p q r | p <- [True, False], q <- [True, False], r <- [True, False]] 

de_morgan1 p q = not (p && q) 
de_morgan2 p q = (not p || not q)

formula3 p q = p
formula4 p q = (p <+> q) <+> q

{-
not True <=> False
not False <=> True
logEquiv1 (\ p -> p ==> False) (\ p -> not p)
logEquiv1 (\ p -> p || True) (const True)
logEquiv1 (\ p -> p && False) (const False)
logEquiv1 (\ p -> p || False) id
logEquiv1 (\ p -> p && True) id
logEquiv1 excluded_middle (const True)
logEquiv1 (\ p -> p && not p) (const False)
-}

