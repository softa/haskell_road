infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y =  x /= y

{-
Results:
*Main> True <+> True
False
*Main> True <+> False
True
*Main> False <+> True    
True
*Main> False <+> False
False

This prooves ex2.txt
-}
