{-
 Just trying the explicit declarations of "and" and "or" using Unicode
-}

(¬) True = False
(¬) False = True

True ∨ x = True
False ∨ x = x

True → x = x
False → x = True

True ↔ x = x
False ↔ x = (¬) x

True ⊕ x = (¬) x
False ⊕ x = x
