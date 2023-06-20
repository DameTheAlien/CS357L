-- Damian Franco
-- CS-558
-- Homework 5

-- LOOK HERE https://www.cs.princeton.edu/~dpw/cos441-11/notes/slides15-lambda-proofs.pdf

-- Problem 5.1 (a)
data Lam = Var String
         | Abs String Lam
         | App Lam Lam
         derving Show

testData = Abs "x" (Var "x")
tru = Abs "t" (Abs "f" (Var "t"))
fls = Abs "t" (Abs "f" (Var "f"))

-- Problem 5.1 (b)
subst :: Lam -> String -> String -> Lam
subst (Var y) x v = if x == y then v else Var y
subst (Abs y e) x v = if x == y then (Abs y e) else (Abs y (subst e x v))
subst (App e1 e2) x v = App (subst e1 x v) (subst e2 x v)

y = Var "y"
x = Var "x"
id = Abs "x" x
foo = App (Abs "y" y) y
subst foo "y" id == App (Abs "y" y) id

-- Problem 5.1 (c)
isValue :: Lam -> Boolean
isValue (Var s) = False
isValue (Abs x e) = True
isValue (App e1 e2) = False

--Problem 5.1 (d)
eval1 :: Lam -> Lam
-- beta rule
eval1 (App (Abs x e) v) | value v = subst e x v
-- app2 rule
eval1 (App v e2) | value v = let e2' = eval e2 in App v e2'
-- app1 rule
eval1 (App e1 e2) = let e1' = eval e1 in App e1' e2
-- forms that don't match LHS; no rule exists
eval1 (Abs x e) = error "Value!"
eval1 (Var x) = error "Stuck!"