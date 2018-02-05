----------------------------------------------------
--FILE NAME: A3.hs           |* * * * * |##########|
--NAME: Han Hong             | * * * * *|##########|
--                           |* * * * * |##########|
--CSCE 314 SECTION 500       |#####################|
--UIN: 824000237             |#####################|
----------------------------------------------------

import Data.Char

--Data Types, Type Classes ======================================================
data Tree a b = Branch b (Tree a b) (Tree a b)
			  | Leaf a
			  
instance (Show a, Show b) => Show (Tree a b) where
  show tree =
    let helper n (Leaf a) = (take n $ repeat ' ') ++ show a
        helper n (Branch nd l1 l2) = (take n $ repeat ' ') ++ show nd ++ "\n" ++
                                     (helper (n+2) l1) ++ "\n" ++
                                     (helper (n+2) l2)
    in  helper 0 tree

preorder  :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder leaf branch (Leaf x) = [leaf x]
preorder leaf branch (Branch nd l1 l2) =
  (branch nd) :
  ((preorder leaf branch l1) ++ (preorder leaf branch l2))
  
postorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
postorder leaf branch (Leaf x) = [leaf x]
postorder leaf branch (Branch nd l1 l2) =
  ((postorder leaf branch l1) ++ (postorder leaf branch l2)) ++ [branch nd]
  
inorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
inorder leaf branch (Leaf x) = [leaf x]
inorder leaf branch (Branch nd l1 l2) =
  (inorder leaf branch l1) ++ [branch nd] ++ (inorder leaf branch l2)
  
-- mytree = Branch "A" 
           -- (Branch "B" 
              -- (Leaf 1) 
              -- (Leaf 2)) 
           -- (Leaf 3)
		   
-- A Tiny Language ===============================================================
data E = IntLit Int
       | BoolLit Bool
       | Plus E E
       | Minus E E
       | Multiplies E E
       | Exponentiate E E
       | Equals E E
         deriving (Eq, Show)

program = Equals 
            (Plus (IntLit 1) (IntLit 2))
            (Minus
             (IntLit 5)
             (Minus (IntLit 3) (IntLit 1)))

eval :: E -> E
eval (IntLit x) = IntLit x
eval (BoolLit x) = BoolLit x

eval (Plus (IntLit x) (IntLit y)) = IntLit (x + y)
eval (Minus (IntLit x) (IntLit y)) = IntLit (x - y)
eval (Multiplies (IntLit x) (IntLit y)) = IntLit (x * y)
eval (Exponentiate (IntLit x) (IntLit y)) = IntLit (x ^ y)

eval (Equals (IntLit x) (IntLit y)) = BoolLit (x == y)
eval (Equals (BoolLit x) (BoolLit y)) = BoolLit (x == y)

eval (Plus e1 e2) = eval (Plus (eval e1) (eval e2))
eval (Minus e1 e2) = eval (Minus (eval e1) (eval e2))
eval (Multiplies e1 e2) = eval (Multiplies (eval e1) (eval e2))
eval (Exponentiate e1 e2) = eval (Exponentiate (eval e1) (eval e2))

eval (Equals e1 e2) = eval (Equals (eval e1) (eval e2))

-- ===================================================================================
logi2 :: Int -> Int
logi2 a = floor(logBase (fromIntegral 2) (fromIntegral a))

log2Sim :: E -> E

log2Sim (IntLit x) = IntLit (logi2 x)
log2Sim (BoolLit x) = BoolLit (x)
log2Sim (Exponentiate (IntLit x) (IntLit y)) = Multiplies (IntLit x) (log2Sim (IntLit y))
log2Sim (Multiplies (IntLit x) (IntLit y)) = Plus (log2Sim (IntLit(x))) (log2Sim(IntLit (y)))
log2Sim (Multiplies e1 e2) = Plus (log2Sim e1) (log2Sim e2)
log2Sim (Equals e1 e2) = Equals (log2Sim e1) (log2Sim e2)
