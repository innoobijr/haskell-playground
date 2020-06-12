module Chap4.Tautology where

import Data.Algebra.Boolean

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A')(Not (Var 'A'))

p2 :: Prop
p2 = Imply(And(Var 'A')(Var 'B'))(Var 'A')

p3 :: Prop
p3 = Imply(Var 'A')(And(Var 'A')(Var 'B'))

p4 :: Prop
p4 = Imply(And(Var 'A')(Imply(Var 'A')(Var 'B')))(Var 'B')


{-|
    In order to evaluate a proposition to a logical value, we need to know the value of each of its variables.
    For this purpose, we declare a substitution as a lookup table that associates variable names to logical values, 
    using the Assoc type that was introduced at the start of this chapter. 
-}
type Subst = Assoc Char Bool

{-|
    A funciton that evsaluat as propositio given a substitutions for its variables can now be defined by pattern matching
    on five possible forms that the proposition can have
-}

eval               :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = Data.Algebra.Boolean.not (eval s p)
eval s (And p q)   = (Data.Algebra.Boolean.&&)(eval s p)(eval s q)
eval s (Imply p q) = (-->)(eval s p)(eval s q)

{-|
    To decide if a proposiiton is a tautology, we will consider all possible substitutions for the variables
    that it contains. First of all, we defined a function that retruns the variables in a proposition:
    
-}
 
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p 
vars (And p q) = vars p ++ vars q
vars(Imply p q) = vars p ++ vars q


bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map(False:)bss ++ map(True:)bss
               where bss = bools (n - 1)

rmdups  :: Eq a => [a] -> [a]
rmdups[] = []
rmdups(x:xs) = x:rmdups(filter(/= x)xs)

substs :: Prop -> [Subst]
substs p = map(zip vs)(bools (length vs))
              where vs = rmdups(vars p)

isTaut :: Prop -> Bool
isTaut p = Data.Algebra.Boolean.and[eval s p | s <- substs p]

isTautList :: Prop -> [Bool]
isTautList p = [eval s p | s <- substs p]
