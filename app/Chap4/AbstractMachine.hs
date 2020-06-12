module Chap4.AbstractMachine where

data Expr =  Val Int | Add Expr Expr
value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value e = eval e []


{-|FOr example the expression (2+3)+4 is evaluated as follows:

    value (Add(Add(Val 2)(Val 3)(Val 4))
    value (Add(Val 2)(Val3)) + value(Val 4)
    (value(Val 2) + value(Val 3)) + value(Val 4)
    (2 + value(Val 3)) + value(Val 4)
    (2 + 3) + value(Val 4)
    5 + value(Val 4)
    5 + 4
    9

-}

{-| Note that the definition of value fucntion does not specify that the left argument of an addition should be 
evaluated before the right, or more generally, what the next step of evaluation should de at any point. Rather, the order
of evaluation is determined by Haskell. If desired, however, such control information can be made explicity by defining
an abstract machine for expressions. which specifies the step-by-step process of their evaluation.

To this end, we first declare a type of control stacks for the abstract machine, which comprise a list of operations to
be performed by the machine after the current evaluation has been completed.

-}

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c  = exec c n
eval (Add x y) c = eval x (EVAL y:c)

{-|
    If the expression is an integer, it is already evaluated, and we begin executing the control stack.
    If the epxression is addition, we evaluate the first argument, x, placing the operation EVAL y on top of the control
    stack to indicate that the second argument, y, should be evaluated once that of the first argument is completed.
    In turn, we define the function that executes a control stack in the context of an integer argument.
-}
   
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y: c)n = eval y(ADD n : c)
exec (ADD n: c)m = exec c(n + m)

{-|
    That is, if the stack is empty, we return the integer argument as the resilt of the execution.
    If the top of the stack is an operation EVAL y. we evaluate the expression y, placing the instruction ADD n on the
    top of the remaining stack to indicate that the current integr argument, n, should be added together with the result
    of evaluating y once this is completed. And finally, if th etop of th stack is an operation ADD n, evaluation of the
    two arguments of an addition is now complete, and we execute the remaining control stack in the context of
    the sumer of two resulting integer values
    
    Finally we define a funciton that evaluates an expression to an integer, by invoking eval with the given expression
    and the empty control stack:
-}
