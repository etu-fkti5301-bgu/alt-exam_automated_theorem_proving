module Printer(
    printExpression
    ) where

import Expressions

printExpression :: Expression -> String
printExpression e = expression 0 e

instance Show Expression where
    show = printExpression

expression :: (Ord n, Num n) => n -> Expression -> String
expression _ (Var e) = e
expression _ (Const n) = show n
expression p (Add e1 e2)
    | 2 < p = "(" ++ s ++ ")"
    | otherwise = s
    where
        s = expression 3 e1 ++ " + " ++ expression 2 e2
expression p (Mul e1 e2)
    | 4 < p = "(" ++ s ++ ")"
    | otherwise = s
    where
        s = expression 5 e1 ++ " * " ++ expression 4 e2
