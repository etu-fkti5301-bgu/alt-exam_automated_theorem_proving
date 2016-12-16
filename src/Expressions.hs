module Expressions
    ( Expression(..),
      simplify
    ) where

-- Expressions constructor [p. 14]
data Expression = Var String | Const Int | Add Expression Expression | Mul Expression Expression

-- Functions for common simplification [p. 15]
-- Main recursive function
simplify :: Expression -> Expression
simplify (Add exp1 exp2) = simplify' $ Add (simplify exp1) (simplify exp2)
simplify (Mul exp1 exp2) = simplify' $ Mul (simplify exp1) (simplify exp2)
simplify exp = simplify' exp

-- Helper
simplify' :: Expression -> Expression
simplify' (Add (Const a) (Const b)) = Const (a + b)
simplify' (Mul (Const a) (Const b)) = Const (a * b)
simplify' (Add (Const 0) x) = x
simplify' (Add x (Const 0)) = x
simplify' (Mul (Const 0) x) = Const 0
simplify' (Mul x (Const 0)) = Const 0
simplify' (Mul (Const 1) x) = x
simplify' (Mul x (Const 1)) = x
simplify' exp = exp
