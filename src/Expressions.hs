module Expressions
    ( Expression(..)
    ) where

-- Expressions constructor [p. 14]
data Expression = Var String | Const Int | Add Expression Expression | Mul Expression Expression deriving(Show)



