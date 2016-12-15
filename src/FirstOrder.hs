module FirstOrder
    ( Term(..)
    , Fol
    ) where

import Logic

data Term = Var String | Fn String [Term] deriving (Show)
data Fol = R String [Term] deriving (Show)
