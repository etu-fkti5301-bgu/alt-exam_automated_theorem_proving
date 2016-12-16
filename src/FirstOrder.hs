module FirstOrder
    ( Term(..)
    , Fol(..)
    , termval
    , holds
  --  , fvt
    --, var
    ) where

import Logic

data Term = Variable String | Fn String [Term] deriving (Show)
data Fol = R String [Term] deriving (Show)


-- Value of a term
termval m@(domain, func, pred) v (Variable x) = v x
termval m@(domain, func, pred) v (Fn f args) = func f (map (termval m v) args)

-- Value of a first-ordered formula
holds m@(domain, func, pred) v False' = False
holds m@(domain, func, pred) v True' = True
holds m@(domain, func, pred) v (Atom(R r args)) = pred r (map (termval m v) args)
holds m@(domain, func, pred) v (Not p) = not (holds m v p)
holds m@(domain, func, pred) v (And p q) = (holds m v p) && (holds m v q)
holds m@(domain, func, pred) v (Or p q) = (holds m v p) || (holds m v q)
holds m@(domain, func, pred) v (Imp p q) = not(holds m v p) || (holds m v q)
holds m@(domain, func, pred) v (Iff p q) = holds m v p == holds m v q

-- there's a need for a |-> function from github of harrison
--holds m@(domain, func, pred) v (Forall x p) = forall (\a -> holds m ((x |-> a) v)) domain
--holds m@(domain, func, pred) v (Exists x p) = exists (\a -> holds m ((x |-> a) v)) domain

-- Set of free variables
--fvt :: Term -> [String]
--fvt _ = 
--fvt (Variable x) = x
--fvt (Fn f args) = map fvt args

-- Get all variables
--var :: (Formula a) -> [String]
--var _ = 
--var True' = []
--var False' = []
--var (Atom(R p args)) = map fvt args
