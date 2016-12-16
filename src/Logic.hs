module Logic
    ( Formula(..)
    , Proposition(..)
    , pname
    , onAtoms
    , overAtoms
    , evaluate
    , makeAnd
    , destructAnd
    , conjuncts
    , makeOr
    , destructOr
    , disjuncts
    , makeImp
    , destructImp
    , antecedent
    , consequent
    , makeIff
    , destructIff
    , makeForall
    , makeExists
    , onAllValuations
    , atoms
    , atomUnion
    , tautology
    , satisfiable
    , unsatisfiable
    , psimplify
    , positive
    , negative
    , dual
    ) where

import Utils
import Data.List

data Formula a =
      False'
    | True'
    | Atom a
    | Not (Formula a)
    | And (Formula a) (Formula a)
    | Or (Formula a) (Formula a)
    | Imp (Formula a) (Formula a)
    | Iff (Formula a) (Formula a)
    | Forall String (Formula a)
    | Exists String (Formula a)
    deriving (Show)

data Proposition = Proposition String

-- Return proposition's name
pname :: Proposition -> String
pname (Proposition string) = string

-- Apply given function to atoms taken
onAtoms :: (a -> Formula a) -> (Formula a) -> (Formula a)
onAtoms function (Atom x) = function x
onAtoms function (Not formula) = (Not $ onAtoms function formula)
onAtoms function (And p q) = (And (onAtoms function p) (onAtoms function q))
onAtoms function (Or p q) = (Or (onAtoms function p) (onAtoms function q))
onAtoms function (Imp p q) = (Imp (onAtoms function p) (onAtoms function q))
onAtoms function (Iff p q) = (Iff (onAtoms function p) (onAtoms function q))
onAtoms function (Forall symbol formula) = (Forall symbol $ onAtoms function formula)
onAtoms function (Exists symbol formula) = (Exists symbol $ onAtoms function formula)
onAtoms _ formula = formula

-- Apply binary function to atoms taken
overAtoms :: (a -> t -> t) -> (Formula a) -> t -> t
overAtoms function (Atom x) operand = function x operand
overAtoms function (Not formula) operand = overAtoms function formula operand
overAtoms function (And p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Or p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Imp p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Iff p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Forall name formula) operand = overAtoms function formula operand
overAtoms function (Exists name formula) operand = overAtoms function formula operand
overAtoms function _ operand = operand

-- Evaluate atom expressions
evaluate :: (Formula a) -> (a -> Bool) -> Bool
evaluate False' _ = False
evaluate True' _ = True
evaluate (Atom x) valuation = valuation x
evaluate (Not formula) valuation = not $ evaluate formula valuation
evaluate (And p q) valuation = evaluate p valuation && evaluate q valuation
evaluate (Or p q) valuation = evaluate p valuation || evaluate q valuation
evaluate (Imp p q) valuation = not (evaluate p valuation) || evaluate q valuation
evaluate (Iff p q) valuation = evaluate p valuation == evaluate q valuation

-- Make AND expression with given arguments
makeAnd :: (Formula a) -> (Formula a) -> (Formula a)
makeAnd p q = (And p q)

-- Return arguments pair of AND expression
destructAnd :: (Formula a) -> ((Formula a), (Formula a))
destructAnd (And p q) = (p, q)
destructAnd _ = error "destructAnd"

-- Return conjuncts list of AND expression
conjuncts :: (Formula a) -> [(Formula a)]
conjuncts (And p q) = conjuncts p ++ conjuncts q
conjuncts conjunct = [conjunct]

-- Make AND expression with given arguments
makeOr :: (Formula a) -> (Formula a) -> (Formula a)
makeOr p q = (Or p q)

-- Return parguments pair of OR expression
destructOr :: (Formula a) -> ((Formula a), (Formula a))
destructOr (Or p q) = (p, q)
destructOr _ = error "destructOr"

-- Return disjuncts list of OR expression
disjuncts :: (Formula a) -> [(Formula a)]
disjuncts (Or p q) = disjuncts p ++ disjuncts q
disjuncts disjunct = [disjunct]

-- Make IMPLY expression with given arguments
makeImp :: (Formula a) -> (Formula a) -> (Formula a)
makeImp p q = (Imp p q)

-- Return arguments pair of IMPLY expression
destructImp :: (Formula a) -> ((Formula a), (Formula a))
destructImp (Imp p q) = (p, q)
destructImp _ = error "destructImp"

-- Return antecedent of IMPLY expression
antecedent :: (Formula a) -> (Formula a)
antecedent formula = fst $ destructImp(formula)

-- Return consequent of IMPLY expression
consequent :: (Formula a) -> (Formula a)
consequent formula = snd $ destructImp(formula)

-- Make EQUIVALENCE expression with given arguments
makeIff :: (Formula a) -> (Formula a) -> (Formula a)
makeIff p q = (Iff p q)

-- Return arguments pair of EQUIVALENCE expression
destructIff :: (Formula a) -> ((Formula a), (Formula a))
destructIff (Iff p q) = (p, q)
destructIff _ = error "destructIff"

-- Make FORALL predicate with given name and expression
makeForall :: String -> (Formula a) -> (Formula a)
makeForall name formula = (Forall name formula)

-- Make EXISTS predicate with given name and expression
makeExists :: String -> (Formula a) -> (Formula a)
makeExists name formula = (Exists name formula)

-- Get list of atoms
atoms formula = atomUnion (\atom -> [atom]) formula

-- Collect atoms by some attribute set by function
atomUnion function formula = overAtoms (\p q -> function p `union` q) formula []

-- 
onAllValuations :: (Eq a) => ((a -> Bool) -> Bool) -> (a -> Bool) -> [a] -> Bool
onAllValuations subfn v [] = subfn v
onAllValuations subfn v (x:xs) = let v' t q = if q==x then t else v q in
                                   onAllValuations subfn (v' False) xs &&
                                   onAllValuations subfn (v' True) xs

-- Checks if formula is a tautology
tautology :: (Eq a) => (Formula a) -> Bool
tautology fm = onAllValuations (evaluate fm) (\x -> False) (atoms fm)

-- Checks if formula is unsatisfiable i.e. it's always false
unsatisfiable :: (Eq a) => (Formula a) -> Bool
unsatisfiable fm = tautology (Not fm)

-- Checks if formula is sastisfiable i.e. it's true at least with one valuation
satisfiable :: (Eq a) => (Formula a) -> Bool
satisfiable fm = not (unsatisfiable fm)

-- Returns dual formula
dual :: Formula a -> Formula a
dual False' = True'
dual True' = False'
dual (Atom a) = (Atom a)
dual (Not a) = (Not (dual a))
dual (And p q) = (Or (dual p) (dual q))
dual (Or p q) = (And (dual p) (dual q))
dual _ = error "dual: Formula involves IMP or IFF"

-- Simplify formulas
-- Test 1: Imp (Imp (True') (Iff (Atom 'x') (False'))) (Not (Or (Atom 'y') (And (False') (Atom 'z'))))
-- Exprected result of test 1: Imp (Not (Atom 'x')) (Not (Atom 'y'))
-- Test 2: Or (Imp (Imp (Atom 'x') (Atom 'y')) (True')) (Not (False'))
-- Exprected result of test 2: True'
psimplify :: Formula a -> Formula a
psimplify (Not atom) = psimplify' (Not (psimplify atom))
psimplify (And p q) = psimplify' (And (psimplify p) (psimplify q))
psimplify (Or p q) = psimplify' (Or (psimplify p) (psimplify q))
psimplify (Imp p q) = psimplify' (Imp (psimplify p) (psimplify q))
psimplify (Iff p q) = psimplify' (Iff (psimplify p) (psimplify q))
psimplify atom = atom

-- Simplifying formulas helper
psimplify' :: Formula a -> Formula a
psimplify' (Not False') = True'
psimplify' (Not True') = False'
psimplify' (Not (Not atom)) = atom
psimplify' (And atom False') = False'
psimplify' (And False' atom) = False'
psimplify' (And atom True') = atom
psimplify' (And True' atom) = atom
psimplify' (Or atom False') = atom
psimplify' (Or False' atom) = atom
psimplify' (Or atom True') = True'
psimplify' (Or True' atom) = True'
psimplify' (Imp False' atom) = True'
psimplify' (Imp atom True') = True'
psimplify' (Imp True' atom) = atom
psimplify' (Imp atom False') = (Not atom)
psimplify' (Iff atom True') = atom
psimplify' (Iff True' atom) = atom
psimplify' (Iff atom False') = (Not atom)
psimplify' (Iff False' atom) = (Not atom)
psimplify' atom = atom

negative :: Formula a -> Bool
negative (Not atom) = True
negative _ = False

positive :: Formula a -> Bool
positive atom = not (negative atom)

-- Substitution
psubst subfn = onAtoms (\p -> subfn p (Atom p))

-- Forall
forall :: (a -> Bool) -> [a] -> Bool
forall p [] = True
forall p (h:t) = (p h) && forall p t

-- Exists
exists :: (a -> Bool) -> [a] -> Bool
exists p [] = False
exists p (h:t) = (p h) || exists p t
