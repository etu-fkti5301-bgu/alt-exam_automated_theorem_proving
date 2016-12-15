module Logic
    ( Formula(..)
    , Proposition(..)
    , pname
    , onAtoms
    , overAtoms
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
    ) where

data Formula =
      False'
    | True'
    | Atom Formula
    | Not Formula
    | And Formula Formula
    | Or Formula Formula
    | Imp Formula Formula
    | Iff Formula Formula
    | Forall String Formula
    | Exists String Formula
    deriving (Eq, Show)

data Proposition = Proposition String

-- Return proposition's name
pname :: Proposition -> String
pname (Proposition string) = string

-- Apply given function to atoms taken
onAtoms :: (Formula -> Formula) -> Formula -> Formula
onAtoms function (Atom formula) = function formula
onAtoms function (Not formula) = (Not $ onAtoms function formula)
onAtoms function (And p q) = (And (onAtoms function p) (onAtoms function q))
onAtoms function (Or p q) = (Or (onAtoms function p) (onAtoms function q))
onAtoms function (Imp p q) = (Imp (onAtoms function p) (onAtoms function q))
onAtoms function (Iff p q) = (Iff (onAtoms function p) (onAtoms function q))
onAtoms function (Forall symbol formula) = (Forall symbol $ onAtoms function formula)
onAtoms function (Exists symbol formula) = (Exists symbol $ onAtoms function formula)
onAtoms _ formula = formula

-- Apply binary function to atoms taken
overAtoms :: (Formula -> t -> t) -> Formula -> t -> t
overAtoms function (Atom formula) operand = function formula operand
overAtoms function (Not formula) operand = overAtoms function formula operand
overAtoms function (And p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Or p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Imp p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Iff p q) operand = overAtoms function p $ overAtoms function q operand
overAtoms function (Forall name formula) operand = overAtoms function formula operand
overAtoms function (Exists name formula) operand = overAtoms function formula operand
overAtoms function _ operand = operand

-- Make AND expression with given arguments
makeAnd :: Formula -> Formula -> Formula
makeAnd p q = (And p q)

-- Return arguments pair of AND expression
destructAnd :: Formula -> (Formula, Formula)
destructAnd (And p q) = (p, q)
destructAnd _ = error "destructAnd"

-- Return conjuncts list of AND expression
conjuncts :: Formula -> [Formula]
conjuncts (And p q) = conjuncts p ++ conjuncts q
conjuncts conjunct = [conjunct]

-- Make AND expression with given arguments
makeOr :: Formula -> Formula -> Formula
makeOr p q = (Or p q)

-- Return parguments pair of OR expression
destructOr :: Formula -> (Formula, Formula)
destructOr (Or p q) = (p, q)
destructOr _ = error "destructOr"

-- Return disjuncts list of OR expression
disjuncts :: Formula -> [Formula]
disjuncts (Or p q) = disjuncts p ++ disjuncts q
disjuncts disjunct = [disjunct]

-- Make IMPLY expression with given arguments
makeImp :: Formula -> Formula -> Formula
makeImp p q = (Imp p q)

-- Return arguments pair of IMPLY expression
destructImp :: Formula -> (Formula, Formula)
destructImp (Imp p q) = (p, q)
destructImp _ = error "destructImp"

-- Return antecedent of IMPLY expression
antecedent :: Formula -> Formula
antecedent formula = fst $ destructImp(formula)

-- Return consequent of IMPLY expression
consequent :: Formula -> Formula
consequent formula = snd $ destructImp(formula)

-- Make EQUIVALENCE expression with given arguments
makeIff :: Formula -> Formula -> Formula
makeIff p q = (Iff p q)

-- Return arguments pair of EQUIVALENCE expression
destructIff :: Formula -> (Formula, Formula)
destructIff (Iff p q) = (p, q)
destructIff _ = error "destructIff"

-- Make FORALL predicate with given name and expression
makeForall :: String -> Formula -> Formula
makeForall name formula = (Forall name formula)

-- Make EXISTS predicate with given name and expression
makeExists :: String -> Formula -> Formula
makeExists name formula = (Exists name formula)
