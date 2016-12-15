module Utils
  ( atoms
  , atomUnion
  ) where

import Data.List
import Logic

-- Get list of atoms
atoms formula = atomUnion (\atom -> [atom]) formula

-- Collect atoms by some attribute set by function
atomUnion function formula = overAtoms (\p q -> function p `union` q) formula []
