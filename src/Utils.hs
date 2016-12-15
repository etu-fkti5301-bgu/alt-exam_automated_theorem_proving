module Utils
  (
  listOfAtoms
  ) where

import Logic

listOfAtoms :: (Formula a)-> [a]
listOfAtoms formula = overAtoms (\x y -> x:y) formula []
