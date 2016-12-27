-- * Signature

module Util.Prelude 
  ( module Prelude
  , first, second
  , (<>), (<+>)
  , fromJust
  , pPrint
  , pp
  )
where

-- * Imports

import Prelude
import Control.Arrow (first, second)
import qualified Util.Print as Print
import Util.Print ((<>), (<+>), pPrint, Print)
import Data.Maybe (fromJust)

-- * Util

pp :: Print a => a -> IO ()
pp = Print.putStrLn . pPrint

-- instance Monad m => Functor m where
--   fmap f x = x >>= return . f
