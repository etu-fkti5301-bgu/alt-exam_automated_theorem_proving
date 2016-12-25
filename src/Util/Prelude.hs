-- * Signature

module Util.Prelude 
  ( module Prelude
  , (<$>)
  , first, second
  , (<>), (<+>)
  , fromJust
  , pPrint
  , print
  , putStr
  , putStrLn
  , pp
  )
where

-- * Imports

import Prelude hiding (print, putStr, putStrLn)
import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import qualified Util.Print as Print
import Util.Print ((<>), (<+>), Doc, pPrint, Print)
import Data.Maybe (fromJust)
import System.IO.UTF8 (print, putStr, putStrLn)

-- * Util

pp :: Print a => a -> IO ()
pp = Print.putStrLn . pPrint

-- instance Monad m => Functor m where
--   fmap f x = x >>= return . f
