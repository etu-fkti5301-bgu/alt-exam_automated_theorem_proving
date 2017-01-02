module Main where

-- Imports

import Util.JSON
import Util.Parse
import Util.Prelude
import Formula
import FormulaSyn
import System.Environment
import Skolem
import Unif


main :: IO ()
main = do
   args <- getArgs
   let mode = head args
       formulas :: [Formula] = map parse $ tail args
   case mode of
     "pnf" -> putStrLn jsonStr
              where
                jsonStr = jsonToString json
                json = makeJSON [("parsed_formulas", pforms), ("pnfs", pnfs)]
                pforms = makeJSON $ map (makeJSON . show . pPrint) formulas
                pnfs = makeJSON $ map (makeJSON . show . pPrint . pnf) formulas
     _ -> putStrLn "undefined"

