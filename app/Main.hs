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
                json = makeJSON [("parsed", pforms), ("result", pnfs)]
                pforms = makeJSON $ map (makeJSON . show . pPrint) formulas
                pnfs = makeJSON $ map (makeJSON . show . pPrint . pnf) formulas
     "snf" -> putStrLn jsonStr
              where
                jsonStr = jsonToString json
                json = makeJSON [("parsed", pforms), ("result", snfs)]
                pforms = makeJSON $ map (makeJSON . show . pPrint) formulas
                snfs = makeJSON $ map (makeJSON . show . pPrint . skolemize) formulas
     "unf" -> putStrLn jsonStr
              where
                jsonStr = jsonToString json
                json = makeJSON [("parsed", pforms), ("result", unifiedTerms)]
                pforms = makeJSON $ map (show . pPrint . (\(x, y) -> [x, y]) . head) terms
                unifiedTerms = makeJSON $ map (show . pPrint . (\(x, y) -> [x, y]) . head . fromJust . unifyAndApply) terms
                terms :: [[(Term, Term)]] = (\[x, y] -> [[(x, y)]]) . map parse $ tail args
     _ -> putStrLn "undefined"
