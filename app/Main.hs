module Main where

-- Imports

import Util.Prelude
import Util.Parse
import Formula
import FormulaSyn
import System.Environment
import Skolem

-- Replace double-quotes (") to (\')
showToJSON :: Show a => a -> String
showToJSON = foldr (\x acc -> if x == '\"' then "\\\'" ++ acc else x:acc) [] . show

-- Make JSON object {"<s1>":"<s2>"}
makeJSONObject :: String -> String -> String
makeJSONObject s1 s2 = "\"" ++ s1 ++ "\":\"" ++ s2 ++ "\""

-- Make JSON with pnf, snf
makeJSON :: Formula -> String
makeJSON f = let body = formula ++ "," ++ parsed ++ "," ++ inPnf ++ "," ++ inSnf
                 formula = makeJSONObject "formula" $ showToJSON f
                 parsed  = makeJSONObject "parsed" $ showToJSON $ pPrint f
                 inPnf   = makeJSONObject "pnf" $ showToJSON $ pPrint $ pnf f
                 inSnf   = makeJSONObject "snf" $ showToJSON $ pPrint $ skolemize f
             in "{" ++ body ++  "}"

-- Print JSON of each formula
printJSONs :: [Formula] -> IO ()
printJSONs []   = return ()
printJSONs (x:xs) = do
   putStrLn $ makeJSON x
   printJSONs xs

main :: IO ()
main = do
   args <- getArgs
   let formulas :: [Formula] = map parse args
   printJSONs formulas
