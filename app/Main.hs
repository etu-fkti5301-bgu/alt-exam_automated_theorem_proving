module Main where

-- Imports

import Util.Prelude
import Util.Parse
import Formula
import FormulaSyn
import System.Environment
import Skolem
import Unif

-- Replace double-quotes (") to (\')
showToJSON :: Show a => a -> String
showToJSON = foldr (\x acc -> if x == '\"' then "\\\'" ++ acc else x:acc) [] . show

-- Make JSON object {"<s1>":"<s2>"}
makeJSONObject :: String -> String -> String
makeJSONObject s1 s2 = "{\"" ++ s1 ++ "\":\"" ++ s2 ++ "\"}"

-- Make JSON list {"<s>" : [x1, x2 ...]"}
makeJSONList :: String -> [String] -> String
makeJSONList s [] = "\"" ++ s ++ "\":[]"
makeJSONList s xs = "\"" ++ s ++ "\":[" ++ foldr (\x acc -> x ++ ", " ++ acc) (last xs) (init xs) ++ "]"

-- Make JSON with pnfs, snfs
makeJSON :: [Formula] -> String
makeJSON f = let body = formulas ++ "," ++ parsed ++ "," ++ inPnf ++ "," ++ inSnf
                 formulas = makeJSONList "formulas" $ map (makeJSONObject "formula" . showToJSON) f
                 parsed   = makeJSONList "parsed_formulas" $ map (makeJSONObject "parsed" . showToJSON . pPrint) f
                 inPnf    = makeJSONList "pnfs" $ map (makeJSONObject "pnf" .showToJSON . pPrint . pnf) f
                 inSnf    = makeJSONList "snfs" $ map (makeJSONObject "snf" .showToJSON . pPrint . skolemize) f
             in "{" ++ body ++  "}"

-- Print JSON of each formula
printJSON :: [Formula] -> IO ()
printJSON = putStrLn . makeJSON

main :: IO ()
main = do
   args <- getArgs
   let formulas :: [Formula] = map parse args
   printJSON formulas

