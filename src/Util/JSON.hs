module Util.JSON
  ( JSONObject(..)
  , JSON(..)
  , jsonToString 
  , prepareToJSON
  )
where

import Prelude

data JSONObject = JSONInt Int
                | JSONStr String
                | JSONObj [(String, JSONObject)]
                | JSONLst [JSONObject]
  deriving (Show)

jsonToString :: JSONObject -> String
jsonToString (JSONInt x) = show x
jsonToString (JSONStr x) = show x
jsonToString (JSONLst []) = "[]"
jsonToString (JSONLst xs) = "[" ++ foldr (\x acc -> (jsonToString x) ++ ", " ++ acc) (jsonToString $ last xs) (init xs) ++ "]"
jsonToString (JSONObj []) = "{}"
jsonToString (JSONObj xs) = "{" ++ foldr (\x acc -> (mkStr x) ++ ", " ++ acc) (mkStr $ last xs) (init xs) ++ "}"
                               where
                                 mkStr (k, v) = show k ++ ":" ++ jsonToString v

prepareToJSON :: String -> String
prepareToJSON = foldr (\x acc -> if x == '\"' then "\\\'" ++ acc else x:acc) []

class JSON a where
   makeJSON :: a -> JSONObject

instance JSON JSONObject where
   makeJSON x = x

instance JSON Int where
   makeJSON x = JSONInt x

instance {-# OVERLAPPING #-} JSON String where
   makeJSON x = JSONStr $ prepareToJSON x

instance {-# OVERLAPPABLE #-} JSON a => JSON [a] where
   makeJSON x = JSONLst $ map makeJSON x

instance {-# OVERLAPPING #-} JSON a => JSON [(String, a)] where
   makeJSON x = JSONObj $ map (\(k,v) -> (prepareToJSON k, makeJSON v)) x
