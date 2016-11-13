module Parser(
    parse
    ) where

import Data.Function
import Data.List
import Data.Char

-- Test: "if (*p1-- == *p2++) then f() else g()"

-- Returns category of character
charCategory :: Char -> Int
charCategory ch
            | ch `elem` alphanumeric = 1
            | ch `elem` numeric      = 2
            | ch `elem` punctuation  = 3
            | ch `elem` space        = 4
            | ch `elem` symbolic     = 5
            | otherwise              = 6
            where
              space = " \t\n\r"
              punctuation = "()[]{}"
              symbolic = "~!@#$%^&*-+=|\\:;<>.?/"
              numeric = "0123456789"
              alphanumeric = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'"

-- Groups by category and removes spaces
parse :: String -> [String]
parse = filter (isNotSpaceString) . groupBy ((==) `on` charCategory)
            where
              space = " \t\n\r"
              isNotSpaceString = not . any (`elem` space) 