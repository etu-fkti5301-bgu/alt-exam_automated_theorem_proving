module Parser(
    lex'
    ) where

import Data.Function
import Data.List
import Data.Char
import Expressions

-- Test: "if (*p1-- == *p2++) then f() else g()"

-- Returns category of character
charCategory :: Char -> Int
charCategory ch
            | ch `elem` alphanumeric = 1
            | ch `elem` punctuation  = 2
            | ch `elem` space        = 3
            | ch `elem` symbolic     = 4
            | otherwise              = 5
            where
              space = " \t\n\r"
              punctuation = "()[]{}"
              symbolic = "~!@#$%^&*-+=|\\:;<>.?/"
              alphanumeric = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'"

-- Groups by category and removes spaces
lex' :: String -> [String]
lex' = separateBrackets . filter (isNotSpaceString) . groupBy ((==) `on` charCategory)
       where
          space = " \t\n\r"
          isNotSpaceString = not . any (`elem` space)

-- Turns strings of brackets into separated elements of list
separateBrackets :: [String] -> [String]
separateBrackets [] = []
separateBrackets (x:xs) = separated ++ separateBrackets xs
                          where
                             separated = if let brackets = "()[]{}" in (x !! 0) `elem` brackets
                                         then (foldr (\ch acc -> [ch]:acc) [] x)
                                         else [x]

-- Parses variables
parseVar :: String -> Maybe Expression
parseVar [] = Nothing
parseVar str@(x:xs) = if let alphabetic = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                         in x `elem` alphabetic
                      then if let alphanumeric = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
                              in foldl (\acc ch -> acc && (ch `elem` alphanumeric)) True xs
                           then Just (Var str)
                           else Nothing
                      else Nothing

-- Parses constants
parseConst :: String -> Maybe Expression
parseConst [] = Nothing
parseConst str@(x:xs) = if let digitsWithoutZero = "123456789"
                           in x `elem` digitsWithoutZero
                        then if let digits = "0123456789"
                                in foldl (\acc ch -> acc && (ch `elem` digits)) True xs
                             then Just (Const (read str :: Int))
                             else Nothing
                        else Nothing

