module Parser(
    lex',
    parseConst,
    parseVar,
    parseAtom,
    parseProduct,
    parseExpression
    ) where

import Data.Function
import Data.List
import Data.Char
import Data.Maybe
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

parseExpression :: [String] -> (Maybe Expression, [String])
parseExpression x 
               | (isNothing . fst) parsedProduct    = (Nothing, x)
               | (null . snd) parsedProduct         = parsedProduct
               | (isNothing . fst) parsedExpression = parsedProduct
               | (head . snd) parsedProduct == "+"  = (Just (Add ((fromJust . fst) parsedProduct) ((fromJust . fst) parsedExpression)), snd parsedExpression)
               | otherwise = parsedProduct
               where
                  parsedProduct    = parseProduct x
                  parsedExpression = (parseExpression . tail . snd) parsedProduct

parseProduct :: [String] -> (Maybe Expression, [String])
parseProduct x 
               | (isNothing . fst) parsedAtom    = (Nothing, x)
               | (null . snd) parsedAtom         = parsedAtom
               | (isNothing . fst) parsedProduct = parsedAtom
               | (head . snd) parsedAtom == "*"  = (Just (Mul ((fromJust . fst) parsedAtom) ((fromJust . fst) parsedProduct)), snd parsedProduct)
               | otherwise = parsedAtom
               where
                  parsedAtom    = parseAtom x
                  parsedProduct = (parseProduct . tail . snd) parsedAtom

parseAtom :: [String] -> (Maybe Expression, [String])
parseAtom x
         | head x == "(" = if (not . null . snd) parsedExpression
                           then (Nothing, x)
                           else (fst parsedExpression, suffix)
         | (not . isNothing) parsedConst = (parsedConst, tail x)
         | (not . isNothing) parsedVar   = (parsedVar, tail x)
         | otherwise = (Nothing, x)
         where
            parsedExpression = let exp = ((takeWhile (/= ")")) . tail) x
                               in parseExpression exp
            suffix = (tail . dropWhile (/= ")")) x
            parsedConst = (parseConst . head) x
            parsedVar   = (parseVar . head) x
