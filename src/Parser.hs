module Parser(
    parseExpression
    ) where

import Data.Function
import Data.List
import Data.Char
import Data.Maybe
import Expressions

-- Parser
parseExpression :: String -> Expression
parseExpression x
               | (not . null . snd) parsedExpression      = error "I can't parse it! :C"
               | (isNothing . fst) parsedExpression = error "I can't parse it! :C"
               | otherwise                          = (fromJust . fst) parsedExpression
               where
                  parsedExpression = (parseExpression' . lex') x

-- Returns category of character
charCategory :: Char -> Int
charCategory ch
            | ch `elem` alphanumeric = 1
            | ch `elem` punctuation  = 2
            | ch `elem` space        = 3
            | ch `elem` symbolic     = 4
            | otherwise              = 5
            where
              space        = " \t\n\r"
              punctuation  = "()[]{}"
              symbolic     = "~!@#$%^&*-+=|\\:;<>.?/"
              alphanumeric = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'"

-- Groups by category and removes spaces
lex' :: String -> [String]
lex' = separateBrackets . filter (isNotSpaceString) . groupBy ((==) `on` charCategory)
       where
          space            = " \t\n\r"
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
                              in all (`elem` alphanumeric) xs
                           then Just (Var str)
                           else Nothing
                      else Nothing

-- Parses constants
parseConst :: String -> Maybe Expression
parseConst [] = Nothing
parseConst str@(x:xs) = if let digitsWithoutZero = "123456789"
                           in x `elem` digitsWithoutZero
                        then if let digits = "0123456789"
                                in all (`elem` digits) xs
                             then Just (Const (read str :: Int))
                             else Nothing
                        else Nothing

-- Parses expressons
parseExpression' :: [String] -> (Maybe Expression, [String])
parseExpression' x 
               | (isNothing . fst) parsedProduct    = (Nothing, x)
               | (null . snd) parsedProduct         = parsedProduct
               | (isNothing . fst) parsedExpression = parsedProduct
               | (head . snd) parsedProduct == "+"  = (Just (Add ((fromJust . fst) parsedProduct) ((fromJust . fst) parsedExpression)), snd parsedExpression)
               | otherwise                          = parsedProduct
               where
                  parsedProduct    = parseProduct x
                  parsedExpression = (parseExpression' . tail . snd) parsedProduct

-- Parses product
parseProduct :: [String] -> (Maybe Expression, [String])
parseProduct x 
               | (isNothing . fst) parsedAtom    = (Nothing, x)
               | (null . snd) parsedAtom         = parsedAtom
               | (isNothing . fst) parsedProduct = parsedAtom
               | (head . snd) parsedAtom == "*"  = (Just (Mul ((fromJust . fst) parsedAtom) ((fromJust . fst) parsedProduct)), snd parsedProduct)
               | otherwise                       = parsedAtom
               where
                  parsedAtom    = parseAtom x
                  parsedProduct = (parseProduct . tail . snd) parsedAtom

-- Parses atom
parseAtom :: [String] -> (Maybe Expression, [String])
parseAtom x
         | (not . isNothing) parsedConst = (parsedConst, tail x)
         | (not . isNothing) parsedVar   = (parsedVar, tail x)
         | head x /= "("                 = (Nothing, x)
         | isNothing takenExpression     = (Nothing, x)
         | (null . snd) parsedExpression = (fst parsedExpression, suffix)
         | otherwise                     = (Nothing, x)
         where
            takenExpression  = takeExpressionFromBrackets x
            parsedExpression = (parseExpression' . fst . fromJust) takenExpression
            suffix = (snd . fromJust) takenExpression
            parsedConst = (parseConst . head) x
            parsedVar   = (parseVar . head) x

-- Returns tuple that contains list from brackets and suffix
takeExpressionFromBrackets :: [String] -> Maybe ([String], [String])
takeExpressionFromBrackets [] = Nothing
takeExpressionFromBrackets xs
                              | head xs /= "("     = Nothing
                              | isNothing expEnd   = Nothing
                              | head brackets /= 0 = Nothing
                              | otherwise          = Just (exp, suffix)
                              where
                                 brackets = scanBrackets xs
                                 expEnd   = (elemIndex 0 . tail) brackets
                                 exp      = (tail . take (fromJust expEnd)) xs
                                 suffix   = drop (fromJust expEnd + 1) xs

-- Returns list of nesting levels of each symbol
scanBrackets :: [String] -> [Int]
scanBrackets = foldr (\str acc ->
                      if str == ")" then (head acc + 1):acc
                      else if str == "("
                      then (head acc - 1):acc
                      else (head acc):acc
                     ) [0]