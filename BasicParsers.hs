-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

module BasicParsers where

import Control.Applicative
import Data.Char
import Test.QuickCheck
import PComb

-----------------------------------------------------------------------------
-- FP2.1
-----------------------------------------------------------------------------

-- Parses a letter
letter :: Parser Char
letter = charIf isLetter

letterEx = parse (letter) $ Stream "bbc" initScanner

-- Parses a lowercase letter
lower :: Parser Char
lower = charIf isLower

lowerEx = parse (lower) $ Stream "bbc" initScanner

-- Parses an uppercase letter
upper :: Parser Char
upper = charIf isUpper

upperEx = parse (upper) $ Stream "Bbc" initScanner

-- Parses a digit
dig :: Parser Char
dig = charIf isDigit

digEx = parse (dig) $ Stream "5ac" initScanner

-----------------------------------------------------------------------------
-- FP2.2
-----------------------------------------------------------------------------

between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3

betweenEx = parse (between (char '(') (identifier) (char ')')) $ Stream "(expression)" initScanner

whitespace :: Parser a -> Parser a
whitespace p = between ws p ws
    where ws = many $ oneOf " \t\n"

whitespaceEx = parse (whitespace (identifier)) $ Stream " expression " initScanner

oneOf :: [Char] -> Parser Char
oneOf xs = charIf (\x -> elem x xs)

oneOfEx = parse (oneOf "abc") $ Stream "cde" initScanner

-----------------------------------------------------------------------------
-- FP2.3
-----------------------------------------------------------------------------

-- Parses one or more occurrences of p, separated by s
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p <*> (s *> sep1 p s <|> pure [])

sep1Ex = parse (sep (char 'a') (char ',')) $ Stream "a,a,a" initScanner

-- Works as sep1 p s, but parses zero or more occurrences of p
sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> pure []

sepEx = parse (sep (char 'a') (char ',')) $ Stream "b,b,b" initScanner

-- Tries to apply parser p; upon failure it results in x
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

optionEx = parse (option 'x' (char 'a')) $ Stream "xabc" initScanner

-----------------------------------------------------------------------------
-- FP2.4
-----------------------------------------------------------------------------

-- Parses a given string, similar to the function char
string :: String -> Parser String
string xs = foldr (\a b -> (:) <$> a <*> b) (pure "") (char <$> xs)

stringEx = parse (string "There was") $ Stream "There was a guy" initScanner

-- Parses an identifier surrounded by whitespace
identifier :: Parser String 
identifier = whitespace $ (:) <$> lower <*> (many $ lower <|> dig)

identifierEx = parse (identifier) $ Stream "rev0l leksah was his name" initScanner

-- Parses an integer surrounded by whitespace
integer :: Parser Integer
integer = whitespace $ read <$> some dig

integerEx = parse (integer) $ Stream "100 years old he was" initScanner

-- Parses a given string surrounded by whitespaces
symbol :: String -> Parser String
symbol xs = whitespace $ string xs

symbolEx = parse (symbol "once he looked") $ Stream "once he looked in the mirror" initScanner

-- Parses something using the provided parser between parentheses
parens :: Parser a -> Parser a 
parens p = between (char '(') p (char ')')

parensEx = parse (parens (string "Wow!")) $ Stream "(Wow!) - he was shocked" initScanner

-- Parses something using the provided parser between braces
braces :: Parser a -> Parser a 
braces p = between (char '{') p (char '}')

bracesEx = parse (braces (string "haskell0ver")) $ Stream "{haskell0ver} he saw in the mirror" initScanner