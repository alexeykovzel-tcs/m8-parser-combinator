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

-- Parses a lowercase letter
lower :: Parser Char
lower = charIf isLower

-- Parses an uppercase letter
upper :: Parser Char
upper = charIf isUpper

-- Parses a digit
dig :: Parser Char
dig = charIf isDigit

-- Examples:
ex_letter   = parse letter $ Stream "abc"
ex_dig      = parse dig $ Stream "123"

-----------------------------------------------------------------------------
-- FP2.2
-----------------------------------------------------------------------------

between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3

whitespace :: Parser a -> Parser a
whitespace p = between ws p ws
    where ws = many $ oneOf " \t\n"

oneOf :: [Char] -> Parser Char
oneOf xs = charIf (\x -> elem x xs)

-- Examples:
ex_between      = parse (between (char '(') (char 'a') (char ')')) $ Stream "(a)b"
ex_whitespace   = parse (whitespace $ string "a") $ Stream "\n a \n "
ex_oneOf        = parse (oneOf "abc") $ Stream "banana"

-----------------------------------------------------------------------------
-- FP2.3
-----------------------------------------------------------------------------

-- Parses one or more occurrences of p, separated by s
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p <*> (s *> sep1 p s <|> pure [])

-- Works as sep1 p s, but parses zero or more occurrences of p
sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> pure []

-- Tries to apply parser p; upon failure it results in x
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- Examples:
ex_sep1         = parse (sep1 (char 'a') (char ',')) $ Stream "a,a,a ccc"
ex_sep1_fail    = parse (sep1 (char 'a') (char ',')) $ Stream "ccc"
ex_sep          = parse (sep (char 'a') (char ',')) $ Stream "ccc"
ex_option_fail  = parse (option 'b' (char 'a')) $ Stream "cba"
ex_option       = parse (option 'b' (char 'a')) $ Stream "abc"

-----------------------------------------------------------------------------
-- FP2.4
-----------------------------------------------------------------------------

-- Parses a given string, similar to the function char
string :: String -> Parser String
string xs = foldr (\a b -> (:) <$> a <*> b) (pure "") (char <$> xs)

-- Parses an identifier surrounded by whitespace
identifier :: Parser String 
identifier = whitespace $ (:) <$> lower <*> (many $ lower <|> dig)

-- Parses an integer surrounded by whitespace
integer :: Parser Integer
integer = whitespace $ read <$> some dig

-- Parses a given string surrounded by whitespaces
symbol :: String -> Parser String
symbol xs = whitespace $ string xs

-- Parses something using the provided parser between parentheses
parens :: Parser a -> Parser a 
parens p = between (char '(') p (char ')')

-- Parses something using the provided parser between braces
braces :: Parser a -> Parser a 
braces p = between (char '{') p (char '}')

-- Examples:
ex_string       = parse (string "abc") $ Stream "abc"
ex_identifier   = parse identifier $ Stream "a123"
ex_integer      = parse integer $ Stream "123"
ex_symbol       = parse (symbol "abc") $ Stream "\n abc \n"
ex_parens       = parse (parens $ char 'a') $ Stream "(a)"
ex_braces       = parse (braces $ char 'a') $ Stream "{a}"