-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

module BasicParsers where

import Control.Applicative
import Data.Char
import Test.QuickCheck
import PComb

-----------------------------------------------------------------------------
-- FP2.1 (Author: Aliaksei)
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

-- Examples of usage
letterEx = parse letter $ stream "bbc"
lowerEx = parse lower $ stream "bbc"
upperEx = parse upper $ stream "Bbc"
digEx = parse dig $ stream "5ac"

prop_letterEx = getResult letterEx == 'b'
prop_lowerEx = getResult lowerEx == 'b'
prop_upperEx = getResult upperEx == 'B'
prop_digEx = getResult digEx == '5'

-----------------------------------------------------------------------------
-- FP2.2 (Author: Aliaksei)
-----------------------------------------------------------------------------

-- Parsers whitespace characters around another parser
whitespace :: Parser a -> Parser a
whitespace p = between ws p ws
    where ws = many $ oneOf " \t\n"

-- Parsers a values between other parsers
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3

-- Parses one of the characters 
oneOf :: [Char] -> Parser Char
oneOf xs = charIf (\x -> elem x xs)

-- Examples of usage
whitespaceEx = parse (whitespace dig) $ stream " 1 "
betweenEx = parse (between (char '(') dig (char ')')) $ stream "(1)"
oneOfEx = parse (oneOf "abc") $ stream "cde"

prop_whitespaceEx = getResult whitespaceEx == '1'
prop_betweenEx = getResult betweenEx == '1'
prop_oneOfEx = getResult oneOfEx == 'c'

-----------------------------------------------------------------------------
-- FP2.3 (Author: Aliaksei)
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

-- Examples of usage
sep1Ex = parse (sep1 (char 'a') (char ',')) $ stream "a,a,a"
sepEx = parse (sep (char 'a') (char ',')) $ stream ""
optionEx = parse (option 'x' (char 'a')) $ stream "xabc"

prop_sep1Ex = getResult sep1Ex == "aaa"
prop_sepEx = getResult sepEx == ""
prop_optionEx = getResult optionEx == 'x'

-----------------------------------------------------------------------------
-- FP2.4 (Author: Aliaksei)
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

-- Examples of usage
stringEx = parse (string "There was")            $ stream "There was a guy"
identifierEx = parse identifier                  $ stream "rev0l leksah was his name"
integerEx = parse integer                        $ stream "100 years old he was"
symbolEx = parse (symbol "once he looked")       $ stream "once he looked in the mirror"
parensEx = parse (parens $ string "Wow!")        $ stream "(Wow!) - he was shocked"
bracesEx = parse (braces $ string "haskell0ver") $ stream "{haskell0ver} he saw in the mirror"

prop_stringEx = getResult stringEx == "There was"
prop_identifierEx = getResult identifierEx == "rev0l"
prop_integerEx = getResult integerEx == 100
prop_symbolEx = getResult symbolEx == "once he looked"
prop_parensEx = getResult parensEx == "Wow!"
prop_bracesEx = getResult bracesEx == "haskell0ver"