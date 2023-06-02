-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

module BasicParsers where

import Control.Applicative
import Data.Char
import Test.QuickCheck
import PComb

-- FP2.1
-- Define the parsers letter :: Parser Char that parses any 
-- (alphabetical) letter, and dig :: Parser Char that parses 
-- any digit.

-- FP2.2
-- The following parser combinators:

-- [1pt] between :: Parser a -> Parser b -> Parser c -> Parser b 
-- runs the three parsers in sequence, and returns the result 
-- of the second parser. Similar to between in ParSec.

-- [1pt] whitespace :: Parser a -> Parser a receives a parser p 
-- and uses it to parse the input stream, while skipping all 
-- surrounding whitespaces (space, tab and newline). For example, 
-- whitespace (char ’a’) can be used to parse the input " a b ", 
-- which results in ’a’ and the remaining input stream "b ".

-- FP2.3
-- The following parser combinators:

-- [1pt] sep1 :: Parser a -> Parser b -> Parser [a]. The parser sep1 p s parses
-- one or more occurrences of p, separated by s. This can, for example, be used
-- to parse a comma separated list.

-- [1pt] sep :: Parser a -> Parser b -> Parser [a]. The parser sep p s works as
-- sep1 p s, but parses zero or more occurrences of p.

-- [1pt] option :: a -> Parser a -> Parser a. option x p tries to apply parser p;
-- upon failure it results in x. Similar to option in ParSec.

-- FP2.4
