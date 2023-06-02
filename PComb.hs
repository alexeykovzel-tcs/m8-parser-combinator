-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)


module PComb where
import Control.Applicative
import Data.Char
import Test.QuickCheck

-- Stream of Chars - can be extended with the 
-- location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

{-  FP1.1

    The parser can receive a “stream” (see Stream) of Chars and 
    result in some type a. This implies that a parser is of 
    type Parser a, where a is the type of the parse result.
-}

{-  FP1.2

    The parser has an appropriate Functor instance.
-}

{-  FP1.3

    The function char :: Char -> Parser Char parses a single 
    (given) Char.
-}

{-  FP1.4

    The function failure :: Parser a is a parser that consumes 
    no input and fails (produces no valid parsing result).
-}

{-  FP1.5

    The parser has an Applicative instance for the sequence 
    combinator.
-}

{-  FP1.6

    The parser has an Alternative instance that tries as few 
    alternatives as possible (i.e., until the first parser succeeds).
-}