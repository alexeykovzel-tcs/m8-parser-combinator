-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

module PComb where
import Control.Applicative
import Data.Char
import Test.QuickCheck

-- Stream of Chars
data Stream 
    = Stream [Char]
    deriving (Eq, Show)

-----------------------------------------------------------------------------
-- FP1.1
-----------------------------------------------------------------------------

data Parser a = P {
    -- parse :: String -> [(a, String)]
    parse :: String -> ErrorHandler ParseError [(a, String)]
}

data ErrorHandler a b = ParseError a | Result b deriving Show

getResult :: ErrorHandler a b -> b
getResult (Result b) = b

getError :: ErrorHandler a b -> a
getError (ParseError a) = a

-- Parse errors
data ParseError = ParsePredicateOpError
                | ParsePredicateError
                | ParseConditionError
                | ParseFunCallError
                | ParseFactorError
                | ParseTermError
                | ParseExpressionError
                | ParseArgumentError
                | ParseStatementError
                | ParseProgramError
                | DefaultError
                deriving Show

-----------------------------------------------------------------------------
-- FP1.2
-----------------------------------------------------------------------------

-- Applies function to the parsed value
instance Functor Parser where
    fmap f (P p) = P (\xs -> 
        case (p xs) of
            ParseError e -> ParseError DefaultError
            Result r -> Result [(f a, ys) | (a, ys) <- r]
        )

-----------------------------------------------------------------------------
-- FP1.3
-----------------------------------------------------------------------------

-- Parses char if predicate returns true
charIf :: (Char -> Bool) -> Parser Char
charIf pred = P p
    where p [] = ParseError DefaultError
          p (x:xs)
            | pred x = Result [(x, xs)]
            | otherwise = ParseError DefaultError

-- Parses a predefined char
char :: Char -> Parser Char
char x = charIf (\y -> x == y)

-----------------------------------------------------------------------------
-- FP1.4
-----------------------------------------------------------------------------

-- Parser that always fails
failure :: Parser a
failure = P (\_ -> ParseError DefaultError)

-----------------------------------------------------------------------------
-- FP1.5
-----------------------------------------------------------------------------

instance Applicative Parser where
    pure a = P (\xs -> Result [(a, xs)])
    p1 <*> p2 = 
        P (\xs -> Result [
            (a b, zs) | 
            (a, ys) <- getResult $ parse p1 xs,
            (b, zs) <- getResult $ parse p2 ys 
        ])

-----------------------------------------------------------------------------
-- FP1.6
-----------------------------------------------------------------------------

instance Alternative Parser where
    empty = failure
    p1 <|> p2 = P (\xs ->
        case (getResult $ parse p1 xs) of
            [] -> parse p2 xs
            r -> Result r
        )