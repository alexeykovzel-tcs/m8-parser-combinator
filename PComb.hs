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
    parse :: String -> ErrorHandler Scanner [(a, String)]
}

data ErrorHandler a b = ParseError a | Result b deriving Show

-----------------------------------------------------------------------------
-- FP5.1
-----------------------------------------------------------------------------

-- Position scanner where parse error occurs
data Scanner = Position String Int Int deriving (Eq, Ord)

instance Show Scanner where
    show (Position _str row column) = show row ++ ":" ++ show column

-- Initializes a scanner
initScanner :: Scanner
initScanner = Position "" 1 1

-- Updates a scanner line and column position depends on the char
updateScanner :: Scanner -> Char -> Scanner
updateScanner (Position str row column) c
    | c == '\n' = Position str (row+1) 1
    | c == '\t' = Position str row (column + 8 - ((column-1) `mod` 8))
    | otherwise = Position str row (column + 1)

-- Scans a string and returns position
scan :: Scanner -> String -> Scanner
scan pos str = foldl updateScanner pos str

-- Parser combinator
(<?>) :: Parser a -> String -> Parser a
p <?> str = P (\xs ->
    case (parse p xs) of
        ParseError e -> error $ "Parse error at " ++ show (scan e xs) ++ ", expected " ++ str
        Result r -> Result r
    )

-----------------------------------------------------------------------------
-- FP1.2
-----------------------------------------------------------------------------

-- Applies function to the parsed value
instance Functor Parser where
    fmap f (P p) = P (\xs -> 
        case (p xs) of
            ParseError e -> ParseError e
            Result r -> Result [(f a, ys) | (a, ys) <- r]
        )

-----------------------------------------------------------------------------
-- FP1.3
-----------------------------------------------------------------------------

-- Parses char if predicate returns true
charIf :: (Char -> Bool) -> Parser Char
charIf pred = P p
    where p [] = ParseError initScanner
          p (x:xs)
            | pred x = Result [(x, xs)]
            | otherwise = ParseError initScanner

-- Parses a predefined char
char :: Char -> Parser Char
char x = charIf (\y -> x == y)

-----------------------------------------------------------------------------
-- FP1.4
-----------------------------------------------------------------------------

-- Parser that always fails
failure :: Parser a
failure = P (\_ -> ParseError initScanner)

-----------------------------------------------------------------------------
-- FP1.5
-----------------------------------------------------------------------------

instance Applicative Parser where
    pure p = P (\xs -> Result [(p, xs)])
    p1 <*> p2 = P (\xs -> 
        case (parse p1 xs) of
            ParseError e -> ParseError e
            Result [(a, str)] -> parse (a <$> p2) str
        )

-----------------------------------------------------------------------------
-- FP1.6
-----------------------------------------------------------------------------

instance Alternative Parser where
    empty = failure
    some p = (:) <$> p <*> many p
    many p = some p <|> pure []
    p1 <|> p2 = P (\xs -> 
        case (parse p1 xs) of
            ParseError _ -> parse p2 xs
            Result r -> Result r
        )