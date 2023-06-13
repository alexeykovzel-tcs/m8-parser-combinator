-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

module PComb where
import Control.Applicative
import Data.Char
import Test.QuickCheck

-----------------------------------------------------------------------------
-- FP1.1 (Authors: Denis & Aliaksei)
-----------------------------------------------------------------------------

-- Parser accepts a stream of chars and returns the parsed entity
-- with the remaining chars to be parsed
data Parser a = P {
    parse :: Stream -> ParseResult a
}

-- Stream of chars with a scanner that tracks the position 
-- of the last parsed character
data Stream 
    = Stream [Char] Scanner
    deriving (Eq, Show)

-- Parser results in either the parse error, indicating the position
-- of where the error occurred, or the parsed entity
data ParseResult a
    = ParseError Scanner
    | Result [(a, Stream)]
    deriving (Eq, Show)

-- Creates a stream of characters
stream :: String -> Stream
stream x = Stream x initScanner

-----------------------------------------------------------------------------
-- FP5.1 (Author: Denis)
-----------------------------------------------------------------------------

-- Error generator for parsers
(<?>) :: Parser a -> String -> Parser a
p <?> str = P $ \(Stream xs s) ->
    case (parse p $ Stream xs s) of
        Result r -> Result r
        ParseError s -> error 
            $ "Parse error at " ++ show s 
            ++ ", expected " ++ str

-- Scanner tracking the character position
data Scanner 
    = Position Int Int
    deriving (Eq, Ord)

instance Show Scanner where
    show (Position row column) 
        = show row ++ ":" ++ show column

-- Initializes a scanner at the beginning
initScanner :: Scanner
initScanner = Position 1 1

-- Updates a scanner position based on the parsed character
updateScanner :: Scanner -> Char -> Scanner
updateScanner (Position row column) c
    | c == '\n' = Position (row + 1) 1
    | c == '\t' = Position row $ column + 8 - ((column - 1) `mod` 8)
    | otherwise = Position row $ column + 1

-- Examples of usage
initScannerEx = initScanner
updateScannerEx = updateScanner initScanner '\n'

prop_initScanner = show initScanner == "1:1"
prop_updateScanner = show updateScannerEx == "2:1"

-----------------------------------------------------------------------------
-- FP1.2 (Authors: Denis & Aliaksei)
-----------------------------------------------------------------------------

-- Applies a function to the parsed value
instance Functor Parser where
    fmap f (P p) = P $ \xs -> 
        case (p xs) of
            Result r -> Result [(f a, ys) | (a, ys) <- r]
            ParseError e -> ParseError e

-- Examples of usage
fmapEx = parse ((,) <$> (char 'a') <*> (char 'b')) $ stream "abc"

prop_fmapEx = getResult fmapEx == ('a','b')

-----------------------------------------------------------------------------
-- FP1.3 (Authors: Denis & Aliaksei)
-----------------------------------------------------------------------------

-- Parses a char if predicate on it returns true
charIf :: (Char -> Bool) -> Parser Char
charIf pred = P p
    where p (Stream [] s) = ParseError s
          p (Stream (x:xs) s)
            | pred x = Result [(x, Stream xs $ updateScanner s x)]
            | otherwise = ParseError s

-- Parses a predefined char
char :: Char -> Parser Char
char x = charIf (\y -> x == y)

-- Examples of usage
charEx = parse (char 'b') $ stream "bbc"
charIfEx = parse (charIf (\x -> x == 'b')) $ stream "bbc"

prop_charEx = getResult charEx == 'b'
prop_charIfEx = getResult charIfEx == 'b'

-----------------------------------------------------------------------------
-- FP1.4 (Author: Aliaksei)
-----------------------------------------------------------------------------

-- Parser that always fails
failure :: Parser a
failure = P (\_ -> ParseError initScanner)

-- Examples of usage
failureEx = parse failure $ stream "abc"

-----------------------------------------------------------------------------
-- FP1.5 (Authors: Denis & Aliaksei)
-----------------------------------------------------------------------------

-- Applies a function within a Parser context
instance Applicative Parser where
    pure p = P $ \xs -> Result [(p, xs)]
    p1 <*> p2 = P $ \xs -> 
        case (parse p1 xs) of
            ParseError e -> ParseError e
            Result [(a, str)] -> parse (a <$> p2) str

-- Examples of usage
pureEx = parse (pure 42) $ stream "123"
appEx = parse ((,) <$> char 'a' <*> char 'a') $ stream "aac"

prop_pureEx = getResult pureEx == 42
prop_appEx = getResult appEx == ('a','a')

-----------------------------------------------------------------------------
-- FP1.6 (Authors: Denis & Aliaksei)
-----------------------------------------------------------------------------

instance Alternative Parser where
    empty = failure
    p1 <|> p2 = P $ \xs -> 
        case (parse p1 xs) of
            ParseError _ -> parse p2 xs
            Result r -> Result r

-- Examples of usage
emptyEx = parse empty $ stream "abc"
someEx = parse (some $ char 'a') $ stream "aaabc"
manyEx = parse (many $ char 'a') $ stream "bc"
altEx = parse (char 'a' <|> char 'b') $ stream "bca"

prop_someEx = getResult someEx == "aaa"
prop_manyEx = getResult manyEx == ""
prop_altEx = getResult altEx == 'b'

-----------------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------------

-- Joins strings using a separator
-- e.g. join ", " ["Hi", "John"] = "Hi, John"
join :: String -> [String] -> String
join _ [x]      = x
join sep (x:xs) = x ++ sep ++ (join sep xs)

getResult :: ParseResult a -> a
getResult p = fst $ head $ (\(Result r) -> r) p