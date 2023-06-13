-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

module PComb where
import Control.Applicative
import Data.Char
import Test.QuickCheck

-- Stream of Chars
data Stream 
    = Stream [Char] Scanner
    deriving (Eq, Show)

-----------------------------------------------------------------------------
-- FP1.1
-----------------------------------------------------------------------------

data Parser a = P {
    parse :: Stream -> ErrorHandler Scanner [(a, Stream)]
}

-- Parser has an error handler which returns an output 
-- if everything was parsed correctly, otherwise a Scanner 
-- with row and column were error happened
data ErrorHandler a b 
    = ParseError a | Result b 
    deriving Show

-----------------------------------------------------------------------------
-- FP5.1
-----------------------------------------------------------------------------

type Row = Int
type Column = Int

-- Position scanner where parse error occurs
data Scanner 
    = Position String Row Column 
    deriving (Eq, Ord)

instance Show Scanner where
    show (Position _str row column) 
        = show row ++ ":" ++ show column

-- Initializes a scanner
initScanner :: Scanner
initScanner = Position "" 1 1

-- Updates a scanner line and column position depends on the char
updateScanner :: Scanner -> Char -> Scanner
updateScanner (Position str row column) c
    | c == '\n' = Position str (row+1) 1
    | c == '\t' = Position str row (column + 8 - ((column-1) `mod` 8))
    | otherwise = Position str row (column + 1)

-- Error generator
(<?>) :: Parser a -> String -> Parser a
p <?> str = P (\(Stream xs s) ->
    case (parse p (Stream xs s)) of
        Result r -> Result r
        ParseError s -> error 
            $ "Parse error at " ++ show s 
            ++ ", expected " ++ str
    )

-- Examples:
initScannerEx = initScanner
updateScannerEx = updateScanner initScanner '\n'
errorGenEx = parse ((char 'a') <?> "'a'") $ Stream "bac" initScanner

-----------------------------------------------------------------------------
-- FP1.2
-----------------------------------------------------------------------------

-- Applies function to the parsed value
instance Functor Parser where
    fmap f (P p) = P (\xs -> 
        case (p xs) of
            Result r -> Result [(f a, ys) | (a, ys) <- r]
            ParseError e -> ParseError e
        )

-- Example:
fmapEx = parse ((,) <$> (char 'a') <*> (char 'b')) $ Stream "abc" initScanner

-----------------------------------------------------------------------------
-- FP1.3
-----------------------------------------------------------------------------

-- Parses char if predicate returns true
charIf :: (Char -> Bool) -> Parser Char
charIf pred = P p
    where p (Stream [] s) = ParseError s
          p (Stream (x:xs) s)
            | pred x = Result [(x, Stream xs $ updateScanner s x)]
            | otherwise = ParseError s

-- Parses a predefined char
char :: Char -> Parser Char
char x = charIf (\y -> x == y)

-- Examples:
charIfEx = parse (charIf (\x -> x == 'b')) $ Stream "bbc" initScanner
charEx = parse (char 'b') $ Stream "bbc" initScanner

-----------------------------------------------------------------------------
-- FP1.4
-----------------------------------------------------------------------------

-- Parser that always fails
failure :: Parser a
failure = P (\_ -> ParseError initScanner)

-- Example:
failureEx = parse (failure) $ Stream "abc" initScanner

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

-- Examples:
pureEx = parse (pure 42) $ Stream "123" initScanner
appEx = parse ((,) <$> char 'a' <*> char 'a') $ Stream "aac" initScanner

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

-- Examples:
emptyEx = parse (empty) $ Stream "abc" initScanner
someEx = parse (some (char 'a')) $ Stream "aaabc" initScanner
manyEx = parse (many (char 'a')) $ Stream "bc" initScanner
altEx = parse ((char 'a') <|> (char 'b')) $ Stream "bca" initScanner