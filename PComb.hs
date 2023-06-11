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
    parse :: Stream -> [(a, Stream)]
}

-----------------------------------------------------------------------------
-- FP1.2
-----------------------------------------------------------------------------

-- Applies function to the parsed value
instance Functor Parser where
    fmap f p = 
        P (\xs -> [
            (f a, ys) | 
            (a, ys) <- parse p xs 
        ])

-----------------------------------------------------------------------------
-- FP1.3
-----------------------------------------------------------------------------

-- Parses char if predicate returns true
charIf :: (Char -> Bool) -> Parser Char
charIf pred = P p
    where p (Stream []) = []
          p (Stream (x:xs))
            | pred x = [(x, Stream xs)]
            | otherwise = []

-- Parses a predefined char
char :: Char -> Parser Char
char x = charIf (\y -> x == y)

-----------------------------------------------------------------------------
-- FP1.4
-----------------------------------------------------------------------------

-- Parser that always fails
failure :: Parser a
failure = P (\_ -> [])

-----------------------------------------------------------------------------
-- FP1.5
-----------------------------------------------------------------------------

instance Applicative Parser where
    pure a = P (\xs -> [(a, xs)])
    p1 <*> p2 = 
        P (\xs -> [
            (a b, zs) | 
            (a, ys) <- parse p1 xs,
            (b, zs) <- parse p2 ys 
        ])

-----------------------------------------------------------------------------
-- FP1.6
-----------------------------------------------------------------------------

instance Alternative Parser where
    empty = failure
    p1 <|> p2 = P $ \xs ->
        case (parse p1 xs) of
            [] -> parse p2 xs
            r -> r