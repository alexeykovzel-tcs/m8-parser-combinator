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
    parse :: String -> [(a, String)]
}

-----------------------------------------------------------------------------
-- FP1.2
-----------------------------------------------------------------------------

instance Functor Parser where
    fmap f p = 
        P (\xs -> [
            (f a, ys) | 
            (a, ys) <- parse p xs 
        ])

-----------------------------------------------------------------------------
-- FP1.3
-----------------------------------------------------------------------------

-- Parses a single character
char :: Char -> Parser Char
char c = P parse
    where parse xs   
            | length xs == 0 = []
            | head xs == c = [(c,tail xs)]
            | otherwise = []

-----------------------------------------------------------------------------
-- FP1.4
-----------------------------------------------------------------------------

-- Always fails to parse
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
    some p = (:) <$> p <*> many p
    many p = some p <|> pure []
    p1 <|> p2 = P (\xs -> parse p1 xs ++ parse p2 xs)