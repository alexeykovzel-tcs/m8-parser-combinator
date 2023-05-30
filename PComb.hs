-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since they are parsed.
-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Second student (syyyyyyy)
-- Student 3: Third student (szzzzzzz)


module PComb where
import Control.Applicative
import Data.Char
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)


