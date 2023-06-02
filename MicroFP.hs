-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll

{-  FP3.1

    A set of type/data constructors that represent an EDSL 
    (deep embedding) for µFP, i.e., it describes the same functionality. 
    Note, that this EDSL can be made very compact compared to the 
    grammar. Define the type constructor Prog to indicate the type 
    of a µFP program (top level), such that we can easily recognise 
    your types. Do not use the constructor Function, as it might 
    conflict with some QuickCheck versions. Be creative in your design!
-}

{-  FP3.2
    
    Define the following functions in your µFP EDSL, all of type 
    Prog (i.e., a program with one function). These must correspond 
    to the definitions in functions.txt

    • fibonacci receives a single argument “n” (note this is a µFP 
    argument, not a Haskell argument! The same applies below), 
    and expresses the calculation of the n-th fibonacci number.

    • fib receives a single argument “n”, and expresses the 
    calculation of the n-th fibonacci number.

    • sum receives one argument “a”, and calculates the sum from 
    1 to a

    • div receives two arguments “x” and “y”, and divides “x” by “y”

    • twice receives two arguments“f” (a function) and “x” (a value), 
    and calculates f(f(x))

    • A data structure that describes add, inc and eleven from 
    functions.txt
-}

{-  FP3.3
    
    pretty :: ? -> String is a pretty printer that generates a 
    textual representation that corresponds to the grammar of µFP. 
    Here ? corresponds to your EDSL.
-}

{-  FP3.4

    eval : Prog -> String -> [Integer] -> Integer, which is a 
    evaluator for your µFP EDSL without support for partial application, lazy 
    evaluation, pattern matching and higher order functions. 
    The function with the given name (of type String) in a program 
    (of type Prog) is evaluated with the arguments of type [Integer], 
    resulting in an Integer. Since pattern matching is not (yet) 
    supported, you may assume constants are not used at the left-hand 
    side (e.g., fib does not work with your evaluator yet). We will 
    test your evaluator with some of the definitions from FP3.1 
    (e.g., eval fib [10]). Make sure that they work with eval 
    and that the type of eval is correct 
    (otherwise our tests might fail).
-}

{-  FP4.1

    Define the parsers factor :: Parser ?, expr :: Parser ?, 
    term :: Parser ? and the remaining parsers needed to parse µFP 
    (here ? is a type from your EDSL). Make sure functions.txt is 
    parsed properly and leads to similar definitions as in FP3.2.
-}

{-  FP4.2

    compile :: String -> ? parses and translates a textual 
    representation of µFP to your EDSL.
-}

{-  FP4.3

    runFile :: FilePath -> [Integer] -> IO Integer results in an 
    IO action that reads the specified file, compiles it, and 
    finally uses eval (from FP3.4) to evaluate it with the list 
    of integers as input to the µFP function. When the file contains
    multiple functions, use the last function in the file.
-}