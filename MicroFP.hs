-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck.All
import qualified Test.QuickCheck as QC

-----------------------------------------------------------------------------
-- FP3.1
-----------------------------------------------------------------------------

-- A set of type/data constructors that represent 
-- an EDSL (deep embedding) for µFP.

type Prog = [Stmt]

data Stmt
    = FunDecl String [Arg] Expr
    deriving Show

data Arg 
    = FixedArg  Integer 
    | VarArg    String
    deriving Show

data Expr
    = Less      Expr Expr
    | Eq        Expr Expr
    | More      Expr Expr
    | Add       Expr Expr
    | Sub       Expr Expr
    | Mult      Expr Expr
    | Fixed     Integer
    | Var       String
    | Cond      Expr Expr Expr
    | FunCall   String [Expr]
    deriving Show

-----------------------------------------------------------------------------
-- FP3.2
-----------------------------------------------------------------------------

-- The following functions in µFP EDSL correspond 
-- to the definitions in functions.txt.

fibonacciProg :: Prog
fibonacciProg = [
    FunDecl "fibonacci" [FixedArg 0] (Fixed 0),
    FunDecl "fibonacci" [FixedArg 1] (Fixed 1),
    FunDecl "fibonacci" [VarArg "n"] (Add addL addR) ]
    where
        addL = FunCall "fibonacci" [Sub (Var "n") (Fixed 1)]
        addR = FunCall "fibonacci" [Sub (Var "n") (Fixed 2)]

fibProg :: Prog
fibProg = [ FunDecl "fib" [VarArg "n"] body ]
    where
        body = Cond cond (Fixed 1) (Add addL addR)
        cond = Less (Var "n") (Fixed 3)
        addL = FunCall "fib" [Sub (Var "n") (Fixed 1)]
        addR = FunCall "fib" [Sub (Var "n") (Fixed 2)]

sumProg :: Prog
sumProg = [ 
    FunDecl "sum" [FixedArg 0] (Fixed 0),
    FunDecl "sum" [VarArg "a"] (Add recSum (Var "a")) ]
    where 
        recSum = FunCall "sum" [Sub (Var "a") (Fixed 1)]

divProg :: Prog
divProg = [ FunDecl "div" [VarArg "x", VarArg "y"] body ]
    where
        body = (Cond cond (Fixed 0) (Add (Fixed 1) divcall))
        cond = Less (Var "x") (Var "y")
        divcall = FunCall "div" [Sub (Var "x") (Var "y"), Var "y"]

twiceProg :: Prog
twiceProg = [ 
    FunDecl "twice" [VarArg "f", VarArg "x"] 
    (FunCall "f" [FunCall "f" [Var "x"]]) ]

combProg :: Prog
combProg = [
    FunDecl "add" [VarArg "x", VarArg "y"] (Add (Var "x") (Var "y")),
    FunDecl "inc" [] (FunCall "add" [Fixed 1]),
    FunDecl "eleven" [] (FunCall "inc" [Fixed 10]) ]

-----------------------------------------------------------------------------
-- FP3.3
-----------------------------------------------------------------------------

-- Pretty printer that generates a textual representation 
-- that corresponds to the grammar of µFP. 

pretty :: Prog -> String
pretty [] = ""
pretty (x:xs) = prettyStmt x ++ pretty xs

prettyStmt :: Stmt -> String
prettyStmt (FunDecl id args expr)
    = id ++ " " ++ prettyArgs ++ " := " ++ prettyExpr expr
    where prettyArgs = join " " (map prettyArg args)

prettyArg :: Arg -> String
prettyArg (FixedArg x) = show x
prettyArg (VarArg x)   = x

prettyExpr :: Expr -> String
prettyExpr (Fixed  x) = show x
prettyExpr (Var    x) = x
prettyExpr (Less x y) = prettyExpr x ++ " < "  ++ prettyExpr y 
prettyExpr (Eq   x y) = prettyExpr x ++ " == " ++ prettyExpr y 
prettyExpr (More x y) = prettyExpr x ++ " > "  ++ prettyExpr y 
prettyExpr (Add  x y) = prettyExpr x ++ " + "  ++ prettyExpr y 
prettyExpr (Sub  x y) = prettyExpr x ++ " - "  ++ prettyExpr y 
prettyExpr (Mult x y) = prettyExpr x ++ " * "  ++ prettyExpr y 

prettyExpr (Cond cond x y) 
    = "if (" ++ prettyExpr cond ++ ")"
    ++ " then { " ++ prettyExpr x ++ " }"
    ++ " else { " ++ prettyExpr y ++ " }"

prettyExpr (FunCall id args) 
    = id ++ " (" ++ prettyArgs ++ ")"
    where prettyArgs = join ", " (map prettyExpr args)

-----------------------------------------------------------------------------
-- FP3.4
-----------------------------------------------------------------------------

-- Evaluator for your µFP EDSL without support for partial application, 
-- lazy evaluation, pattern matching and higher order functions. 

type LUT = [(String, Val)]

data Context = C { prog :: Prog, vars :: LUT }

data Val = IntVal Integer | BoolVal Bool 
    deriving Show

eval :: Prog -> String -> [Integer] -> Integer
eval prog name intArgs = fromInt result
    where result = evalFun prog prog name (IntVal <$> intArgs)

-- Find and evaluate function value
evalFun :: Prog -> [Stmt] -> String -> [Val] -> Val
evalFun _ [] _ _ = error "Such function does not exist"
evalFun prog ((FunDecl n args expr):funs) name vals
    | n == name && argsMatch args vals = evalExpr ctx expr
    | otherwise = evalFun prog funs name vals
    where ctx = C prog (initLUT args vals)

-- Init look-up table for variables prior to function call
initLUT :: [Arg] -> [Val] -> LUT
initLUT [] [] = []
initLUT ((FixedArg _):xs) (_:ys) = initLUT xs ys
initLUT ((VarArg x):xs) (y:ys) = (x, y) : initLUT xs ys

-- Check if values match function's arguments
argsMatch :: [Arg] -> [Val] -> Bool
argsMatch args vals
    | length args /= length vals = False
    | otherwise = and $ zipWith match args vals
    where match (FixedArg x) y = x == fromInt y
          match (VarArg _) _ = True

-- Evaluate expression value
evalExpr :: Context -> Expr -> Val
evalExpr _   (Fixed x) = IntVal x
evalExpr ctx (Var   x) = findVar (vars ctx) x

evalExpr ctx (Cond cond e1 e2)
    | (\(BoolVal x) -> x) $ evalExpr ctx cond = evalExpr ctx e1
    | otherwise = evalExpr ctx e2

evalExpr ctx (FunCall name args) 
    = evalFun p p name ((evalExpr ctx) <$> args)
    where p = prog ctx

evalExpr ctx expr = case expr of
    (Less e1 e2) -> applyBool  (<) (evalExpr ctx e1) (evalExpr ctx e2)
    (Eq   e1 e2) -> applyBool (==) (evalExpr ctx e1) (evalExpr ctx e2)
    (More e1 e2) -> applyBool  (>) (evalExpr ctx e1) (evalExpr ctx e2)
    (Add  e1 e2) -> applyInt   (+) (evalExpr ctx e1) (evalExpr ctx e2)
    (Sub  e1 e2) -> applyInt   (-) (evalExpr ctx e1) (evalExpr ctx e2)
    (Mult e1 e2) -> applyInt   (*) (evalExpr ctx e1) (evalExpr ctx e2)

-- Binary operation on booleans
applyBool :: (Integer -> Integer -> Bool) -> Val -> Val -> Val
applyBool op (IntVal x) (IntVal y) = BoolVal $ op x y

-- Binary operation on integers 
applyInt :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
applyInt op (IntVal x) (IntVal y) = IntVal $ op x y

-- Find a variable value by its name
findVar :: LUT -> String -> Val
findVar [] _ = error "Such variable does not exist"
findVar ((x, val):xs) y
    | x == y = val
    | otherwise = findVar xs y

-- Value extractors
fromInt :: Val -> Integer
fromInt (IntVal x) = x

fromBool :: Val -> Bool
fromBool (BoolVal x) = x

-----------------------------------------------------------------------------
-- FP4.1
-----------------------------------------------------------------------------

-- type Prog = [Stmt]

-- data Stmt
--     = FunDecl String [Arg] Expr
--     deriving Show

-- data Arg 
--     = FixedArg  Integer 
--     | VarArg    String
--     deriving Show

-- data Expr
--     = Less      Expr Expr
--     | Eq        Expr Expr
--     | More      Expr Expr
--     | Add       Expr Expr
--     | Sub       Expr Expr
--     | Mult      Expr Expr
--     | Fixed     Integer
--     | Var       String
--     | Cond      Expr Expr Expr
--     | FunCall   String [Expr]
--     deriving Show

-- parseProg :: Parser Prog
-- parseProg [] = []
-- parseProg (x:xs) = parseStmt x : parseProg xs

-- parseStmt :: Parser Stmt
-- parseStmt = FunDecl
--     <$> identifier
--     <*> parseArgs
--     <*> parseExpr

-- parseFactor :: Parser Expr
-- parseFactor =

-- parseTerm :: Parser Expr
-- parseTerm =

-- parseExpr :: Parser Expr
-- parseExpr =

-----------------------------------------------------------------------------
-- FP4.2
-----------------------------------------------------------------------------

-- Parses and translates a textual representation 
-- of µFP to EDSL (a.k.a compilation).

-- compile :: String -> Prog
-- compile code = parse code

-----------------------------------------------------------------------------
-- FP4.3
-----------------------------------------------------------------------------

-- Reads the specified file, compiles it, and evaluates 
-- it to the µFP function. When the file contains multiple functions, 
-- the last function in the file is used.

-- runFile :: FilePath -> [Integer] -> IO Integer
-- runFile path args = evalLast <$> compile <$> readFile path

-----------------------------------------------------------------------------
-- Utils
-----------------------------------------------------------------------------

join :: String -> [String] -> String
join _ [x]      = x
join del (x:xs) = x ++ del ++ (join del xs)

-----------------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------------

prop_fibonacci = eval fibonacciProg "fibonacci" [10] == 55
prop_fib = eval fibProg "fib" [10] == 55
prop_sum = eval sumProg "sum" [8] == 36 
prop_div = eval divProg "div" [15, 7] == 2

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll