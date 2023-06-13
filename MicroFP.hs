-- Student 1: Aliaksei Kouzel (s2648563)
-- Student 2: Denis Krylov (s2808757)
-- Student 3: Serkan Akin (s2727218)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Data.Maybe
import Test.QuickCheck.All
import qualified Test.QuickCheck as QC
import Data.Char
import Data.List

-----------------------------------------------------------------------------
-- FP5.6
-- FP5.6
-----------------------------------------------------------------------------
-- The following declarations are used to define the EDSL as an instance of Arbitrary.
-- Two QuickCheck test are defined. They are ran once you run the "check command".

-- The following code generates random programs as well as 
-- contains QuickCheck helpers used throughout the file

data NonEmptyList a = NonEmptyList [a] deriving (Show)

data NonNegative = NonNeg Integer deriving (Eq, Show)

data Positive = Pos Integer deriving (Eq, Show)

-- EDSL as an instance of Arbitrary

-- Generates non-negative integers
instance QC.Arbitrary NonNegative where
    arbitrary = NonNeg <$> (abs <$> QC.arbitrary)

-- Generates positive integers
instance QC.Arbitrary Positive where
    arbitrary = Pos <$> (QC.getPositive <$> QC.arbitrary) 

-- Generates non-empty lists
instance QC.Arbitrary a => QC.Arbitrary (NonEmptyList a) where
    arbitrary = NonEmptyList <$> (QC.listOf1 QC.arbitrary)

-- Generates statements
instance QC.Arbitrary Stmt where
    arbitrary = FunDecl 
        <$> genName         -- function name
        <*> QC.arbitrary    -- arguments
        <*> QC.arbitrary    -- expression

-- Generates a random argument
instance QC.Arbitrary Arg where
    arbitrary = QC.oneof [
            VarArg <$> genIdentifier, 
            IntArg . genInteger <$> QC.arbitrary 
        ]

-- Generates a predicate operator
instance QC.Arbitrary PredOp where
    arbitrary = QC.oneof [pure Less, pure Eq, pure More]

-- Generates a random expression but with a constraint
instance QC.Arbitrary Expr where
    arbitrary = QC.sized expr 
        where 
            expr 0 = QC.oneof [
                    Fixed . genInteger <$> QC.arbitrary, 
                    Var <$> genIdentifier
                ]
            expr n = QC.oneof [
                    Add <$> nextExpr <*> nextExpr,
                    Sub <$> nextExpr <*> nextExpr,
                    Mult <$> nextExpr <*> nextExpr,
                    Fixed . genInteger <$> QC.arbitrary,
                    Var <$> genIdentifier,
                    Cond <$> nextPred <*> nextExpr <*> nextExpr,
                    FunCall <$> genName <*> QC.listOf nextExpr
                ]
                where 
                    nextExpr = expr (n `div` 100)
                    nextPred = (,,) 
                        <$> nextExpr 
                        <*> QC.arbitrary 
                        <*> nextExpr

-- Generates a name with length 3
genName :: QC.Gen String
genName = QC.vectorOf 3 $ QC.elements "fghk"

-- Generates an identifier with length 3
genIdentifier :: QC.Gen String
genIdentifier = QC.vectorOf 3 $ QC.elements "abcde"

-- Generates a positive integer
genInteger :: Positive -> Integer
genInteger (Pos n) = n

-- Generator tests
prop_expr :: Expr -> Bool
prop_expr expr = compileWith expression (pretty expr) == expr 

prop_prog :: QC.Property
prop_prog = QC.forAll (QC.resize 3 QC.arbitrary) 
    $ \(NonEmptyList prog) -> compile (pretty prog) == prog

-----------------------------------------------------------------------------
-- FP3.1
-----------------------------------------------------------------------------

-- A set of type/data constructors that represent 
-- an EDSL (deep embedding) for µFP.

type Prog = [Stmt] 

data Stmt
    = FunDecl String [Arg] Expr
    deriving (Show, Eq)

data Arg 
    = IntArg    Integer 
    | VarArg    String
    deriving (Show, Eq)

data Expr
    = Add       Expr Expr
    | Sub       Expr Expr
    | Mult      Expr Expr
    | Fixed     Integer
    | Var       String
    | Cond      Pred Expr Expr
    | FunCall   String [Expr]
    deriving (Show, Eq)

type Pred = (Expr, PredOp, Expr)

data PredOp
    = Less | Eq | More
    deriving (Show, Eq)

-----------------------------------------------------------------------------
-- FP3.2
-----------------------------------------------------------------------------

-- The following functions in µFP EDSL correspond 
-- to the definitions in functions.txt.

-- Program that calculates a fibonacci number
-- (uses pattern matching)
prog_fibonacci :: Prog
prog_fibonacci = [
    FunDecl "fibonacci" [IntArg 0] (Fixed 0),
    FunDecl "fibonacci" [IntArg 1] (Fixed 1),
    FunDecl "fibonacci" [VarArg "n"] (Add addL addR) ]
    where
        addL = FunCall "fibonacci" [Sub (Var "n") (Fixed 1)]
        addR = FunCall "fibonacci" [Sub (Var "n") (Fixed 2)]

-- Program that calculates a fibonacci number
prog_fib :: Prog
prog_fib = [ FunDecl "fib" [VarArg "n"] body ]
    where
        pred = (Var "n", Less, Fixed 3)
        body = Cond pred (Fixed 1) (Add addL addR)
        addL = FunCall "fib" [Sub (Var "n") (Fixed 1)]
        addR = FunCall "fib" [Sub (Var "n") (Fixed 2)]

-- Program that calculates a sum of integers from 1 to "a"
-- (uses pattern matching)
prog_sum :: Prog
prog_sum = [ 
    FunDecl "sum" [IntArg 0] (Fixed 0),
    FunDecl "sum" [VarArg "a"] (Add recSum (Var "a")) ]
    where 
        recSum = FunCall "sum" [Sub (Var "a") (Fixed 1)]

-- Program that calculates div on two integers
prog_div :: Prog
prog_div = [ FunDecl "div" [VarArg "x", VarArg "y"] body ]
    where
        divcall = FunCall "div" [Sub (Var "x") (Var "y"), Var "y"]
        pred = (Var "x", Less, Var "y")
        body = (Cond pred (Fixed 0) (Add (Fixed 1) divcall))

-- Program that applies a function twice passed as argument
-- (it is a higher order function) 
prog_twice :: Prog
prog_twice = [ 
    FunDecl "twice" [VarArg "f", VarArg "x"] 
    (FunCall "f" [FunCall "f" [Var "x"]]) ]

-- Program that calculates "11" 
-- using partially applied functions 
prog_comb :: Prog
prog_comb = [
    FunDecl "add" [VarArg "x", VarArg "y"] (Add (Var "x") (Var "y")),
    FunDecl "inc" [] (FunCall "add" [Fixed 1]),
    FunDecl "eleven" [] (FunCall "inc" [Fixed 10]) ]

-----------------------------------------------------------------------------
-- FP3.3
-----------------------------------------------------------------------------

-- Pretty printer that generates a textual representation 
-- that corresponds to the grammar of µFP. 

class Pretty a where
    pretty :: a -> String 

instance Pretty Prog where
    pretty = unwords . map pretty

instance Pretty Stmt where
    pretty (FunDecl id args expr)
        = unwords [id, prettyArgs, ":=", pretty expr, ";"]
        where prettyArgs
                | length args == 0 = ""
                | otherwise = unwords $ map pretty args

instance Pretty Arg where
    pretty (IntArg x) = show x
    pretty (VarArg x) = x

instance Pretty Expr where
    pretty (Fixed  x) = show x
    pretty (Var    x) = x
    pretty (Add  x y) = "(" ++ prettyJoin x " + " y ++ ")"
    pretty (Sub  x y) = "(" ++ prettyJoin x " - " y ++ ")" 
    pretty (Mult x y) = "(" ++ prettyJoin x " * " y ++ ")" 

    pretty (Cond pred e1 e2) 
        = "if (" ++ pretty pred ++ ")"
        ++ " then { " ++ pretty e1 ++ " }"
        ++ " else { " ++ pretty e2 ++ " }"

    pretty (FunCall id []) 
        = id ++ " ()"

    pretty (FunCall id args) 
        = id ++ " (" ++ prettyArgs ++ ")"
        where prettyArgs
                | length args == 0 = ""
                | otherwise = join ", " $ map pretty args

instance Pretty Pred where
    pretty (e1, op, e2) = prettyJoin e1 (pretty op) e2

instance Pretty PredOp where
    pretty Less = " < "
    pretty More = " > "
    pretty Eq = " == "

prettyJoin :: Pretty a => a -> String -> a -> String
prettyJoin s1 sep s2 = pretty s1 ++ sep ++ pretty s2

-----------------------------------------------------------------------------
-- FP5.4 (partial application)
-----------------------------------------------------------------------------

-- Partial application is implemented by preprocessing the program
-- before evaluation and adding "ghost" arguments to complete 
-- partially applied functions

preEval :: Prog -> Prog
preEval prog = prePartial [] prog

-- Completes partially applied functions
prePartial :: Prog -> Prog -> Prog
prePartial _ [] = []
prePartial prog (x:xs) = y : prePartial (y:prog) xs 
    where y = preStmt prog x

-- If a function has a function call as its expression,
-- then add "ghost" arguments if it is partially applied
preStmt :: Prog -> Stmt -> Stmt
preStmt prog decl@(FunDecl name args (FunCall id vals))
    | not (isArg id args) && valsLeft /= 0 = fullDecl
    | otherwise = decl
    where 
        valsLeft  = arity prog id - length vals
        ghostArgs = genGhost valsLeft
        fullArgs  = args ++ (VarArg <$> ghostArgs)
        fullVals  = vals ++ (Var <$> ghostArgs)
        fullDecl  = FunDecl name fullArgs (FunCall id fullVals)

-- If an expression is not a function call, 
-- then there cannot be partial application
preStmt _ stmt = stmt

-- Checks if arguments contain the given id
isArg :: String -> [Arg] -> Bool
isArg id ((IntArg _):xs) = isArg id xs
isArg id ((VarArg name):xs) = id == name || isArg id xs
isArg id [] = False

-- Generates "ghost" arguments for a partially applied function
genGhost :: Int -> [String]
genGhost 0 = []
genGhost num = ("_" ++ show num) : genGhost (num - 1)

-- Gets function arity by its name
arity :: Prog -> String -> Int
arity [] id = error $ "Such function doesn't exist: " ++ id
arity ((FunDecl name args _):xs) id
    | name == id = length args
    | otherwise  = arity xs id

-----------------------------------------------------------------------------
-- FP3.4 -- FP5.2 -- FP5.5
-----------------------------------------------------------------------------

-- Evaluator for µFP EDSL with support for pattern matching
-- and higher order functions.

-- Context contains variables and the program itself (text)
data Context = Ctx { vars :: LUT, text :: Prog }

-- Look-up table that contains variable values
type LUT = [(String, Val)]

-- Variable values
data Val
    = Fun String        -- function reference ( -- FP5.5 )
    | Int Integer       -- Integer value
    deriving (Show, Eq)

-- Finds and evaluates a function in the program
-- and produces an integer
eval :: Prog -> String -> [Integer] -> Integer
eval rawProg name args = (\(Int x) -> x) result
    where 
        result = evalFun prog name (Int <$> args)
        prog = preEval rawProg

-- Evaluates a function
evalFun :: Prog -> String -> [Val] -> Val
evalFun prog id vals = evalExpr ctx expr
    where 
        FunDecl _ args expr = findFun prog id vals
        ctx = Ctx (bindVars args vals) prog

-- Evaluates an expression
evalExpr :: Context -> Expr -> Val
evalExpr _ (Fixed x) = Int x

-- Evaluates a variable. if it's not found in LUT, 
-- then a global function is passed
evalExpr ctx (Var name) = case var of
    Just val -> val
    Nothing  -> Fun name
    where var = findVar (vars ctx) name

-- Evaluates an if/else condition
evalExpr ctx (Cond pred e1 e2)
    | evalPred ctx pred = evalExpr ctx e1
    | otherwise = evalExpr ctx e2

-- Evaluates a function call
evalExpr ctx (FunCall id args)
    = evalFun (text ctx) name (evalExpr ctx <$> args)
    where 
        -- Function is either global 
        -- or passed as a variable
        name = case findVar (vars ctx) id of
            Just (Fun name) -> name
            _               -> id

-- Evaluates an arithmetic operation
evalExpr ctx expr = case expr of
    (Add  e1 e2) -> Int $ evalBin (+) ctx e1 e2
    (Sub  e1 e2) -> Int $ evalBin (-) ctx e1 e2
    (Mult e1 e2) -> Int $ evalBin (*) ctx e1 e2

-- Evaluates a predicate
evalPred :: Context -> Pred -> Bool
evalPred ctx pred = case pred of
    (e1, Less, e2) -> evalBin (<) ctx e1 e2
    (e1, More, e2) -> evalBin (>) ctx e1 e2
    (e1, Eq,   e2) -> evalBin (==) ctx e1 e2

-- Evaluates a binary expression
evalBin :: (Integer -> Integer -> a) -> Context -> Expr -> Expr -> a
evalBin apply ctx e1 e2 = apply a b
    where
        Int a = evalExpr ctx e1
        Int b = evalExpr ctx e2

-- Binds variables to values prior to function call
bindVars :: [Arg] -> [Val] -> LUT
bindVars [] [] = []
bindVars ((IntArg _):xs) (_:ys) = bindVars xs ys
bindVars ((VarArg x):xs) (y:ys) = (x, y) : bindVars xs ys

-- Finds a variable value in the loop-up table by its name
findVar :: LUT -> String -> Maybe Val
findVar [] _ = Nothing
findVar ((nx,x):xs) n
    | nx /= n = findVar xs n
    | otherwise = Just x

-- Finds a function declaration using pattern matching ( -- FP5.2 )
findFun :: Prog -> String -> [Val] -> Stmt
findFun [] id vals = error $ unwords ["No match found:", id]
findFun (fun@(FunDecl name args expr):funs) id vals
    | name == id && funMatch args vals = fun
    | otherwise = findFun funs id vals

-- Checks if values match function's arguments
funMatch :: [Arg] -> [Val] -> Bool
funMatch args vals
    | length args /= length vals = False
    | otherwise = and $ zipWith match args vals
    where 
        match (VarArg _) _ = True
        match (IntArg x) val = case val of
            (Int y) -> x == y
            _       -> False

-- Testing evaluator on functions from "functions.txt"
prop_eval_fibonacci :: Bool
prop_eval_fibonacci = eval prog_fibonacci "fibonacci" [10] == 55

prop_eval_fib :: Bool
prop_eval_fib = eval prog_fib "fib" [10] == 55

prop_eval_sum :: NonNegative -> Bool
prop_eval_sum (NonNeg a) = eval prog_sum  "sum" [a] == test_sum a

prop_eval_div :: Positive -> Positive -> Bool
prop_eval_div (Pos a) (Pos b) = eval prog_div  "div" [a, b] == a `div` b

prop_eval_add :: Integer -> Integer -> Bool
prop_eval_add a b = eval prog_comb "add" [a, b] == a + b

prop_eval_inc :: Integer -> Bool
prop_eval_inc a = eval prog_comb "inc" [a] == a + 1

prop_eval_eleven :: Bool
prop_eval_eleven = eval prog_comb "eleven" [] == 11

test_sum :: Integer -> Integer
test_sum 0 = 0
test_sum n = n + (test_sum $ n - 1)

-----------------------------------------------------------------------------
-- FP4.1
-----------------------------------------------------------------------------

program :: Parser Prog
program = some statement <?> "'function declaration'"

statement :: Parser Stmt
statement = FunDecl
    <$> identifier
    <*> many argument
    <*  (symbol ":=" <?> "':='") <*> expression
    <*  (char ';' <?> ";")

argument :: Parser Arg
argument =  (IntArg <$> integer)
        <|> (VarArg <$> identifier)

expression :: Parser Expr
expression = (whitespace $ term `chain` op)
    where op = (Add <$ symbol "+")
           <|> (Sub <$ symbol "-")

term :: Parser Expr
term = factor `chain` op
    where op = (Mult <$ symbol "*")

factor :: Parser Expr
factor = condition
    <|> parens expression
    <|> funCall
    <|> Var   <$> identifier
    <|> Fixed <$> integer

funCall :: Parser Expr
funCall = FunCall
    <$> identifier
    <*> (parens $ sep expression $ symbol ",")

condition :: Parser Expr
condition = Cond
    <$ symbol "if" <*> parens predicate
    <* symbol "then" <*> braces expression
    <* symbol "else" <*> braces expression

predicate :: Parser Pred
predicate = (,,)
    <$> expression
    <*> predicateOp
    <*> expression

predicateOp :: Parser PredOp
predicateOp = 
        Eq <$ symbol "=="
    <|>  Less <$ symbol "<"
    <|> More <$ symbol ">" 
    <?> "operation < or > or =="

-- Chains expressions like:
-- 2 + 2 + 2  => Add 2 (Add 2 (Add 2))
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = reorder <$> p <*> op <*> chain p op <|> p
  where reorder x f y = f x y

-----------------------------------------------------------------------------
-- FP4.2 -- FP5.1
-----------------------------------------------------------------------------

-- Compiles a µFP program
compile :: String -> Prog
compile code = compileWith program code

-- Parses a textual representation of µFP to EDSL
compileWith :: Parser a -> String -> a
compileWith parser code = fst $ head 
    $ (\(Result r) -> r) 
    $ parse parser
    $ Stream code initScanner

-- Compilation tests
prop_compile_fibonacci = prog_fibonacci == (compile $ pretty prog_fibonacci)
prop_compile_fib  = prog_fib  == (compile $ pretty prog_fib)
prop_compile_sum  = prog_sum  == (compile $ pretty prog_sum)
prop_compile_div  = prog_div  == (compile $ pretty prog_div)
prop_compile_comb = prog_comb == (compile $ pretty prog_comb)

-----------------------------------------------------------------------------
-- FP4.3
-----------------------------------------------------------------------------

-- Reads the specified file, compiles and evaluates it.
-- When the file contains multiple functions, the last one is used
runFile :: FilePath -> [Integer] -> IO Integer
runFile path args = evalLast args <$> compile <$> readFile path

-- Evaluates the last function of the program
evalLast :: [Integer] -> Prog -> Integer
evalLast args prog = eval prog name args
    where name = (\(FunDecl name _ _) -> name) $ last prog

-----------------------------------------------------------------------------
-- FP5.3
-----------------------------------------------------------------------------

-- Applies patmatch on each group of function declarations. 
-- However, it will fail if functions are declared in mixed order
patmatch :: Prog -> Prog
patmatch prog = concatMap patmatchFunc (groupFuncs prog)

-- Groups function declarations by their name
groupFuncs :: Prog -> [Prog]
groupFuncs = groupBy sameFunDecl . sortOn funDeclName
    where
        funDeclName (FunDecl name _ _) = name
        sameFunDecl (FunDecl name1 _ _) (FunDecl name2 _ _) = name1 == name2

-- Gets a program and returns a program in if-else form
patmatchFunc :: Prog -> Prog
patmatchFunc ((FunDecl name [] expr):fs) 
           = [(FunDecl name [VarArg ""] expr)]

patmatchFunc ((FunDecl name [(VarArg x)] expr):fs) 
           = [(FunDecl name [VarArg x] expr)]

patmatchFunc ((FunDecl name (x:y:xs) expr):fs) 
           = [(FunDecl name (x:y:xs) expr)]

patmatchFunc ((FunDecl name ((IntArg x):_) expr):fs) 
           = [(FunDecl name [VarArg var] cond)]
    where
        cond = Cond pred expr $ matchRest fs
        pred = (Var var, Eq, Fixed x)
        var = varProg fs

-- After the initial statement is converted 
-- the rest will be converted here
matchRest :: [Stmt] -> Expr
matchRest ((FunDecl _ ((VarArg x):_) expr):_) = expr 
matchRest ((FunDecl _ ((IntArg x):_) expr):fs) = cond
    where 
        cond = Cond pred expr $ matchRest fs
        pred = (Var $ varProg fs, Eq, Fixed x)

-- Finds a function call where variable is not a digit 
-- and returns that letter
varProg :: Prog -> String
varProg [] = " "
varProg ((FunDecl _ ((VarArg x):_) _):fs) = x
varProg ((FunDecl _ ((IntArg _):_) _):fs) = varProg fs

-- Tests for fibonacci and sum with "patmatch"
prop_eval_patmatch_fib = eval (patmatch prog_fibonacci) "fibonacci" [10] == 55
prop_eval_patmatch_sum = eval (patmatch prog_sum)  "sum" [8] == 36

-----------------------------------------------------------------------------
-- Utils
-----------------------------------------------------------------------------

-- Joins strings using a separator
-- e.g. join ", " ["Hi", "John"] = "Hi, John"
join :: String -> [String] -> String
join _ [x]      = x
join sep (x:xs) = x ++ sep ++ (join sep xs)

-- Runs all tests
return []
check = $quickCheckAll