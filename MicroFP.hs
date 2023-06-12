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

prog_fibonacci :: Prog
prog_fibonacci = [
    FunDecl "fibonacci" [IntArg 0] (Fixed 0),
    FunDecl "fibonacci" [IntArg 1] (Fixed 1),
    FunDecl "fibonacci" [VarArg "n"] (Add addL addR) ]
    where
        addL = FunCall "fibonacci" [Sub (Var "n") (Fixed 1)]
        addR = FunCall "fibonacci" [Sub (Var "n") (Fixed 2)]

prog_fib :: Prog
prog_fib = [ FunDecl "fib" [VarArg "n"] body ]
    where
        pred = (Var "n", Less, Fixed 3)
        body = Cond pred (Fixed 1) (Add addL addR)
        addL = FunCall "fib" [Sub (Var "n") (Fixed 1)]
        addR = FunCall "fib" [Sub (Var "n") (Fixed 2)]

prog_sum :: Prog
prog_sum = [ 
    FunDecl "sum" [IntArg 0] (Fixed 0),
    FunDecl "sum" [VarArg "a"] (Add recSum (Var "a")) ]
    where 
        recSum = FunCall "sum" [Sub (Var "a") (Fixed 1)]

prog_div :: Prog
prog_div = [ FunDecl "div" [VarArg "x", VarArg "y"] body ]
    where
        divcall = FunCall "div" [Sub (Var "x") (Var "y"), Var "y"]
        pred = (Var "x", Less, Var "y")
        body = (Cond pred (Fixed 0) (Add (Fixed 1) divcall))

prog_twice :: Prog
prog_twice = [ 
    FunDecl "twice" [VarArg "f", VarArg "x"] 
    (FunCall "f" [FunCall "f" [Var "x"]]) ]

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
        where prettyArgs = unwords $ map pretty args

instance Pretty Arg where
    pretty (IntArg x) = show x
    pretty (VarArg x) = x

instance Pretty Expr where
    pretty (Fixed  x) = show x
    pretty (Var    x) = x
    pretty (Add  x y) = prettyJoin x " + " y 
    pretty (Sub  x y) = prettyJoin x " - " y 
    pretty (Mult x y) = prettyJoin x " * " y 

    pretty (Cond pred e1 e2) 
        = "if (" ++ pretty pred ++ ")"
        ++ " then { " ++ pretty e1 ++ " }"
        ++ " else { " ++ pretty e2 ++ " }"

    pretty (FunCall id args) 
        = id ++ " (" ++ prettyArgs ++ ")"
        where prettyArgs = join ", " $ map pretty args

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

-- Program preprocessing before evaluation
-- (e.g. support for partial application)
preEval :: Prog -> Prog
preEval prog = prePartial [] prog

-- Completes partially applied functions
prePartial :: Prog -> Prog -> Prog
prePartial _ [] = []
prePartial prog (x:xs) = y : prePartial (y:prog) xs 
    where y = preStmt prog x

-- If a function has a function call as its expression,
-- then add missing arguments if it is partially applied
preStmt :: Prog -> Stmt -> Stmt
preStmt prog decl@(FunDecl name args (FunCall id vals))
    | not (isArg id args) && valsLeft /= 0 = fullDecl
    | otherwise = decl
    where 
        valsLeft  = arity prog id - length vals
        ghostArgs = genArgs valsLeft
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

-- Generates missing arguments for 
-- a partially applied function
genArgs :: Int -> [String]
genArgs 0 = []
genArgs num = ("_" ++ show num) : genArgs (num - 1)

-- Gets function arity by its name
arity :: Prog -> String -> Int
arity [] id = error $ "Such function doesn't exist: " ++ id
arity ((FunDecl name args _):xs) id
    | name == id = length args
    | otherwise  = arity xs id

-----------------------------------------------------------------------------
-- FP3.4 ; FP5.2 ; FP5.5
-----------------------------------------------------------------------------

-- Context contains variables and the program itself
data Context = Ctx { vars :: LUT, text :: Prog }

-- Look-up table that contains variable values
type LUT = [(String, Val)]

-- "Val" is what variables can contain, it can be either
-- an integer or a function reference ( FP5.5 ) 
data Val
    = Int Integer
    | Fun String
    deriving (Show, Eq)

-- Evaluator for µFP EDSL
eval :: Prog -> String -> [Integer] -> Integer
eval rawProg name args = (\(Int x) -> x) result
    where 
        result = evalFun prog prog name (Int <$> args)
        prog = preEval rawProg

-- Evaluates the first function in the program 
-- by using pattern matching ( FP5.2 ) 
evalFun :: Prog -> Prog -> String -> [Val] -> Val
evalFun _ [] id vals = error $ unwords ["No match found:", id, show vals]
evalFun prog ((FunDecl name args expr):funs) id vals
    | name == id && funMatch args vals = result
    | otherwise = evalFun prog funs id vals
    where 
        result = evalExpr (Ctx vars prog) expr
        vars = bindVals args vals

evalExpr :: Context -> Expr -> Val
evalExpr _ (Fixed x) = Int x

-- Evaluates a variable as either an integer 
-- or a function reference
evalExpr ctx (Var name) = case var of
    Just val -> val
    Nothing  -> Fun name
    where var = findVar (vars ctx) name

-- Evaluates an if/else condition
evalExpr ctx (Cond pred e1 e2)
    | evalPred ctx pred = evalExpr ctx e1
    | otherwise = evalExpr ctx e2

-- Evaluates a function call as either an argument of 
-- a higher order function ( FP5.5 ), or as a global function
evalExpr ctx (FunCall id args)
    = evalFun prog prog funId (evalExpr ctx <$> args)
    where 
        prog = text ctx
        funId = case findVar (vars ctx) id of
            Just (Fun name) -> name
            _ -> id

-- Evaluates a binary expression
evalExpr ctx expr = case expr of
    (Add  e1 e2) -> apply (+) (evalExpr ctx e1) (evalExpr ctx e2)
    (Sub  e1 e2) -> apply (-) (evalExpr ctx e1) (evalExpr ctx e2)
    (Mult e1 e2) -> apply (*) (evalExpr ctx e1) (evalExpr ctx e2)
    where apply op (Int x) (Int y) = Int $ op x y

evalPred :: Context -> Pred -> Bool
evalPred ctx pred = case pred of
    (e1, Less, e2) -> apply (<)  (evalExpr ctx e1) (evalExpr ctx e2)
    (e1, More, e2) -> apply (>)  (evalExpr ctx e1) (evalExpr ctx e2)
    (e1, Eq,   e2) -> apply (==) (evalExpr ctx e1) (evalExpr ctx e2)
    where apply op (Int x) (Int y) = op x y

-- Binds values to arguments prior to function call
bindVals :: [Arg] -> [Val] -> LUT
bindVals [] [] = []
bindVals ((IntArg _):xs) (_:ys) = bindVals xs ys
bindVals ((VarArg x):xs) (y:ys) = (x, y) : bindVals xs ys

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

-- Finds variable value in LUT by its name
findVar :: LUT -> String -> Maybe Val
findVar [] _ = Nothing
findVar ((nx,x):xs) n
    | nx /= n = findVar xs n
    | otherwise = Just x

-- Testing evaluators
prop_eval_fib1 = eval prog_fibonacci "fibonacci" [10] == 55
prop_eval_fib2 = eval prog_fib  "fib" [10] == 55
prop_eval_sum  = eval prog_sum  "sum" [8] == 36 
prop_eval_div  = eval prog_div  "div" [15, 7] == 2
prop_eval_add  = eval prog_comb "add" [10, 40] == 50
prop_eval_inc  = eval prog_comb "inc" [100] == 101
prop_eval_elvn = eval prog_comb "eleven" [] == 11

-----------------------------------------------------------------------------
-- FP4.1
-----------------------------------------------------------------------------

program :: Parser Prog
program = some statement

statement :: Parser Stmt
statement = FunDecl
    <$> (identifier <?> "identifier")
    <*> (many argument <?> "arguments")
    <*  symbol ":=" <*> (expression <?> "expression")
    <*  char ';'

argument :: Parser Arg
argument =  IntArg <$> (integer <?> "integer or identifier")
        <|> VarArg <$> (identifier <?> "integer or identifier")

expression :: Parser Expr
expression = whitespace $ term `chain` op
    where op = (Add <$ symbol "+" <?> "+ or -")
           <|> (Sub <$ symbol "-" <?> "+ or -")

term :: Parser Expr
term = factor `chain` op
    where op = Mult <$ (symbol "*" <?> "*")

factor :: Parser Expr
factor = (condition <?> msg)
    <|> (parens expression <?> msg)
    <|> (funCall <?> msg)
    <|> Var   <$> (identifier <?> msg)
    <|> Fixed <$> (integer <?> msg)
    where msg = "condition or expression or function call or identifier or integer"

funCall :: Parser Expr
funCall = FunCall
    <$> (identifier <?> "identifier")
    <*> (parens $ sep expression $ symbol "," <?> "expressions")

condition :: Parser Expr
condition = Cond
    <$ (symbol "if" <?> "if") <*> (parens predicate <?> "predicate")
    <* (symbol "then" <?> "then") <*> (braces expression <?> "expression")
    <* (symbol "else" <?> "else") <*> (braces expression <?> "expression")

predicate :: Parser Pred
predicate = (,,)
    <$> (expression <?> "expression")
    <*> (predicateOp <?> "predicate operation")
    <*> (expression <?> "expression")

predicateOp :: Parser PredOp
predicateOp = 
        Less <$ (symbol "<" <?> msg)
    <|> More <$ (symbol ">" <?> msg)
    <|> Less <$ (symbol "==" <?> msg)
    where msg = "< or > or =="

-- Chains expressions like this:
-- 2 + 2 + 2  => Add 2 (Add 2 (Add 2))
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = reorder <$> p <*> op <*> chain p op <|> p
  where reorder x f y = f x y

-----------------------------------------------------------------------------
-- FP4.2, FP5.1
-----------------------------------------------------------------------------

-- Parsing a textual representation of µFP to EDSL.
compile :: String -> Prog
compile code = fst $ head $ getResult $ parse program code

-- -- -- Testing
prop_compile_fibonacci = prog_fibonacci == (compile $ pretty prog_fibonacci)
prop_compile_fib  = prog_fib  == (compile $ pretty prog_fib)
prop_compile_sum  = prog_sum  == (compile $ pretty prog_sum)
prop_compile_div  = prog_div  == (compile $ pretty prog_div)
prop_compile_comb = prog_comb == (compile $ pretty prog_comb)

-----------------------------------------------------------------------------
-- FP4.3
-----------------------------------------------------------------------------

-- Reading the specified file, compile it, and evaluate it to 
-- the µFP function. When the file contains multiple functions, 
-- the last function in the file is used.

runFile :: FilePath -> [Integer] -> IO Integer
runFile path args = evalLast args <$> compile <$> readFile path

evalLast :: [Integer] -> Prog -> Integer
evalLast args prog = eval prog name args
    where name = (\(FunDecl name _ _) -> name) $ last prog

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