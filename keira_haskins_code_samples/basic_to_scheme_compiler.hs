module Main where
{-

  Author     : Keira Haskins
  Class      : CS 456 - Advanced Declarative Programming
  Synopsis   : Attempt at implementing a BASIC to Scheme compiler.

  References :
  1.  https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora058.html
  2.  http://www.cs.unm.edu/~williams/cs357/compiler.html

-}
import Text.Parsec.Char
import Text.Parsec.String
import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import Data.IORef
import Data.List

data BInt       = Integer Integer
                  deriving (Show)

data BStr       = Str String
                  deriving (Show)

data BLine      = Line BInt Statement
                  deriving (Show)

data ID         = Letter Char
                  deriving (Show)

data Array      = Array ID Char Expression Char deriving (Show)

data Var        = Var Char
                | Arr Array
                  deriving (Show)

data Factor     = VarFactor Var
                | BasicIntFactor BInt
                | ExprFactor Expression
                  deriving (Show)

data Term       = BareTerm Factor
                | MultTerm Factor Factor
                | DivTerm Factor Factor
                  deriving (Show)

data Expression = BareExpr Term
                | MinusExpr Term Term
                | UnaryPlusExpr Term
                | UnaryMinusExpr Term
                | PlusExpr Term Term
                | ExprList [Expression]
                  deriving (Show)

data ArrayList  = ArrayList Array Char ArrayList deriving (Show)

data IDList     = ID Char IDList deriving (Show)

data PrintList  = PrintListComm Expression Char [PrintList]
                | PrintListColn Expression Char [PrintList]
                  deriving (Show)

data Statement  = PRINT Expression
                | PRINTl PrintList
                | LET Var Expression
                | IF Expression String Expression Statement
                | GOTO Expression
                | GOSUB Expression
                | CLEAR
                | RETURN
                | DIM ArrayList
                | END
                | FOR ID Char Expression BStr Expression
                | FOR2 ID Char Expression BStr Expression BStr Expression
                | INPUT BStr ID IDList
                  deriving (Show)

data Statements = Statements [Statement]

data Const      = BInt
                | BStr

parens :: Parser a -> Parser a
parens m = do
  _ <- char '('
  n <- m
  _ <- char ')'
  return n

pInteger :: Parser BInt
pInteger = Integer . read <$> many1 digit

pString :: Parser BStr
pString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ Str x

pVar :: Parser Var
pVar = Var <$> upper

parseFactor :: Parser Factor
parseFactor =
  pVarFact           <|>
  pBIntFact          <|>
  (parens pExprFact)

pVarFact :: Parser Factor
pVarFact = VarFactor <$> pVar

pBIntFact :: Parser Factor
pBIntFact = BasicIntFactor <$> pInteger

pExprFact :: Parser Factor
pExprFact = do
  expression <- parseExpression
  return $ ExprFactor expression

parseTerm :: Parser Term
parseTerm =
  try pMultTerm <|>
  try pDivTerm  <|>
  pBareTerm

pBareTerm :: Parser Term
pBareTerm = BareTerm <$> parseFactor

pBinTerm :: Char -> (Factor -> Factor -> Term) -> Parser Term
pBinTerm op typeclass = do
  left  <- parseFactor
  spaces
  _     <- char op
  spaces
  right <- parseFactor
  return $ typeclass left right

pMultTerm = pBinTerm '*' MultTerm

pDivTerm  = pBinTerm '/' DivTerm

parseExpression :: Parser Expression
parseExpression =
  try pPlusExpr       <|>
  try pMinusExpr      <|>
  try pUnaryPlusExpr  <|>
  try pUnaryMinusExpr <|>
  pBareExpr

pBareExpr :: Parser Expression
pBareExpr = do
  term <- parseTerm
  return $ BareExpr term

pBinaryExpr :: Char -> (Term -> Term -> Expression) -> Parser Expression
pBinaryExpr op typeclass = do
  left  <- parseTerm
  spaces
  _     <- char op
  spaces
  right <- parseTerm
  return $ typeclass left right

pPlusExpr :: Parser Expression
pPlusExpr = pBinaryExpr '+' PlusExpr

pMinusExpr :: Parser Expression
pMinusExpr = pBinaryExpr '-' MinusExpr

pUnaryExpr :: Char -> (Term -> Expression) -> Parser Expression
pUnaryExpr op typeclass = do
  _    <- char op
  spaces
  term <- parseTerm
  return $ typeclass term

pUnaryPlusExpr :: Parser Expression
pUnaryPlusExpr = pUnaryExpr '+' UnaryPlusExpr

pUnaryMinusExpr :: Parser Expression
pUnaryMinusExpr = pUnaryExpr '-' UnaryMinusExpr

parseStatement :: Parser Statement
parseStatement =
  pPrintStmt    <|>
  pLetStmt      <|>
  pIfStmt       <|>
  try pGotoStmt <|>
  pGoSubStmt    <|>
  pReturnStmt   <|>
  pClearStmt    <|>
  pEndStmt

pPrintStmt :: Parser Statement
pPrintStmt = do
  _ <- string "PRINT"
  spaces
  expression <- parseExpression
  return $ PRINT expression

pLetStmt :: Parser Statement
pLetStmt = do
  _ <- string "LET"
  spaces
  var <- pVar
  spaces
  _ <- char '='
  spaces
  expression <- parseExpression
  return $ LET var expression

pRelOp :: Parser String
pRelOp =
  (try $ string "<=") <|>
  (try $ string ">=") <|>
  (try $ string "<>") <|>
  string        "<"   <|>
  string        ">"   <|>
  string        "="

pIfStmt :: Parser Statement
pIfStmt = do
  _ <- string "IF"
  spaces
  left  <- parseExpression
  spaces
  relop <- pRelOp
  spaces
  right <- parseExpression
  spaces
  _ <- string "THEN"
  spaces
  statement <- parseStatement
  return $ IF left relop right statement

pGotoStmt :: Parser Statement
pGotoStmt = do
  _ <- string "GOTO"
  spaces
  expression <- parseExpression
  return $ GOTO expression

pGoSubStmt :: Parser Statement
pGoSubStmt = do
  _ <- string "GOSUB"
  spaces
  expression <- parseExpression
  return $ GOSUB expression

pClearStmt :: Parser Statement
pClearStmt = do
  _ <- string "CLEAR"
  return CLEAR

pReturnStmt :: Parser Statement
pReturnStmt = do
  _ <- string "RETURN"
  return RETURN

pEndStmt :: Parser Statement
pEndStmt = do
  _ <- string "END"
  return END

pLine :: Parser BLine
pLine = do
  number    <- pInteger
  spaces
  statement <- parseStatement
  return $ Line number statement

parseSRC :: [String] -> Either ParseError [BLine]
parseSRC program = helper program []
  where
    helper [] acc          = Right acc
    helper (line:rest) acc = case (parse pLine "" line) of
      Right line2          -> helper rest (acc ++ [line2])
      Left  err            -> Left err

-- ****************
-- *** Compiler ***
-- ****************

-- *** Scheme Data types and a few functions borrowed from [2] ***
-- *** Not all being implemented currently.                    ***

data Sexpr = Left'
           | Right'
           | Period
           | Symbol String
           | Number Integer
           | Boolean Bool
           | Nil
           | Cons {car :: Sexpr, cdr :: Sexpr}
           | Closure {args :: [Sexpr], body :: [Sexpr], env :: Env}
           | Unary {name :: String, func1 :: Sexpr -> Sexpr}
           | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr} | Void | Error

type Binding = (Sexpr, Sexpr)
type Frame   = [Binding]
type Env     = [Frame]

instance Eq Sexpr where
    Left' == Left' = True
    Right' == Right' = True
    Period == Period = True
    (Symbol x) == (Symbol y) = (x == y)
    (Number x) == (Number y) = (x == y)
    (Boolean x) == (Boolean y) = (x == y)
    Nil == Nil = True
    (Unary x f) == (Unary y g) = (x == y)
    (Binary x f) == (Binary y g) = (x == y)
    Void == Void = True
    Error == Error = True
    x == y = False

instance Show Sexpr where
  show (Left') = "Left"
  show (Right') = "Right"
  show (Period) = "Period"
  show (Symbol x) = x
  show (Number x) = (show x)
  show (Boolean x) = if x == True then "#t" else "#f"
  show (Nil) = "()"
  show (Cons x y) = "(" ++ (show x) ++ (showCdr y) ++ ")"
  show (Closure _ _ _) = "#"
  show (Unary x _) = "#"
  show (Binary x _) = "#"
  show (Void) = "#"
  show (Error) = "Error"

showCdr :: Sexpr -> String
showCdr (Nil) = ""
showCdr (Cons x Nil) = " " ++ (show x)
showCdr (Cons x v@(Cons y z)) = " " ++ (show x) ++ (showCdr v)
showCdr (Cons x y) = " " ++ (show x) ++ " . " ++ (show y)
showCdr x = " . " ++ (show x)

data VarInfo = VarInfo {dat  :: Sexpr}
data FunInfo = FunInfo {datF :: Sexpr}

data Environment = Env {
  ast  :: [BLine] ,
  code :: [Sexpr] ,
  vars :: [[(Sexpr, VarInfo)]],
  funs :: [[(Sexpr, FunInfo)]]
                       }

initEnv :: [BLine] -> Environment
initEnv tree = Env {
  ast    = tree,
  code   = [],
  vars   = [[]],
  funs   = [[]]
                    }

-- ***********************************************************************
-- ***********************************************************************

select _ [] [] = []
select pred (x:xs) (y:ys) = if (pred x)
  then y:rest else rest where rest = select pred xs ys

unary :: String -> (Sexpr -> Sexpr) -> Binding
unary name func1 = ((Symbol name), (Unary name func1))

binary :: String -> (Sexpr -> Sexpr -> Sexpr) -> Binding
binary name func2 = ((Symbol name), (Binary name func2))

arithmetic :: String -> (Integer -> Integer -> Integer) -> Binding
arithmetic name op = binary name func2
  where func2 (Number x) (Number y)   = (Number (x `op` y))

relational :: String -> (Integer -> Integer -> Bool) -> Binding
relational name op = binary name func2
    where func2 (Number x) (Number y) = (Boolean (x `op` y))

notFalse :: Sexpr -> Bool
notFalse (Boolean x) = not (x == False)

addVar :: Var -> BInt -> StateT Environment IO ()
addVar var int = do
  modify (\env -> env {
    vars = case vars env of
       (scope:rest) ->  ((compileVar var, VarInfo (compileInt int)) : scope) : rest;
       _            -> [[(compileVar var, VarInfo (compileInt int))]]
        })

getVar :: Sexpr -> StateT Environment IO VarInfo
getVar id = do
  env <- get
  return $ look (vars env) id
    where
      look [] id           = error $ "Variable Not Found."
      look (scope:rest) id = case lookup id scope of
        Nothing   -> look rest id
        Just info -> info

{--}
addFun :: BLine -> StateT Environment IO FunInfo
addFun line = do
    modify (\env -> env {
    funs = case funs env of
      (scope:rest) -> ((compileLine line, FunInfo (compileLine)) : scope) : rest;
      _            -> [[(id, FunInfo ())]]
    })
    return ()


compileInt :: BInt -> Sexpr
compileInt (Integer val) = Number val

compileString :: BStr -> Sexpr
compileString (Str str) = Symbol str

compileVar :: Var -> Sexpr
compileVar (Var v) = Symbol [v]
compileVar (Arr _) = Nil

compileFactor :: Factor -> StateT Environment IO VarInfo
compileFactor (VarFactor var) = (getVar (compileVar var))

{--}
compileTerm :: Term -> StateT Environment IO VarInfo
compileTerm (MultTerm fact1 fact2) = do
  factA <- compileFactor fact1
  factB <- compileFactor fact2
  --factA factB

compileExpression :: Expression -> StateT Environment IO VarInfo
--compileExpression (BareExpr (BareTerm (BasicIntFactor (Integer int)))) = Number int
compileExpression (BareExpr term) = (compileTerm term)
--compileExpression (MinusExpr term) = do
--compileExpression (PlusExpr ) =

compileLine :: BLine -> StateT Environment IO VarInfo
compileLine (Line (Integer _) (LET var expr)) = do
  varC <- compileVar var
  numC <- compileExpression expr
  addVar varC numC

compileLine (Line (Integer int) (PRINT expr)) = do
  let
    exprC   = compileExpression exprC
    newEnv' = appendProgram newEnv

  vr <- compileVar var
  vl <- compileExpression stmt
  nm <- show getBlockNum
  newenv <- incBlockNum env
  let name = "func" ++ [nm]
  return $ (Cons (Cons (Symbol name) (Symbol "lambda")) )


foo         = "foo_bas.txt"
gcd'        = "gcd_bas.txt"
pascal      = "pascal.txt"
sieve       = "sieve_bas.txt"
bubblesort  = "bubblesort_bas.txt"
fib         = "fib_bas.txt"
root        = "root_bas.txt"
guess       = "guess_bas.txt"
amazing     = "amazing_bas.txt"

main :: IO ()
main = do
--  input     <- readFile $ getArgs
  input     <- readFile foo
  parseTree <- (return . parseSRC . lines) input
  case parseTree of
    Right ast -> do
      print ast
    Left err -> print err
