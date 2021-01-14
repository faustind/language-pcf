module Parser where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

nat :: Parser Integer
nat = Tok.natural lexer

number :: Parser Expr
number = do
  n <- nat
  return (Nat n)

bool :: Parser Expr
bool =
  (reserved "true" >> return (Bool True)) <|>
  (reserved "false" >> return (Bool False))

list :: Parser Expr
list =
  do l <- brackets (expr `sepBy` comma)
     return $ List l
     <|> (reserved "nil" >> return (List []))

primOp :: Parser Expr
primOp =
  (reserved "succ" >> do
     x <- expr
     return (PrimOp Succ x)) <|>
  (reserved "pred" >> do
     x <- expr
     return (PrimOp Pred x)) <|>
  (reserved "iszero" >> do
     x <- expr
     return (PrimOp IsZero x)) <|>
  (reserved "zero" >> (return (Nat 0)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

mu :: Parser Expr
mu = do
  reserved "mu"
  f <- identifier
  reservedOp "."
  body <- expr
  return (Mu f body)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  xs <- many identifier
  reservedOp "="
  m <- expr
  reserved "in"
  n <- expr
  return $
    if null xs
      then App (Lam x n) m
      else App (Lam x n) (foldr Lam m xs)

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  xs <- many identifier
  reservedOp "="
  m <- expr
  reserved "in"
  n <- expr
  return $
    if null xs
      then App (Lam x n) (Mu x m)
      else App (Lam x n) (Mu x (foldr Lam m xs))

aexp :: Parser Expr
aexp =
  parens expr <|> list <|> bool <|> number <|> ifthen <|> mu <|> lambda <|>
  primOp <|>
  try letrecin <|>
  letin <|>
  variable

-- Parse an expression "e0"
-- then try parsing many "ei"s (i >= 1)
--     if no "ei" return "e0"
--     otherwise return
--          App (... App (App e0 e1) e3 ...)
term :: Parser Expr
term =
  aexp >>= \x -> (many1 aexp >>= \xs -> return (foldl App x xs)) <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table =
  [[infixOp "+" (Op Add) Ex.AssocLeft], [infixOp "-" (Op Sub) Ex.AssocLeft]]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return $
    if null args
      then (name, body)
      else (name, foldr Lam body args)

letrecdecl :: Parser Binding
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return $
    if null args
      then (name, Mu name body)
      else (name, Mu name (foldr Lam body args))

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try val <|> try letrecdecl <|> letdecl

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl :: Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

parseModule :: FilePath -> L.Text -> Either ParseError [Binding]
parseModule fname input = parse (contents modl) fname input
