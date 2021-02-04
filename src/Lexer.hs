module Lexer where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Text.Lazy

import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax

type Op a = Ex.Operator L.Text () Identity a

type Operators a = Ex.OperatorTable L.Text () Identity a

reservedNames :: [String]
reservedNames =
  [ "if"
  , "then"
  , "else"
  , "mu"
  , "true"
  , "false"
  , "pred"
  , "succ"
  , "zero"
  , "iszero"
  , "let"
  , "rec"
  , "in"
  ]

reservedOps :: [String]
reservedOps = ["\\", ".", "=", "+", "-", "*", "/"]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer =
  Tok.makeTokenParser $
  Tok.LanguageDef
    { Tok.commentStart = "{-"
    , Tok.commentEnd = "-}"
    , Tok.commentLine = "--"
    , Tok.nestedComments = True
    , Tok.identStart = letter
    , Tok.identLetter = alphaNum <|> oneOf "_'"
    , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames = reservedNames
    , Tok.reservedOpNames = reservedOps
    , Tok.caseSensitive = True
    }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

semi :: Parser String
semi = Tok.semi lexer

comma :: Parser String
comma = Tok.comma lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
