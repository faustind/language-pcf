module Syntax where

data Expr
  = Var Var
  | Nat Integer
  | Bool Bool
  | App Expr Expr
  | Lam Var Expr
  | Mu Id Expr
  | If Expr Expr Expr
  | Op BinOp Expr Expr
  | PrimOp PrimOp Expr
  | Undefined
  deriving (Show, Eq)

type Var = String

type Id = String

type Cond = Expr -- Evaluates to a Bool

data PrimOp
  = Succ
  | Pred
  | IsZero
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  deriving (Show, Eq)
