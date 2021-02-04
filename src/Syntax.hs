module Syntax where

data Expr
  = Var Var
  | Nat Integer
  | Bool Bool
  | List [Expr]
  | App Expr Expr
  | Lam Var Expr
  | Mu Id Expr
  | If Expr Expr Expr
  | Op BinOp Expr Expr
  | PrimOp PrimOp Expr
  | Undefined
  deriving (Show, Eq)

type Var = String

type Id = Var

type Binder = Var

type Cond = Expr -- Evaluates to a Bool

data PrimOp
  = Succ
  | Pred
  | IsZero
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | Mult
  | Div
  deriving (Show, Eq)

type Binding = (Binder, Expr)
