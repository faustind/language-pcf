{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Syntax

import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader
import qualified Data.Map as Map

-- type ValCtx = Map.Map String PCFValue
type ValCtx = Map.Map String Expr

data PCFValue
  = VNat Integer
  | VBool Bool
  | VLam Var Expr -- | VMu Var Expr

type EList = Expr

newtype Eval a =
  Eval
    { unEval :: ReaderT ValCtx IO a
    }
  deriving (Monad, Functor, Applicative, MonadReader ValCtx, MonadIO, MonadFail)

eval :: Expr -> Eval Expr
eval (Nat i) = return $ Nat i
eval (Bool t) = return $ Bool t
eval (Var v) = return $ Var v
eval (If cond tr fl) = do
  Bool true <- eval cond
  if true
    then eval tr
    else eval fl
eval (Lam x body) = return $ Lam x body
eval (Mu f body) = return $ subst f (Mu f body) body
eval (App fun arg) = do
  Lam x body <- eval fun
  eval (subst x arg body)
eval (PrimOp op e) = do
  Nat i <- eval e
  case op of
    Succ -> return $ Nat $ i + 1
    Pred ->
      return $
      if i == 0
        then Nat 0
        else Nat $ i - 1
    IsZero -> return $ Bool $ i == 0
eval (Op binop e1 e2) = do
  Nat i <- eval e1
  Nat j <- eval e2
  return $ operator binop i j

operator :: BinOp -> Integer -> Integer -> Expr
operator Add a b = Nat $ a + b
operator Sub a b =
  Nat $
  let res = a - b
   in if res < 0
        then 0
        else res

subst :: Var -> Expr -> Expr -> Expr
subst v e (Nat i) = Nat i
subst v e (Bool t) = Bool t
subst v e (PrimOp op arg) = PrimOp op (subst v e arg)
subst v e (Op binop e1 e2) = Op binop (subst v e e1) (subst v e e2)
subst v e (If cond tr fl) = If (subst v e cond) (subst v e tr) (subst v e fl)
subst v e (App f arg) = App (subst v e f) (subst v e arg)
subst v e (Lam x body) =
  if v == x
    then Lam x body
    else Lam x (subst v e body)
subst v e (Mu f body) =
  if f == v
    then Mu f body
    else Mu f (subst v e body)
subst v e (Var x) =
  if v == x
    then e
    else Var x
subst v e Undefined = Undefined

runEval :: ValCtx -> Eval b -> IO b
runEval env action = runReaderT (unEval action) env
