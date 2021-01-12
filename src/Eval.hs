{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Control.Monad.Fail (MonadFail)
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

import Syntax

type ValCtx = Map.Map String Expr

type Eval a = Identity a

emptyValCtx = Map.empty

eval :: ValCtx -> Expr -> Eval Expr
eval env expr =
  case expr of
    Undefined -> error "The unexpected has happened!"
    (Nat i) -> return $ Nat i
    (Bool t) -> return $ Bool t
    (List l) -> return $ List l
    (Var v) -> return $ fromMaybe (Var v) (Map.lookup v env)
    (If cond tr fl) -> do
      let unwrap cond = do
            cond' <- eval env cond
            case cond' of
              Bool t -> pure t
              _ -> error "eval: type error: Bool"
      true <- unwrap cond
      if true
        then eval env tr
        else eval env fl
    (Lam x body) -> return $ Lam x body
    (Mu f body) -> return $ subst f (Mu f body) body
    (App fun arg) ->
      let unwrap list = do
            l <- eval env list
            case l of
              List es -> pure es
              _ -> error "eval: type error: List"
       in case fun of
            (App (Var "cons") e) -> do
              tail <- unwrap arg
              head <- eval env e
              return $ List ((:) head tail)
            (Var fname) ->
              if fname `elem` words "head tail null"
                then do
                  contents <- unwrap arg
                  case fname of
                    "head" -> eval env (head contents)
                    "tail" -> return $ List (tail contents)
                    "null" -> return $ Bool (null contents)
                else do
                  f <- eval env (Var fname)
                  case f of
                    (Var _) -> error ("unbound identifier: " ++ fname)
                    _ -> eval env $ App f arg
            _ ->
              let unwrap' fun = do
                    fun' <- eval env fun
                    case fun' of
                      Lam x body -> pure (x, body)
                      _ -> error "eval: type error: Abstraction"
               in do (bndr, body) <- unwrap' fun
                     eval env (subst bndr arg body)
    (PrimOp op e) -> do
      let unwrap nat = do
            n <- eval env nat
            case n of
              Nat i -> pure i
              _ -> error "eval: type error: Nat"
      i <- unwrap e
      case op of
        Succ -> return $ Nat $ i + 1
        Pred ->
          return $
          if i == 0
            then Nat 0
            else Nat $ i - 1
        IsZero -> return $ Bool $ i == 0
    (Op binop e1 e2) -> do
      let unwrap nat = do
            v <- eval env nat
            case v of
              Nat i -> pure i
              _ -> error "eval: type error: Nat"
      i <- unwrap e1
      j <- unwrap e2
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
subst v e (List l) = List $ map (subst v e) l
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

runEval :: ValCtx -> Binder -> Expr -> (Expr, ValCtx)
runEval env binder action =
  let res = runIdentity $ eval env action
   in (res, Map.insert binder res env)
