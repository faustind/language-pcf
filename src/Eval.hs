{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Control.Monad.Except
import Control.Monad.Fail (MonadFail)
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

import Syntax

type ValCtx = Map.Map String Expr

newtype Eval a =
  Eval
    { unEval :: ReaderT ValCtx (ExceptT String Identity) a
    }
  deriving (Functor, Applicative, Monad, MonadError String, MonadReader ValCtx)

emptyValCtx = Map.empty

eval :: Expr -> Eval Expr
eval expr =
  case expr of
    Undefined -> do
      throwError "The unexpected has happened!"
    (Nat i) -> return $ Nat i
    (Bool t) -> return $ Bool t
    (List l) -> return $ List l
    (Var v) -> do
      env <- ask
      return $ fromMaybe (Var v) (Map.lookup v env)
    (If cond tr fl) -> do
      let unwrap cond = do
            cond' <- eval cond
            case cond' of
              Bool t -> pure t
              _ -> do
                throwError "eval: type error in condition Bool"
      true <- unwrap cond
      if true
        then eval tr
        else eval fl
    (Lam x body) -> return $ Lam x body
    (Mu f body) -> return $ subst f (Mu f body) body
    (App fun arg) ->
      let unwrap list = do
            l <- eval list
            case l of
              List es -> pure es
              _ -> do
                throwError "eval: type error in condition Bool"
       in case fun of
            (App (Var "cons") e) -> do
              tail <- unwrap arg
              head <- eval e
              return $ List ((:) head tail)
            (Var fname) ->
              if fname `elem` words "head tail null"
                then do
                  contents <- unwrap arg
                  case fname of
                    "head" ->
                      if null contents
                        then do
                          throwError "eval: no head on empty list"
                        else eval (head contents)
                    "tail" ->
                      if null contents
                        then do
                          throwError "eval: no tail on empty list"
                        else return $ List (tail contents)
                    "null" -> return $ Bool (null contents)
                else do
                  f <- eval (Var fname)
                  case f of
                    (Var _) -> do
                      throwError "eval: unbound identifier"
                    _ -> eval $ App f arg
            _ ->
              let unwrap' fun = do
                    fun' <- eval fun
                    case fun' of
                      Lam x body -> pure (x, body)
                      _ -> do
                        throwError "eval: type error in application"
               in do (bndr, body) <- unwrap' fun
                     eval (subst bndr arg body)
    (PrimOp op e) -> do
      let unwrap nat = do
            n <- eval nat
            case n of
              Nat i -> pure i
              _ -> do
                throwError "eval: type error: expected Nat"
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
            v <- eval nat
            case v of
              Nat i -> pure i
              _ -> do
                throwError "eval: type error: expected Nat"
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
operator Mult a b = Nat $ a * b
operator Div a b =
  if b == 0
    then Undefined
    else Nat $ a `div` b

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

runEval :: ValCtx -> Binder -> Expr -> IO (Expr, ValCtx)
runEval env binder action =
  let res = runIdentity $ runExceptT $ runReaderT (unEval $ eval action) env
   in case res of
        Left err
        -- In case of error return the envrionment unmodified
        -- after printing the error to stdout
         -> do
          print err
          return (Undefined, env)
        Right value -> return (value, Map.insert binder value env)
