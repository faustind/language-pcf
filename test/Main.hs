{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import System.IO.Unsafe
import Test.Hspec

import Parser
import Syntax

--import Eval
main :: IO ()
main = do
  hspec $
    describe "src/Parser.hs parses " $ do
      it "Var" $ parseExpr "x" `shouldBe` (Right $ Var "x")
      it "Nat" $ parseExpr "0" `shouldBe` (Right $ Nat 0)
      it "zero" $ parseExpr "zero" `shouldBe` (Right $ Nat 0)
      it "lambda single var" $
        parseExpr "\\x.x" `shouldBe` (Right $ Lam "x" (Var "x"))
      it "lambda multi var" $
        parseExpr "\\x y.add x y" `shouldBe`
        (Right $ Lam "x" (Lam "y" (App (App (Var "add") (Var "x")) (Var "y"))))
      it "primOp Pred" $
        parseExpr "pred 1" `shouldBe` (Right $ PrimOp Pred (Nat 1))
      it "primOp Succ" $
        parseExpr "succ 1" `shouldBe` (Right $ PrimOp Succ (Nat 1))
      it "primOp IsZero" $
        parseExpr "iszero 1" `shouldBe` (Right $ PrimOp IsZero (Nat 1))
      it "binOp" $
        parseExpr "1 + 3 + 4" `shouldBe`
        (Right $ Op Add (Op Add (Nat 1) (Nat 3)) (Nat 4))
      it "it-then-else" $
        parseExpr "if iszero 0 then 0 else 1" `shouldBe`
        (Right $ If (PrimOp IsZero (Nat 0)) (Nat 0) (Nat 1))
      it "app" $
        parseExpr "(\\x.x) 1" `shouldBe`
        (Right $ App (Lam "x" (Var "x")) (Nat 1))
      it "let" $
        parseExpr "let x = 3 in succ x" `shouldBe`
        (Right $ App (Lam "x" (PrimOp Succ (Var "x"))) (Nat 3))
      it "letrec" $
        parseExpr
          "let rec add x y = if iszero x then y else add (pred x) (succ y) in add 2 3" `shouldBe`
        (Right $
         App
           (Lam "add" (App (App (Var "add") (Nat 2)) (Nat 3)))
           (Mu
              "add"
              (Lam
                 "x"
                 (Lam
                    "y"
                    (If
                       (PrimOp IsZero (Var "x"))
                       (Var "y")
                       (App
                          (App (Var "add") (PrimOp Pred (Var "x")))
                          (PrimOp Succ (Var "y"))))))))
      it "mu" $
        parseExpr "mu add. \\x y. if iszero y then x else succ (add x (pred y))" `shouldBe`
        (Right $
         Mu
           "add"
           (Lam
              "x"
              (Lam
                 "y"
                 (If
                    (PrimOp IsZero (Var "y"))
                    (Var "x")
                    (PrimOp
                       Succ
                       (App (App (Var "add") (Var "x")) (PrimOp Pred (Var "y"))))))))
